(in-package #:qvm-app-ng-tests)

(alexandria:define-constant +generic-x-0-quil-program+
  "DECLARE ro BIT[2]; X 0; MEASURE 0 ro[0]"
  :test #'string=)

(alexandria:define-constant +all-ro-addresses+
    (alexandria:plist-hash-table '("ro" t) :test #'equal)
  :test #'equalp)

(alexandria:define-constant +empty-hash-table+
    (make-hash-table)
  :test #'equalp)

(alexandria:define-constant +allocation-method-strings+
    (mapcar #'string-downcase qvm-app-ng::+available-allocation-methods+)
  :test #'equal)

(alexandria:define-constant +simulation-method-strings+
    (mapcar #'string-downcase qvm-app-ng::+available-simulation-methods+)
  :test #'equal)

(global-vars:define-global-var +rpc-response-token-scanner+
    (cl-ppcre:create-scanner
     "\\A{\"token\":\"[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}\"}\\z"))

(global-vars:define-global-var +iso-time-scanner+
    (cl-ppcre:create-scanner
     "\\A\\d{4}-(?:1[0-2]|0[1-9])-(?:3[0-1]|[1-2][0-9]|0[0-9]) (?:2[0-3]|[0-1][0-9]):[0-5][0-9]:[0-5][0-9]\\z"))

(global-vars:define-global-var +rpc-response-version-scanner+
    (cl-ppcre:create-scanner "\\A([0-9]+\\.)+[0-9]+ \\[[0-9a-f]+\\]\\z"))

(defun plist-lowercase-keys (plist)
  (assert (evenp (length plist)))
  (loop :for (k v) :on plist :by #'cddr
        :collect (string-downcase k) :collect v))

(defun plist->json (plist)
  (with-output-to-string (*standard-output*)
    (yason:encode-plist (plist-lowercase-keys plist))))

(defun plist->hash-table (plist &key (test 'equal))
  "Like ALEXANDRIA:PLIST-HASH-TABLE but with TEST defaulting to EQUAL.

Also calls PLIST-LOWERCASE-KEYS to STRING-DOWNCASE the PLIST keys."
  (alexandria:plist-hash-table (plist-lowercase-keys plist) :test test))

(defun alist->hash-table (alist &key (test 'equal))
  "Like ALEXANDRIA:ALIST-HASH-TABLE but with TEST defaulting to EQUAL."
  (alexandria:alist-hash-table alist :test test))

(defun hash-table-fields-checker (fields)
  (lambda (json-data)
    (mapc (lambda (spec)
            (destructuring-bind (field value &key (test 'equal)) spec
              (if (functionp value)
                  (is (funcall value (gethash field json-data)))
                  (is (funcall (alexandria:ensure-function test) (gethash field json-data) value)))))
          fields)))

(defun response-json-fields-checker (fields)
  (lambda (response-string)
    (funcall (hash-table-fields-checker fields)
             (yason:parse response-string))))

(defun invalidate-token (qvm-token)
  ;; Replacing all 4's with 5's is guaranteed to produce an invalid v4 UUID, since it will overwrite
  ;; the version field.
  (substitute #\5 #\4 qvm-token))

(defun extract-token (response)
  (gethash "token" (yason:parse response)))

(defun extract-and-validate-token (response)
  (let ((token (extract-token response)))
    (is (qvm-app-ng::valid-uuid-string-p token))
    token))

(defun simulation-method->qvm-type (simulation-method &key pauli-noise-p)
  "Wrapper around QVM-APP-NG::SIMULATION-METHOD->QVM-TYPE.

Returns a STRING and allows SIMULATION-METHOD to be either a STRING or SYMBOL."
  (string (qvm-app-ng::simulation-method->qvm-type
           (if (stringp simulation-method)
               (qvm-app-ng::parse-simulation-method simulation-method)
               simulation-method)
           :pauli-noise-p pauli-noise-p)))

(defun %make-url (protocol host port &optional (path "/"))
  (format nil "~A://~A:~D~A" protocol host port path))

(defun call-with-rpc-server (host port debug function)
  (let (rpc-acceptor)
    (unwind-protect
         (progn
           (setf rpc-acceptor (qvm-app-ng::start-server host port debug))
           (funcall function (%make-url "http" host (tbnl:acceptor-port rpc-acceptor))))
      (qvm-app-ng::stop-server rpc-acceptor)
      (qvm-app-ng::reset-persistent-qvms-db)
      (qvm-app-ng::reset-jobs-db))))

(defmacro with-rpc-server ((url-var &key (host "127.0.0.1") (port 0) (debug nil)) &body body)
  "Execute BODY with URL-VAR bound the URL of a new RPC server started on HOST and PORT.

HOST defaults to 127.0.0.1 and PORT defaults to a randomly assigned port."
  (check-type url-var symbol)
  (alexandria:once-only (host port)
    `(call-with-rpc-server ,host ,port ,debug (lambda (,url-var) ,@body))))

(defun call-with-drakma-request (body-or-stream status-code headers uri stream must-close reason-phrase function)
  (unwind-protect
       (funcall function body-or-stream status-code headers uri stream must-close reason-phrase)
    (progn
      (when (streamp stream)
        (close stream))
      (when (streamp body-or-stream)
        (close body-or-stream)))))

(defmacro check-request (request-form
                         &key (status 200)
                              (response-re nil response-re-p)
                              (response-callback nil response-callback-p))
  "Evaluate a REQUEST-FORM that makes an HTTP request and check that the response status is STATUS and that the response body conforms to RESPONSE-RE and/or RESPONSE-CALLBACK.

REQUEST-FORM is expected to return the same VALUES as a DRAKMA:HTTP-REQUEST, namely (VALUES RESPONSE STATUS-CODE HEADERS URI STREAM MUST-CLOSE REASON-PHRASE)."
  (alexandria:once-only (status)
    (alexandria:with-gensyms
        (body-or-stream status-code headers uri stream must-close reason-phrase body-as-string)
      `(multiple-value-call #'call-with-drakma-request
         ,request-form
         (lambda (,body-or-stream ,status-code ,headers ,uri ,stream ,must-close ,reason-phrase)
           (declare (ignore ,headers ,uri ,stream ,must-close ,reason-phrase))
           (is (= ,status ,status-code))
           (let ((,body-as-string (if (streamp ,body-or-stream)
                                      (alexandria:read-stream-content-into-string ,body-or-stream)
                                      ,body-or-stream)))
             ,@(when response-re-p
                 `((is (cl-ppcre:scan ,response-re ,body-as-string))))
             ,@(when response-callback-p
                 `((funcall (alexandria:ensure-function ,response-callback) ,body-as-string)))

             ,body-as-string))))))

(defun http-request (&rest args)
  "Make an HTTP-REQUEST via DRAKMA:HTTP-REQUEST and treat application/json responses as text."
  (let ((drakma:*text-content-types* (append drakma:*text-content-types*
                                             '(("application" . "json")))))
    (apply #'drakma:http-request args)))

(defun simple-request (url &rest json-plist)
  "Make a POST request to the URL given. Any additional keyword args are collected in JSON-PLIST and converted to a JSON dict and sent as the request body."
  (http-request url :method ':POST :content (plist->json json-plist)))

(defun job-request (url &rest json-plist)
  "Create an async job for the request in JSON-PLIST, then await and return the result.

The CREATE-JOB request is expected to return with status 200 OK.

Ensure that the job is deleted afterwards."
  (let ((job-token (extract-and-validate-token
                    (check-request (simple-request url :type "create-job"
                                                       :sub-request (plist->hash-table json-plist))
                                   :status 202))))
    (unwind-protect
         (simple-request url :type "job-result" :job-token job-token)
      (simple-request url :type "delete-job" :job-token job-token))))

(defun request-qvm-state (url qvm-token)
  (gethash "state" (yason:parse (simple-request url :type "qvm-info" :qvm-token qvm-token))))

(defun request-job-status (url job-token)
  (gethash "status" (yason:parse (simple-request url :type "job-info" :job-token job-token))))

(defun wait-for (expected thunk
                 &key (repeat 5)
                      (initial-delay 0.001)
                      (backoff 2)
                      (test #'string=)
                      (final-assert-p t))
  "Wait for THUNK to return a result that is equal to EXPECTED according to TEST.

Repeat the call to THUNK REPEAT times. If the first call fails, then try again after INITIAL-DELAY seconds, then (* BACKOFF INITIAL-DELAY) and so on.

If FINAL-ASSERT-P is T, then assert that the TEST is satisfied before returning."
  (loop :repeat repeat
        :for result := (funcall thunk)
        :for delay := initial-delay :then (* backoff delay)
        :until (funcall test expected result)
        :do (sleep delay)
        :finally (when final-assert-p
                   (is (funcall test expected result)))))

(deftest test-rpc-api-invalid-request ()
  "Requests without a valid JSON request body return 400 Bad Request."
  (with-rpc-server (url)
    (dolist (content '("" "not-a-json-dict"))
      (check-request (http-request url :method ':POST :content content)
                     :status 400
                     :response-re "Bad Request"))))

(deftest test-rpc-error-response ()
  "Test that error responses have the expected JSON structure and content."
  (flet ((response-error-checker (message)
           (response-json-fields-checker
            `(("error_type" "qvm_error")
              ("status" ,message)))))
    (with-rpc-server (url)
      ;; error before handler dispatch
      (check-request (http-request url :method ':POST :content "")
                     :status 400
                     :response-callback (response-error-checker "QVM RPC Error: Bad Request
Failed to parse JSON object from request body: NIL"))

      ;; error during parameter parsing
      (check-request (simple-request url
                                     :type "qvm-info"
                                     ;; invalid token
                                     :qvm-token "5e2e05f0-f91c-5f02-96ef-361ffc55a0fa")
                     :status 400
                     :response-callback (response-error-checker "QVM RPC Error: Bad Request
Invalid persistent QVM token. Expected a v4 UUID. Got \"5e2e05f0-f91c-5f02-96ef-361ffc55a0fa\""))

      ;; error during handler execution
      (check-request (simple-request url
                                     :type "qvm-info"
                                     ;; syntactically valid but non-existent token
                                     :qvm-token "5e2e05f0-f91c-4f02-96ef-361ffc55a0fa")
                     :status 500
                     :response-callback (response-error-checker "QVM RPC Error: Internal Server Error
Failed to find persistent QVM #1=5e2e05f0-f91c-4f02-96ef-361ffc55a0fa
Failed to find key #1# in SAFETY-HASH")))))

(deftest test-rpc-api-404 ()
  "Requests for URIs other than \"/\" or for non-existent RPC methods return 404 Not Found."
  (with-rpc-server (url)
    (check-request (simple-request url)
                   :status 404)
    (check-request (simple-request url :type "some-non-existent-method")
                   :status 404)))

(deftest test-rpc-api-version ()
  "Test the \"version\" API call."
  (with-rpc-server (url)
    (check-request (simple-request url :type "version")
                   :response-re +rpc-response-version-scanner+)))

(deftest test-rpc-api-run-program-simple-request ()
  "Simple run-program calls on emphemeral QVMs return the expected results."
  (with-rpc-server (url)
    (dolist (allocation-method +allocation-method-strings+)
      (dolist (simulation-method +simulation-method-strings+)
        (check-request (simple-request url
                                       :type "run-program"
                                       :allocation-method allocation-method
                                       :simulation-method simulation-method
                                       :compiled-quil +generic-x-0-quil-program+
                                       :addresses +all-ro-addresses+)
                       :response-callback (response-json-fields-checker '(("ro" ((1 0))))))))))

(deftest test-rpc-api-run-program-with-pauli-noise ()
  "Test run-program calls on emphemeral QVMs with Pauli channel noise."
  (with-rpc-server (url)
    ;; gate and measurement all zeros
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +all-ro-addresses+
                                   :gate-noise '(0.0 0.0 0.0)
                                   :measurement-noise '(0.0 0.0 0.0))
                   :response-callback (response-json-fields-checker '(("ro" ((1 0))))))

    ;; gate only, P(X) = 1.0 flips the bit in the result
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +all-ro-addresses+
                                   :gate-noise '(1.0 0.0 0.0))
                   :response-callback (response-json-fields-checker '(("ro" ((0 0))))))

    ;; measurement only, P(Y) = 1.0 flips the bit in the result
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +all-ro-addresses+
                                   :gate-noise '(0.0 0.0 0.0)
                                   :measurement-noise '(0.0 1.0 0.0))
                   :response-callback (response-json-fields-checker '(("ro" ((0 0))))))

    ;; gate and measurement errors cancel out
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +all-ro-addresses+
                                   :gate-noise '(1.0 0.0 0.0)
                                   :measurement-noise '(0.0 1.0 0.0))
                   :response-callback (response-json-fields-checker '(("ro" ((1 0))))))

    ;; Pauli noise is incompatible with the full-density-matrix simulation-method
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "full-density-matrix"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +all-ro-addresses+
                                   :gate-noise '(0.0 0.0 0.0))
                   :status 500)))

(deftest test-rpc-api-run-program-invalid-requests ()
  "Test input validation for the run-program call."
  (with-rpc-server (url)
    ;; specify both qvm-token and simulation-method
    (check-request (simple-request url
                                   :type "run-program"
                                   :qvm-token (qvm-app-ng::make-persistent-qvm-token)
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; specify both qvm-token and allocation-method
    (check-request (simple-request url
                                   :type "run-program"
                                   :qvm-token (qvm-app-ng::make-persistent-qvm-token)
                                   :allocation-method "native"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; specify both qvm-token and gate-noise
    (check-request (simple-request url
                                   :type "run-program"
                                   :qvm-token (qvm-app-ng::make-persistent-qvm-token)
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :gate-noise '(0.0 0.0 0.0))
                   :status 400)

    ;; specify both qvm-token and measurement-noise
    (check-request (simple-request url
                                   :type "run-program"
                                   :qvm-token (qvm-app-ng::make-persistent-qvm-token)
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :measurement-noise '(0.0 0.0 0.0))
                   :status 400)

    ;; specify simulation-method without allocation-method
    (check-request (simple-request url
                                   :type "run-program"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; specify allocation-method without simulation-method
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; specify neither qvm-token nor simulation-method nor allocation-method
    (check-request (simple-request url
                                   :type "run-program"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; invalid qvm-token
    (check-request (simple-request url
                                   :type "run-program"
                                   :qvm-token "123345"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; invalid simulation-method
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "super-fast"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; invalid allocation-method
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "super-fast"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; invalid compiled-quil
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil "(defgate FOO (:as :permutation) #(0 2 3 1))"
                                   :addresses +empty-hash-table+)
                   :status 400)

    ;; NB: the :ADDRESSES parameter is complicated enough that it gets a separate test in
    ;; TEST-RPC-API-RUN-PROGRAM-ADDRESSES.

    ;; invalid gate-noise
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :gate-noise 0.0)
                   :status 400)

    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :gate-noise '(0.0 0.0))
                   :status 400)

    ;; invalid measurement-noise
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :measurement-noise 0.0)
                   :status 400)

    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :measurement-noise '(0.0 0.0))
                   :status 400)))

(deftest test-rpc-api-run-program-addresses ()
  "Test variations of the ADDRESSES parameter for the run-program call."
  (with-rpc-server (url)
    ;; empty addresses
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil +generic-x-0-quil-program+
                     :addresses +empty-hash-table+)
     :response-re "{}")

    ;; addresses t
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil +generic-x-0-quil-program+
                     :addresses +all-ro-addresses+)
     :response-callback (response-json-fields-checker '(("ro" ((1 0))))))

    ;; explicit index list
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil +generic-x-0-quil-program+
                     :addresses (plist->hash-table '("ro" (0))))
     :response-callback (response-json-fields-checker '(("ro" ((1))))))

    ;; non-consecutive indices + "non ro" named register
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil "DECLARE mem BIT[3]; X 3; MEASURE 0 mem[0]; MEASURE 3 mem[2]"
                     :addresses (plist->hash-table '("mem" (0 2))))
     :response-callback (response-json-fields-checker '(("mem" ((0 1))))))

    ;; invalid register index OOB
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil +generic-x-0-quil-program+
                     :addresses (plist->hash-table '("ro" (0 2))))
     :status 500)

    ;; invalid negative register index
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil +generic-x-0-quil-program+
                     :addresses (plist->hash-table '("ro" (-1))))
     :status 400)

    ;; non-existent named register
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil "DECLARE ro BIT; I 0"
                     :addresses (plist->hash-table '("zonk" t)))
     :status 500)))

(deftest test-rpc-api-create-qvm ()
  "Test create-qvm for various combinations of SIMULATION-METHOD and NUM-QUBITS."
  (with-rpc-server (url)
    (dolist (allocation-method +allocation-method-strings+)
      (dolist (simulation-method +simulation-method-strings+)
        (dolist (num-qubits '(0 1 4))
          (let* ((response (check-request (simple-request url
                                                          :type "create-qvm"
                                                          :allocation-method allocation-method
                                                          :simulation-method simulation-method
                                                          :num-qubits num-qubits)
                                          :status 201
                                          :response-re +rpc-response-token-scanner+))
                 (token (extract-and-validate-token response)))

            ;; check that info reports expected values for qvm-type and num-qubits
            (check-request (simple-request url :type "qvm-info" :qvm-token token)
                           :response-callback
                           (response-json-fields-checker
                            `(("qvm-type" ,(simulation-method->qvm-type simulation-method))
                              ("num-qubits" ,num-qubits)
                              ("state" "READY")
                              ("metadata" ,(hash-table-fields-checker
                                            `(("allocation-method" ,(string-upcase allocation-method))
                                              ("created" ,(lambda (s)
                                                            (cl-ppcre:scan +iso-time-scanner+ s)))))))))

            ;; cleanup
            (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                           :response-re "Deleted persistent QVM")))))))

(deftest test-rpc-api-qvm-memory-estimate ()
  "Test the qvm-memory-estimate method."
  (with-rpc-server (url)
    (dolist (allocation-method +allocation-method-strings+)
      (dolist (simulation-method +simulation-method-strings+)
        (dolist (num-qubits '(0 1 16))
          ;; EXPECTED-BYTES is duplicating the calculation in QVM-APP-NG::OCTETS-REQUIRED-FOR-QVM.
          ;; We could just call that function here, but then we'd only be testing the HTTP
          ;; interface, not the calculation. By duplicating the calculation here, this test will
          ;; still catch any bugs that creep in to QVM-APP-NG::OCTETS-REQUIRED-FOR-QVM, at the
          ;; expense of needing to be separately kept in sync as support for more SIMULATION-METHODs
          ;; and noise models are added.
          (let ((expected-bytes
                  (* qvm::+octets-per-cflonum+
                     (expt 2 (cond ((string= "pure-state" simulation-method) num-qubits)
                                   ((string= "full-density-matrix" simulation-method) (1+ (* 2 num-qubits)))
                                   ;; Eventually, this will need to be updated to deal
                                   ;; with other QVM variants.
                                   (t (error "Don't know how to estimate memory usage for SIMULATION-METHOD ~A"
                                             simulation-method)))))))
            (check-request (simple-request url
                                           :type "qvm-memory-estimate"
                                           :allocation-method allocation-method
                                           :simulation-method simulation-method
                                           :num-qubits num-qubits)
                           :response-callback
                           (response-json-fields-checker `(("bytes" ,expected-bytes))))))))))

(deftest test-rpc-api-wait-resume ()
  "Test wait and resume on a persistent QVM."
  (with-rpc-server (url)
    (let* ((qvm-token (extract-and-validate-token
                       (check-request (simple-request url
                                                      :type "create-qvm"
                                                      :allocation-method "native"
                                                      :simulation-method "pure-state"
                                                      :num-qubits 2)
                                      :status 201
                                      :response-re +rpc-response-token-scanner+)))
           (job-token (extract-and-validate-token
                       (check-request (simple-request url
                                                      :type "create-job"
                                                      :sub-request
                                                      (plist->hash-table
                                                       `(:type "run-program"
                                                         :qvm-token ,qvm-token
                                                         :compiled-quil "DECLARE ro BIT; DECLARE alpha REAL; MOVE alpha 0.0; WAIT; RX(alpha) 0; MEASURE 0 ro"
                                                         :addresses ,+all-ro-addresses+)))
                                      :status 202))))

      ;; Wait for the persistent qvm to enter the WAITING state
      (wait-for "WAITING" (lambda () (request-qvm-state url qvm-token)))

      ;; the associated JOB should now be RUNNING
      (wait-for "RUNNING" (lambda () (request-job-status url job-token)))

      ;; run-program on a persistent qvm in a non-READY state is disallowed
      (check-request (simple-request url
                                     :type "run-program"
                                     :qvm-token qvm-token
                                     :compiled-quil +generic-x-0-quil-program+
                                     :addresses +all-ro-addresses+)
                     :status 500)

      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token qvm-token
                                     :memory-contents (alist->hash-table `(("alpha" . ((0 ,pi))))))
                     :status 200)

      (check-request (simple-request url :type "resume" :qvm-token qvm-token)
                     :status 200)

      (check-request (simple-request url :type "qvm-info" :qvm-token qvm-token)
                     :response-callback
                     (response-json-fields-checker
                      `(("state" ,(lambda (value)
                                     (member value '("RESUMING" "RUNNING" "READY") :test #'string=))))))

      ;; Wait for the persistent qvm to enter the READY state
      (wait-for "READY" (lambda () (request-qvm-state url qvm-token)))

      ;; Wait for the job to finish
      (wait-for "FINISHED" (lambda () (request-job-status url job-token)))

      (check-request (simple-request url
                                     :type "read-memory"
                                     :qvm-token qvm-token
                                     :addresses +all-ro-addresses+)
                     :response-callback (response-json-fields-checker '(("ro" ((1))))))

      ;; job-result returns the same data as read-memory
      (check-request (simple-request url :type "job-result" :job-token job-token)
                     :response-callback (response-json-fields-checker '(("ro" ((1))))))

      ;; cleanup
      (check-request (simple-request url :type "delete-qvm" :qvm-token qvm-token)
                     :response-re "Deleted persistent QVM")
      (check-request (simple-request url :type "delete-job" :job-token job-token)
                     :response-re "Deleted async JOB"))))

(deftest test-rpc-api-wait-resume-async-full-monty ()
  "Test wait and resume on a persistent QVM with all calls being async jobs."
  (with-rpc-server (url)
    (let* ((qvm-token (extract-and-validate-token
                       (check-request (job-request url
                                                   :type "create-qvm"
                                                   :allocation-method "native"
                                                   :simulation-method "pure-state"
                                                   :num-qubits 2)
                                      :status 201
                                      :response-re +rpc-response-token-scanner+)))
           (job-token (extract-and-validate-token
                       (check-request (simple-request url
                                                      :type "create-job"
                                                      :sub-request
                                                      (plist->hash-table
                                                       `(:type "run-program"
                                                         :qvm-token ,qvm-token
                                                         :compiled-quil "DECLARE ro BIT; DECLARE alpha REAL; MOVE alpha 0.0; WAIT; RX(alpha) 0; MEASURE 0 ro"
                                                         :addresses ,+all-ro-addresses+)))
                                      :status 202))))

      (check-request (job-request url :type "qvm-info" :qvm-token qvm-token)
                     :response-callback (response-json-fields-checker '(("state" "WAITING"))))

      (check-request (job-request url :type "job-info" :job-token job-token)
                     :response-callback (response-json-fields-checker '(("status" "RUNNING"))))

      ;; run-program on a persistent qvm in a non-READY state is disallowed
      (check-request (job-request url
                                  :type "run-program"
                                  :qvm-token qvm-token
                                  :compiled-quil +generic-x-0-quil-program+
                                  :addresses +all-ro-addresses+)
                     :status 500)

      (check-request (job-request url
                                  :type "write-memory"
                                  :qvm-token qvm-token
                                  :memory-contents (alist->hash-table `(("alpha" . ((0 ,pi))))))
                     :status 200)

      (check-request (job-request url :type "resume" :qvm-token qvm-token)
                     :status 200)

      (check-request (job-request url :type "qvm-info" :qvm-token qvm-token)
                     :response-callback
                     (response-json-fields-checker
                      `(("state" ,(lambda (value)
                                    (member value '("RESUMING" "RUNNING" "READY") :test #'string=))))))

      ;; Wait for the persistent qvm to enter the READY state
      (wait-for "READY" (lambda () (request-qvm-state url qvm-token)))

      ;; Wait for the job to finish
      (wait-for "FINISHED" (lambda () (request-job-status url job-token)))

      (check-request (job-request url
                                  :type "read-memory"
                                  :qvm-token qvm-token
                                  :addresses +all-ro-addresses+)
                     :response-callback (response-json-fields-checker '(("ro" ((1))))))

      ;; job-result returns the same data as read-memory
      (check-request (job-request url :type "job-result" :job-token job-token)
                     :response-callback (response-json-fields-checker '(("ro" ((1))))))

      ;; cleanup
      (check-request (job-request url :type "delete-qvm" :qvm-token qvm-token)
                     :response-re "Deleted persistent QVM")
      (check-request (job-request url :type "delete-job" :job-token job-token)
                     :response-re "Deleted async JOB"))))

(deftest test-rpc-api-resume-invalid-requests ()
  "Test invalid resume API calls."
  (with-rpc-server (url)
    ;; non-existent persistent qvm
    (check-request (job-request url :type "resume"
                                    :qvm-token (qvm-app-ng::make-persistent-qvm-token))
                   :status 500)
    (let ((qvm-token (extract-and-validate-token
                      (check-request (simple-request url
                                                     :type "create-qvm"
                                                     :allocation-method "native"
                                                     :simulation-method "pure-state"
                                                     :num-qubits 2)
                                     :status 201
                                     :response-re +rpc-response-token-scanner+))))

      ;; cannot resume pqvm in non-waiting state
      (is (string= "READY" (request-qvm-state url qvm-token)))
      (check-request (job-request url :type "resume" :qvm-token qvm-token)
                     :status 500))))

(deftest test-rpc-api-read-memory ()
  "Test the read-memory API call."
  (with-rpc-server (url)
    (let ((qvm-token (extract-and-validate-token
                      (check-request (job-request url
                                                  :type "create-qvm"
                                                  :allocation-method "native"
                                                  :simulation-method "pure-state"
                                                  :num-qubits 2)
                                     :status 201
                                     :response-re +rpc-response-token-scanner+))))

      ;; request nothing get nothing
      (check-request (simple-request url :type "read-memory"
                                         :qvm-token qvm-token
                                         :addresses +empty-hash-table+)
                     :response-re "{}")

      ;; error, no classical memory subsystem configured yet
      (check-request (simple-request url :type "read-memory"
                                         :qvm-token qvm-token
                                         :addresses +all-ro-addresses+)
                     :status 500)

      ;; DECLARE some memory
      (check-request (simple-request url
                                     :type "run-program"
                                     :qvm-token qvm-token
                                     :compiled-quil "DECLARE ro BIT[2]; DECLARE theta REAL[2]; MOVE ro[1] 1; MOVE theta[1] 2.0"
                                     :addresses +empty-hash-table+)
                     :response-re "{}")

      ;; Wait for the persistent qvm to enter the READY state
      (wait-for "READY" (lambda () (request-qvm-state url qvm-token)))

      ;; registers can now be read
      (check-request (simple-request url
                                     :type "read-memory"
                                     :qvm-token qvm-token
                                     :addresses +all-ro-addresses+)
                     :response-callback (response-json-fields-checker '(("ro" ((0 1))))))

      (check-request (simple-request url
                                     :type "read-memory"
                                     :qvm-token qvm-token
                                     :addresses (plist->hash-table '("theta" t)))
                     :response-callback (response-json-fields-checker '(("theta" ((0.0 2.0))))))

      ;; or even a single location
      (check-request (simple-request url :type "read-memory"
                                         :qvm-token qvm-token
                                         :addresses (plist->hash-table '("ro" (1))))
                     :response-callback (response-json-fields-checker '(("ro" ((1))))))

      (check-request (simple-request url
                                     :type "read-memory"
                                     :qvm-token qvm-token
                                     :addresses (plist->hash-table '("theta" (1))))
                     :response-callback (response-json-fields-checker '(("theta" ((2.0))))))

      ;; but ro[2] is out-of-bounds
      (check-request (simple-request url :type "read-memory"
                                         :qvm-token qvm-token
                                         :addresses (plist->hash-table '("ro" (2))))
                     :status 500)

      ;; and register "zonk" does not exist
      (check-request (simple-request url :type "read-memory"
                                         :qvm-token qvm-token
                                         :addresses (plist->hash-table '("zonk" t)))
                     :status 500)

      ;; invalid qvm-token
      (check-request (simple-request url :type "read-memory"
                                         :qvm-token (invalidate-token qvm-token)
                                         :addresses +all-ro-addresses+)
                     :status 400)

      ;; invalid address parameter
      (check-request (simple-request url :type "read-memory"
                                         :qvm-token qvm-token
                                         :addresses "all of them")
                     :status 400)

      ;; clean up
      (check-request (simple-request url :type "delete-qvm" :qvm-token qvm-token)
                     :response-re "Deleted persistent QVM"))))

(deftest test-rpc-api-write-memory ()
  "Test the write-memory API call."
  (with-rpc-server (url)
    (let ((qvm-token (extract-and-validate-token
                      (check-request (job-request url
                                                  :type "create-qvm"
                                                  :allocation-method "native"
                                                  :simulation-method "pure-state"
                                                  :num-qubits 2)
                                     :status 201
                                     :response-re +rpc-response-token-scanner+))))

      ;; error, no classical memory subsystem configured yet
      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token qvm-token
                                     :memory-contents (alist->hash-table '(("ro" . ((0 1))))))
                     :status 500)

      ;; DECLARE some memory
      (check-request (simple-request url
                                     :type "run-program"
                                     :qvm-token qvm-token
                                     :compiled-quil "DECLARE ro BIT[2]; DECLARE theta REAL[2]"
                                     :addresses +empty-hash-table+)
                     :response-re "{}")

      ;; Wait for the persistent qvm to enter the READY state
      (wait-for "READY" (lambda () (request-qvm-state url qvm-token)))

      ;; registers can now be written to and read from
      (loop :for (write-memory expected)
              :in '(((("ro" . ((0 1)))) ; r[0] = 1
                     (("ro" ((1 0)))
                      ("theta" ((0.0 0.0)))))
                    ((("ro" . ((1 1))) ; ro[1] = 1
                      ("theta" . ((0 1.1) (1 2.2)))) ; theta[0] = 1.1; theta[1] = 2.2
                     (("ro" ((1 1)))
                      ("theta" ((1.1 2.2))))))
            :do (check-request (simple-request url
                                               :type "write-memory"
                                               :qvm-token qvm-token
                                               :memory-contents (alist->hash-table write-memory))
                               :status 200)
                (check-request (simple-request url
                                               :type "read-memory"
                                               :qvm-token qvm-token
                                               :addresses (alist->hash-table '(("ro" . t)
                                                                               ("theta" . t))))
                               :response-callback (response-json-fields-checker expected)))

      ;; 1.0 is not a valid BIT
      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token qvm-token
                                     :memory-contents (alist->hash-table '(("ro" . ((0 1.0))))))
                     :status 500)

      ;; 2 is not a valid BIT
      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token qvm-token
                                     :memory-contents (alist->hash-table '(("ro" . ((0 2))))))
                     :status 500)

      ;; 1 is not a valid REAL
      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token qvm-token
                                     :memory-contents (alist->hash-table '(("theta" . ((0 1))))))
                     :status 500)

      ;; ro[2] is out-of-bounds
      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token qvm-token
                                     :memory-contents (alist->hash-table '(("ro" . ((2 1))))))
                     :status 500)

      ;; and register "zonk" does not exist
      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token qvm-token
                                     :memory-contents (alist->hash-table '(("zonk" . ((0 1))))))
                     :status 500)

      ;; invalid qvm-token
      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token (invalidate-token qvm-token)
                                     :memory-contents (alist->hash-table '(("ro" . ((0 1))))))
                     :status 400)

      ;; invalid memory-contents parameters
      (dolist (invalid '((("ro" . ((0))))
                         (("ro" . ((0 1 2))))
                         (("ro" . ((0.0 1))))
                         (("ro" . ((-1 1))))
                         ((0 1))))
        (check-request (simple-request url
                                       :type "write-memory"
                                       :qvm-token qvm-token
                                       :memory-contents (alist->hash-table invalid))
                       :status 400))

      (check-request (simple-request url
                                     :type "write-memory"
                                     :qvm-token qvm-token
                                     :memory-contents '(0 1))
                     :status 400)

      ;; clean up
      (check-request (simple-request url :type "delete-qvm" :qvm-token qvm-token)
                     :response-re "Deleted persistent QVM"))))

(deftest test-rpc-api-create-qvm-with-pauli-noise ()
  "Test create-qvm for various combinations of SIMULATION-METHOD and Pauli noise parameters.."
  (with-rpc-server (url)
    ;; Pauli noise is incompatible with the full-density-matrix simulation-method
    (check-request (simple-request url
                                   :type "create-qvm"
                                   :allocation-method "native"
                                   :simulation-method "full-density-matrix"
                                   :num-qubits 1
                                   :gate-noise '(1.0 0.0 0.0)
                                   :measurement-noise '(0.0 0.0 0.0))
                   :status 500)

    (dolist (gate-noise '(nil (1.0 0.0 0.0)))
      (dolist (measurement-noise '(nil (0.0 1.0 0.0)))
        (let* ((response (check-request (simple-request url
                                                        :type "create-qvm"
                                                        :allocation-method "native"
                                                        :simulation-method "pure-state"
                                                        :num-qubits 1
                                                        :gate-noise gate-noise
                                                        :measurement-noise measurement-noise)
                                        :status 201
                                        :response-re +rpc-response-token-scanner+))
               (token (extract-and-validate-token response)))

          ;; check that info reports expected values for qvm-type
          (check-request (simple-request url :type "qvm-info" :qvm-token token)
                         :response-callback
                         (response-json-fields-checker
                          `(("qvm-type" ,(simulation-method->qvm-type
                                          "pure-state"
                                          :pauli-noise-p (or gate-noise measurement-noise))))))

          ;; cleanup
          (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                         :response-re "Deleted persistent QVM"))))))

(deftest test-rpc-api-create-qvm-invalid-requests ()
  "Test input validation for the create-qvm call."
  (with-rpc-server (url)
    ;; invalid simulation-method
    (check-request (simple-request url
                                   :type "create-qvm"
                                   :allocation-method "native"
                                   :simulation-method "super-fast"
                                   :num-qubits 0)
                   :status 400)

    ;; no simulation method
    (check-request (simple-request url :type "create-qvm" :allocation-method "native" :num-qubits 0)
                   :status 400)

    ;; invalid simulation method
    (check-request (simple-request url
                                   :type "create-qvm"
                                   :simulation-method "super-fast"
                                   :allocation-method "native"
                                   :num-qubits 0)
                   :status 400)

    ;; no allocation method
    (check-request (simple-request url :type "create-qvm"
                                       :simulation-method "pure-state"
                                       :num-qubits 0)
                   :status 400)

    ;; invalid allocation method
    (check-request (simple-request url
                                   :type "create-qvm"
                                   :simulation-method "pure-state"
                                   :allocation-method "super-fast"
                                   :num-qubits 0)
                   :status 400)

    ;; invalid num-qubits
    (check-request (simple-request url
                                   :type "create-qvm"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :num-qubits -1)
                   :status 400)

    ;; no num-qubits
    (check-request (simple-request url :type "create-qvm"
                                       :allocation-method "native"
                                       :simulation-method "pure-state")
                   :status 400)

    ;; invalid gate-noise
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :gate-noise 0.0)
                   :status 400)

    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :gate-noise '(0.0 0.0))
                   :status 400)

    ;; invalid measurement-noise
    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :measurement-noise 0.0)
                   :status 400)

    (check-request (simple-request url
                                   :type "run-program"
                                   :allocation-method "native"
                                   :simulation-method "pure-state"
                                   :compiled-quil +generic-x-0-quil-program+
                                   :addresses +empty-hash-table+
                                   :measurement-noise '(0.0 0.0))
                   :status 400)))

(deftest test-rpc-api-qvm-info ()
  "Test that qvm-info returns the expected results."
  (with-rpc-server (url)
    ;; info on non-existing token
    (check-request (simple-request url
                                   :type "qvm-info"
                                   :qvm-token (qvm-app-ng::make-persistent-qvm-token))
                   :status 500
                   :response-re "Failed to find persistent QVM")

    ;; delete on non-existing token
    (check-request (simple-request url
                                   :type "delete-qvm"
                                   :qvm-token (qvm-app-ng::make-persistent-qvm-token))
                   :status 500
                   :response-re "Failed to find persistent QVM")

    (let* ((response (check-request (simple-request url
                                                    :type "create-qvm"
                                                    :allocation-method "native"
                                                    :simulation-method "pure-state"
                                                    :num-qubits 1)
                                    :status 201
                                    :response-re +rpc-response-token-scanner+))
           (token (extract-and-validate-token response)))

      ;; info on invalid token
      (check-request (simple-request url :type "qvm-info" :qvm-token (invalidate-token token))
                     :status 400
                     :response-re "Invalid persistent QVM token.")

      ;; info on existing token
      (check-request (simple-request url :type "qvm-info" :qvm-token token)
                     :response-callback
                     (response-json-fields-checker
                      `(("qvm-type" "PURE-STATE-QVM")
                        ("num-qubits" 1)
                        ("state" "READY")
                        ("metadata" ,(hash-table-fields-checker
                                      `(("allocation-method" "NATIVE")
                                        ("created" ,(lambda (s)
                                                      (cl-ppcre:scan +iso-time-scanner+ s)))))))))

      ;; upper case token also accepted
      (check-request (simple-request url :type "qvm-info" :qvm-token (string-upcase token))



                     :response-re "PURE-STATE-QVM")

      ;; delete on existing token
      (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                     :response-re "Deleted persistent QVM")

      ;; info on deleted token
      (check-request (simple-request url :type "qvm-info" :qvm-token token)
                     :status 500
                     :response-re "Failed to find persistent QVM"))))

(deftest test-rpc-api-persistent-qvm-run-program ()
  "Test run-program calls on a persistent QVM."
  (with-rpc-server (url)
    (dolist (allocation-method +allocation-method-strings+)
      (dolist (simulation-method +simulation-method-strings+)
        ;; run-program on non-existent token
       (check-request (simple-request url
                                      :type "run-program"
                                      :qvm-token (qvm-app-ng::make-persistent-qvm-token)
                                      :compiled-quil +generic-x-0-quil-program+
                                      :addresses +empty-hash-table+)
                      :status 500
                      :response-re "Failed to find persistent QVM")

       (let* ((response (check-request (simple-request url
                                                       :type "create-qvm"
                                                       :allocation-method allocation-method
                                                       :simulation-method simulation-method
                                                       :num-qubits 2)
                                       :status 201
                                       :response-re +rpc-response-token-scanner+))
              (token (extract-and-validate-token response)))

         ;; run-program on existing token
         (check-request (simple-request url
                                        :type "run-program"
                                        :qvm-token token
                                        :compiled-quil +generic-x-0-quil-program+
                                        :addresses +all-ro-addresses+)
                        :response-callback (response-json-fields-checker '(("ro" ((1 0))))))

         ;; I 0: qubit 0 remains in excited state
         (check-request (simple-request url
                                        :type "run-program"
                                        :qvm-token token
                                        :compiled-quil "DECLARE ro BIT[2]; I 0; MEASURE 0 ro[0]"
                                        :addresses +all-ro-addresses+)
                        :response-callback (response-json-fields-checker '(("ro" ((1 0))))))

         ;; X 0: flips qubit 0 back to ground state
         (check-request (simple-request url
                                        :type "run-program"
                                        :qvm-token token
                                        :compiled-quil +generic-x-0-quil-program+
                                        :addresses +all-ro-addresses+)
                        :response-callback (response-json-fields-checker '(("ro" ((0 0))))))

         ;; cleanup
         (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                        :response-re "Deleted persistent QVM"))))))

(deftest test-rpc-api-create-job ()
  (with-rpc-server (url)
    ;; version: check handler that returns plain text
    (check-request (job-request url :type "version")
                   :response-re +rpc-response-version-scanner+)

    ;; qvm-memory-estimate: check handler that returns JSON
    (check-request (job-request url :type "qvm-memory-estimate"
                                    :simulation-method "pure-state"
                                    :allocation-method "native"
                                    :num-qubits 2)
                   :response-callback (response-json-fields-checker
                                       `(("bytes" ,(lambda (bytes) (plusp bytes))))))
    ;; create-job: check that attempting a nested create-job returns 400 Bad Request
    (check-request (simple-request url :type "create-job"
                                       :sub-request (plist->hash-table
                                                     `(:type "create-job"
                                                       :sub-request ,(plist->hash-table
                                                                      '(:type "version")))))
                   :status 400)

    ;; qvm-info: check that errors are propagated for invalid requests
    (let ((job-token (extract-and-validate-token
                      (check-request
                       (simple-request url
                                       :type "create-job"
                                       :sub-request (plist->hash-table '(:type "qvm-info"
                                                                         :qvm-token "bogus")))
                       :status 202))))

      (check-request (simple-request url :type "job-info" :job-token job-token)
                     :response-callback
                     (response-json-fields-checker
                      `(("status" ,(lambda (value)
                                     (member value '("RUNNING" "ERROR") :test #'string=))))))

      (check-request (simple-request url :type "job-result" :job-token job-token)
                     :status 400)

      (is (string= "ERROR" (request-job-status url job-token)))

      ;; cleanup
      (check-request (simple-request url :type "delete-job" :job-token job-token)
                     :response-re "Deleted async JOB"))

    ;; job-info and job-result: slightly wacky, but check that these can be called asynchronously
    (let ((job-token (extract-and-validate-token
                      (check-request
                       (simple-request url
                                       :type "create-job"
                                       :sub-request (plist->hash-table '(:type "version")))
                       :status 202))))

      (check-request (job-request url :type "job-info" :job-token job-token)
                     :response-callback
                     (response-json-fields-checker
                      `(("status" ,(lambda (value)
                                     (member value '("RUNNING" "FINISHED") :test #'string=))))))

      (check-request (job-request url :type "job-result" :job-token job-token)
                     :response-re +rpc-response-version-scanner+)

      ;; cleanup
      (check-request (simple-request url :type "delete-job" :job-token job-token)
                     :response-re "Deleted async JOB"))))
