(in-package :qvm-app-ng-tests)

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
    (mapcar #'string-downcase qvm-app-ng::**available-allocation-methods**)
  :test #'equal)

(alexandria:define-constant +simulation-method-strings+
    (mapcar #'string-downcase qvm-app-ng::**available-simulation-methods**)
  :test #'equal)

(defun plist-lowercase-keys (plist)
  (assert (evenp (length plist)))
  (loop :for (k v) :on plist :by #'cddr
        :collect (string-downcase k) :collect v))

(defun plist->json (plist)
  (with-output-to-string (*standard-output*)
    (yason:encode-plist (plist-lowercase-keys plist))))

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

(defun plist-downcase-keys (plist)
  (assert (evenp (length plist)))
  (loop :for (key value) :on plist :by #'cddr
        :nconc (list (string-downcase key) value)))

(defun invalidate-token (qvm-token)
  (substitute #\5 #\4 qvm-token))

(defun extract-token (response)
  (gethash "token" (yason:parse response)))

(defun extract-and-validate-token (response)
  (let ((token (extract-token response)))
    (is (qvm-app-ng::valid-persistent-qvm-token-p token))
    token))

(defun simulation-method->qvm-type (simulation-method)
  (alexandria:eswitch ((string-downcase simulation-method) :test #'string=)
    ("pure-state" "PURE-STATE-QVM")
    ("full-density-matrix" "DENSITY-QVM")))

(defun %make-url (protocol host port &optional (path "/"))
  (format nil "~A://~A:~D~A" protocol host port path))

(defun call-with-rpc-server (host port function)
  (let (rpc-acceptor)
    (unwind-protect
         (progn
           (setf rpc-acceptor (qvm-app-ng::start-server host port))
           (funcall function (%make-url "http" host (tbnl:acceptor-port rpc-acceptor))))
      (qvm-app-ng::stop-server rpc-acceptor)
      (qvm-app-ng::reset-persistent-qvms-db))))

(defmacro with-rpc-server ((url-var &key (host "127.0.0.1") (port 0)) &body body)
  "Execute BODY with URL-VAR bound the URL of a new RPC server started on HOST and PORT.

HOST defaults to 127.0.0.1 and PORT defaults to a randomly assigned port."
  (check-type url-var symbol)
  (alexandria:once-only (host port)
    `(call-with-rpc-server ,host ,port (lambda (,url-var) ,@body))))

(defun call-with-drakma-request (body-or-stream status-code headers uri stream must-close reason-phrase function)
  (unwind-protect
       (funcall function body-or-stream status-code headers uri stream must-close reason-phrase)
    (progn
      (close stream)
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
           (declare (ignore ,headers ,uri ,must-close ,reason-phrase))
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

(deftest test-rpc-api-invalid-request ()
  "Requests without a valid JSON request body return 400 Bad Request."
  (with-rpc-server (url)
    (dolist (content '("" "not-a-json-dict"))
      (check-request (http-request url :method ':POST :content content)
                     :status 400
                     :response-re "Bad Request"))))

(deftest test-rpc-api-404 ()
  "Requests for URIs other than \"/\" or for non-existent RPC methods return 404 Not Found."
  (with-rpc-server (url)
    (check-request (simple-request url)
                   :status 404)
    (check-request (simple-request url :type "some-non-existent-method")
                   :status 404)))

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
                       :response-callback (response-json-fields-checker '(("ro" ((1 0))))))

        (check-request (simple-request url
                                       :type "run-program"
                                       :qvm-token ()
                                       :allocation-method allocation-method
                                       :simulation-method simulation-method
                                       :compiled-quil +generic-x-0-quil-program+
                                       :addresses +all-ro-addresses+)
                       :response-callback (response-json-fields-checker '(("ro" ((1 0))))))))))

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
                   :status 500)))

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
     :response-callback (response-json-fields-checker `(("ro" ((1 0))))))

    ;; explicit index list
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil +generic-x-0-quil-program+
                     :addresses (alexandria:plist-hash-table '("ro" (0))))
     :response-callback (response-json-fields-checker `(("ro" ((1))))))

    ;; non-consecutive indices + "non ro" named register
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil "DECLARE mem BIT[3]; X 3; MEASURE 0 mem[0]; MEASURE 3 mem[2]"
                     :addresses (alexandria:plist-hash-table '("mem" (0 2))))
     :response-callback (response-json-fields-checker `(("mem" ((0 1))))))

    ;; non-existent named register
    (check-request
     (simple-request url
                     :type "run-program"
                     :allocation-method "native"
                     :simulation-method "pure-state"
                     :compiled-quil "DECLARE ro BIT; I 0"
                     :addresses (alexandria:plist-hash-table '("zonk" t)))
     :status 500
     :response-re "qvm_error")))

(global-vars:define-global-var **rpc-response-token-scanner**
    (cl-ppcre:create-scanner
     "\\A{\"token\":\"[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}\"}\\z"))

(global-vars:define-global-var **iso-time-scanner**
    (cl-ppcre:create-scanner
     "\\A\\d{4}-(?:1[0-2]|0[1-9])-(?:3[0-1]|[1-2][0-9]|0[0-9]) (?:2[0-3]|[0-1][0-9]):[0-5][0-9]:[0-5][0-9]\\z"))

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
                                          :response-re **rpc-response-token-scanner**))
                 (token (extract-and-validate-token response)))

            ;; check that info reports expected values for qvm-type and num-qubits
            (check-request (simple-request url :type "qvm-info" :qvm-token token)
                           :response-callback
                           (response-json-fields-checker
                            `(("qvm-type" ,(simulation-method->qvm-type simulation-method))
                              ("num-qubits" ,num-qubits)
                              ("metadata" ,(hash-table-fields-checker
                                            `(("allocation-method" ,(string-upcase allocation-method))
                                              ("created" ,(lambda (s)
                                                            (cl-ppcre:scan **iso-time-scanner** s)))))))))

            ;; cleanup
            (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                           :response-re "Deleted persistent QVM")))))))

(deftest test-rpc-api-create-qvm-invalid-requests ()
  "Test input validate for the create-qvm call."
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
                                    :response-re **rpc-response-token-scanner**))
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
                        ("metadata" ,(hash-table-fields-checker
                                      `(("allocation-method" "NATIVE")
                                        ("created" ,(lambda (s)
                                                      (cl-ppcre:scan **iso-time-scanner** s)))))))))

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
                                       :response-re **rpc-response-token-scanner**))
              (token (extract-and-validate-token response)))

         ;; run-program on existing token
         (check-request (simple-request url
                                        :type "run-program"
                                        :qvm-token token
                                        :compiled-quil +generic-x-0-quil-program+
                                        :addresses +all-ro-addresses+)
                        :response-callback (response-json-fields-checker `(("ro" ((1 0))))))

         ;; I 0: qubit 0 remains in excited state
         (check-request (simple-request url
                                        :type "run-program"
                                        :qvm-token token
                                        :compiled-quil "DECLARE ro BIT[2]; I 0; MEASURE 0 ro[0]"
                                        :addresses +all-ro-addresses+)
                        :response-callback (response-json-fields-checker `(("ro" ((1 0))))))

         ;; X 0: flips qubit 0 back to ground state
         (check-request (simple-request url
                                        :type "run-program"
                                        :qvm-token token
                                        :compiled-quil +generic-x-0-quil-program+
                                        :addresses +all-ro-addresses+)
                        :response-callback (response-json-fields-checker `(("ro" ((0 0))))))

         ;; cleanup
         (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                        :response-re "Deleted persistent QVM"))))))
