(in-package :qvm-app-ng-tests)

(defun plist-lowercase-keys (plist)
  (assert (evenp (length plist)))
  (loop :for (k v) :on plist :by #'cddr
        :collect (string-downcase k) :collect v))

(defun plist->json (plist)
  (with-output-to-string (*standard-output*)
    (yason:encode-plist (plist-lowercase-keys plist))))

(defun response-json-fields-checker (fields)
  (lambda (response-string)
    (let ((json-data (yason:parse response-string)))
      (mapc (lambda (spec)
              (destructuring-bind (field value &key (test 'equal)) spec
                (is (funcall (alexandria:ensure-function test) (gethash field json-data) value))))
            fields))))

(defun invalidate-token (qvm-token)
  (substitute #\5 #\4 qvm-token))

(defun simulation-method->qvm-type (simulation-method)
  (alexandria:eswitch (simulation-method :test #'string=)
    ("pure-state" "PURE-STATE-QVM")
    ("full-density-matrix" "DENSITY-QVM")))

(defun %make-url (protocol host port &optional (path "/"))
  (format nil "~A://~A:~D~A" protocol host port path))

(defmacro with-rpc-server
    ((url-var &key (protocol "http") (host "127.0.0.1") (port 0)) &body body)
  (check-type url-var symbol)
  (alexandria:once-only (protocol host port)
    (alexandria:with-gensyms (app)
      `(let (,app)
         (unwind-protect
              (progn
                (setf ,app (qvm-app-ng::start-server ,host ,port))
                (let ((,url-var (%make-url ,protocol ,host (tbnl:acceptor-port ,app))))
                  ,@body))
           (qvm-app-ng::stop-server ,app)
           (qvm-app-ng::reset-persistent-qvms-db))))))

(defmacro check-request (request-form
                         &key (status 200)
                              (response-re nil response-re-p)
                              (response-callback nil response-callback-p))
  (alexandria:once-only (status)
    (alexandria:with-gensyms
        (body-or-stream status-code headers uri stream must-close reason-phrase body-as-string)
      `(multiple-value-bind
             (,body-or-stream ,status-code ,headers ,uri ,stream ,must-close ,reason-phrase)
           ,request-form
         (declare (ignore ,headers ,uri ,must-close ,reason-phrase))
         (unwind-protect
              (progn
                (is (= ,status ,status-code))
                (let ((,body-as-string (if (streamp ,body-or-stream)
                                           (alexandria:read-stream-content-into-string ,body-or-stream)
                                           ,body-or-stream)))
                  ,@(when response-re-p
                      `((is (cl-ppcre:scan ,response-re ,body-as-string))))
                  ,@(when response-callback-p
                      `((funcall (alexandria:ensure-function ,response-callback) ,body-as-string)))

                  ,body-as-string))
           (progn
             (close ,stream)
             (when (streamp ,body-or-stream)
               (close ,body-or-stream))))))))

(defun http-request (&rest args)
  "Make an HTTP-REQUEST via DRAKMA:HTTP-REQUEST and treat application/json responses as text."
  (let ((drakma:*text-content-types* (append drakma:*text-content-types*
                                             '(("application" . "json")))))
    (apply #'drakma:http-request args)))

(defun simple-request (url &rest json-plist)
  "Make a POST request to the URL given. Any additional keyword args are collected in JSON-PLIST and converted to a JSON dict and sent as the request body."
  (http-request url :method ':POST :content (plist->json json-plist)))

(deftest test-rpc-api-invalid-request ()
  (with-rpc-server (url)
    (dolist (content '("" "not-a-json-dict"))
      (check-request (http-request url :method ':POST :content content)
                     :status 500
                     :response-re "qvm_error"))))

(deftest test-rpc-api-404 ()
  (with-rpc-server (url)
    (check-request (simple-request url)
                   :status 404)
    (check-request (simple-request url :type "some-non-existent-method")
                   :status 404)))

(deftest test-rpc-api-run-program-simple-request ()
  (with-rpc-server (url)
    (dolist (simulation-method qvm-app-ng::**available-simulation-methods**)
      (check-request (simple-request url
                                     :type "run-program"
                                     :simulation-method simulation-method
                                     :compiled-quil "DECLARE ro BIT[2]; X 0; MEASURE 0 ro[0]"
                                     :addresses (alexandria:plist-hash-table '("ro" t)))
                     :response-callback (response-json-fields-checker '(("ro" ((1 0)))))))))

(deftest test-rpc-api-run-program-addresses ()
  (with-rpc-server (url)
    ;; addresses t
    (check-request
     (simple-request url
                     :type "run-program"
                     :simulation-method "pure-state"
                     :compiled-quil "DECLARE ro BIT[2]; X 0; MEASURE 0 ro[0]"
                     :addresses (alexandria:plist-hash-table '("ro" t)))
     :response-callback (response-json-fields-checker `(("ro" ((1 0))))))

    ;; explicit index list
    (check-request
     (simple-request url
                     :type "run-program"
                     :simulation-method "pure-state"
                     :compiled-quil "DECLARE ro BIT[2]; X 0; MEASURE 0 ro[0]"
                     :addresses (alexandria:plist-hash-table '("ro" (0))))
     :response-callback (response-json-fields-checker `(("ro" ((1))))))

    ;; non-consecutive indices + "non ro" named register
    (check-request
     (simple-request url
                     :type "run-program"
                     :simulation-method "pure-state"
                     :compiled-quil "DECLARE mem BIT[3]; X 3; MEASURE 0 mem[0]; MEASURE 3 mem[2]"
                     :addresses (alexandria:plist-hash-table '("mem" (0 2))))
     :response-callback (response-json-fields-checker `(("mem" ((0 1))))))

    ;; non-existent named register
    (check-request
     (simple-request url
                     :type "run-program"
                     :simulation-method "pure-state"
                     :compiled-quil "DECLARE ro BIT; I 0"
                     :addresses (alexandria:plist-hash-table '("zonk" t)))
     :status 500
     :response-re "qvm_error")))

(global-vars:define-global-var **rpc-response-token-scanner**
    (cl-ppcre:create-scanner
     "\\A{\"token\":\"[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-4[A-Fa-f0-9]{3}-[89ABab][A-Fa-f0-9]{3}-[A-Fa-f0-9]{12}\"}\\z"
     :case-insensitive-mode t))

(deftest test-rpc-api-qvm-info ()
  (with-rpc-server (url)
    (let* ((response (check-request (simple-request url
                                                    :type "create-qvm"
                                                    :simulation-method "pure-state"
                                                    :num-qubits 1)
                                    :response-re **rpc-response-token-scanner**))
           (token (gethash "token" (yason:parse response))))
      (not-signals error (qvm-app-ng::check-qvm-token token))

      ;; info on non-existing token
      (check-request (simple-request url :type "qvm-info" :qvm-token (invalidate-token token))
                     :status 500
                     :response-re "Failed to find persistent QVM")

      ;; info on existing token
      (check-request (simple-request url :type "qvm-info" :qvm-token token)
                     :response-re "PURE-STATE-QVM")

      ;; delete on non-existing token
      (check-request (simple-request url :type "delete-qvm" :qvm-token (invalidate-token token))
                     :status 500
                     :response-re "Failed to find persistent QVM")

      ;; delete on existing token
      (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                     :response-re "Deleted persistent QVM")

      ;; info on deleted token
      (check-request (simple-request url :type "qvm-info" :qvm-token token)
                     :status 500
                     :response-re "Failed to find persistent QVM"))))

(deftest test-rpc-api-create-qvm ()
  (with-rpc-server (url)
    (dolist (simulation-method qvm-app-ng::**available-simulation-methods**)
      (dolist (num-qubits '(0 1 4))
        (let* ((response (check-request (simple-request url
                                                        :type "create-qvm"
                                                        :simulation-method simulation-method
                                                        :num-qubits num-qubits)
                                        :response-re **rpc-response-token-scanner**))
               (token (gethash "token" (yason:parse response))))
          (not-signals error (qvm-app-ng::check-qvm-token token))

          ;; check that info reports expected values for qvm-type and num-qubits
          (check-request (simple-request url :type "qvm-info" :qvm-token token)
                         :response-callback
                         (response-json-fields-checker
                          `(("qvm-type" ,(simulation-method->qvm-type simulation-method))
                            ("num-qubits" ,num-qubits))))

          ;; cleanup
          (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                         :response-re "Deleted persistent QVM"))))))

(deftest test-rpc-api-persistent-qvm-run-program ()
  (with-rpc-server (url)
    (dolist (simulation-method qvm-app-ng::**available-simulation-methods**)
      (let* ((response (check-request (simple-request url
                                                      :type "create-qvm"
                                                      :simulation-method simulation-method
                                                      :num-qubits 2)
                                      :response-re **rpc-response-token-scanner**))
             (token (gethash "token" (yason:parse response))))
        (not-signals error (qvm-app-ng::check-qvm-token token))

        ;; run-program on existing token
        (check-request (simple-request url
                                       :type "run-program"
                                       :qvm-token token
                                       :compiled-quil "DECLARE ro BIT[2]; X 0; MEASURE 0 ro[0]"
                                       :addresses (alexandria:plist-hash-table '("ro" t)))
                       :response-callback (response-json-fields-checker `(("ro" ((1 0))))))

        ;; I 0: qubit 0 remains in excited state
        (check-request (simple-request url
                                       :type "run-program"
                                       :qvm-token token
                                       :compiled-quil "DECLARE ro BIT[2]; I 0; MEASURE 0 ro[0]"
                                       :addresses (alexandria:plist-hash-table '("ro" t)))
                       :response-callback (response-json-fields-checker `(("ro" ((1 0))))))

        ;; X 0: flips qubit 0 back to ground state
        (check-request (simple-request url
                                       :type "run-program"
                                       :qvm-token token
                                       :compiled-quil "DECLARE ro BIT[2]; X 0; MEASURE 0 ro[0]"
                                       :addresses (alexandria:plist-hash-table '("ro" t)))
                       :response-callback (response-json-fields-checker `(("ro" ((0 0))))))

        ;; run-program on non-existent token
        (check-request (simple-request url
                                       :type "run-program"
                                       :qvm-token (invalidate-token token)
                                       :compiled-quil ""
                                       :addresses (make-hash-table))
                       :status 500
                       :response-re "Failed to find persistent QVM")

        (check-request (simple-request url :type "delete-qvm" :qvm-token token)
                       :response-re "Deleted persistent QVM")))))
