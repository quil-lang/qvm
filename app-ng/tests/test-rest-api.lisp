(in-package :qvm-app-ng-tests)

(defun plist-lowercase-keys (plist)
  (assert (evenp (length plist)))
  (loop :for (k v) :on plist :by #'cddr
        :collect (string-downcase k) :collect v))

(defun plist->json (plist)
  (with-output-to-string (*standard-output*)
    (yason:encode-plist (plist-lowercase-keys plist))))

(defun %url (protocol host port &optional path)
  (format nil "~A://~A:~D~@[~A~]" protocol host port path))

(defun %request-url (host-url path)
  (concatenate 'string host-url path))

(defmacro with-rest-server
    ((url-var &key (protocol "http") (host "127.0.0.1") (port 0)) &body body)
  (check-type url-var symbol)
  (alexandria:once-only (protocol host port)
    (alexandria:with-gensyms (app)
      `(let (,app)
         (unwind-protect
              (progn
                (setf ,app (qvm-app-ng::start-server ,host ,port))
                (let ((,url-var (%url ,protocol ,host (tbnl:acceptor-port ,app))))
                  ,@body))
           (tbnl:stop ,app))))))

(defmacro check-request (request-form &key (status 200) content-re)
  (alexandria:once-only (status content-re)
    (alexandria:with-gensyms (body-or-stream status-code headers uri stream must-close reason-phrase)
      `(multiple-value-bind (,body-or-stream ,status-code ,headers ,uri ,stream ,must-close ,reason-phrase)
           ,request-form
         (declare (ignore ,headers ,uri ,must-close ,reason-phrase))
         (unwind-protect
              (progn
                (is (= ,status ,status-code))
                (when ,content-re
                  (is (cl-ppcre:scan ,content-re
                                     (if (streamp ,body-or-stream)
                                         (alexandria:read-stream-content-into-string ,body-or-stream)
                                         ,body-or-stream)))))
           (progn
             (close ,stream)
             (when (streamp ,body-or-stream)
               (close ,body-or-stream))))))))

(defun simple-request (method path &rest json-plist)
  (with-rest-server (host-url)
    (drakma:http-request (%request-url host-url path) :method method :content (plist->json json-plist))))

(deftest test-rest-api-version ()
  (check-request (simple-request ':POST "/" :type "version")
                 :content-re "\\A[0-9.]+ \\[[A-Fa-f0-9]+\\]\\z" ))
