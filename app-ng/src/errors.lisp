(in-package #:qvm-app-ng)

(define-condition rpc-error (simple-error)
  ((http-status
    :initarg :http-status
    :initform +http-internal-server-error+
    :reader rpc-error-http-status))
  (:report (lambda (condition stream)
             (format stream "QVM RPC Error:~@[ ~A~]"
                     (http-status-string (rpc-error-http-status condition)))
             (alexandria:when-let ((format-control (simple-condition-format-control condition)))
               (format stream "~&~?" format-control (simple-condition-format-arguments condition)))))
  (:documentation "Base error type for all RPC errors."))

(macrolet ((def-rpc-error (name http-status documentation)
             `(progn
                (define-condition ,name (rpc-error)
                  ()
                  (:default-initargs :http-status ,http-status)
                  (:documentation ,documentation))
                (defun ,name (&optional format-control &rest format-arguments)
                  (error ',name :format-control format-control :format-arguments format-arguments)))))
  (def-rpc-error rpc-bad-request-error +http-bad-request+
    "Error signalled when caller makes an invalid RPC request.")
  (def-rpc-error rpc-parameter-parse-error +http-bad-request+
    "Error signalled when a user provides a bad input parameter."))

(define-condition user-input-error (simple-error) ()
  (:documentation "Error that is signaled when validating user input fails."))

(defun user-input-error (&optional format-control &rest format-arguments)
  (error 'user-input-error :format-control format-control :format-arguments format-arguments))

(defun rewrap-simple-error (original-error new-error-type)
  "Maybe rewrap ORIGINAL-ERROR as a NEW-ERROR-TYPE.

If both (TYPE-OF ORIGINAL-ERROR) and NEW-ERROR-TYPE are non-EQ subtypes of SIMPLE-ERROR, then return a new ERROR of type NEW-ERROR-TYPE copying over ORIGINAL-ERROR's :FORMAT-CONTROL and :FORMAT-ARGUMENTS.

Otherwise, return ORIGINAL-ERROR."
  (cond ((and (typep original-error 'simple-error)
              (subtypep new-error-type 'simple-error)
              (not (eq (type-of original-error) new-error-type)))
         (make-condition new-error-type
                         :format-control (simple-condition-format-control original-error)
                         :format-arguments (simple-condition-format-arguments original-error)))
        (t original-error)))

(defun call-with-rewrapped-simple-errors (new-error-type function &rest args)
  "APPLY FUNCTION to ARGS in an environment where SIMPLE-ERRORs are re-signaled as NEW-ERROR-TYPE.

NEW-ERROR-TYPE should itself be a subtype of SIMPLE-ERROR."
  (handler-case
      (apply function args)
    (simple-error (c)
      ;; Alternatively, we could CHANGE-CLASS and UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, but lets not
      ;; get too crazy just yet. We're saving the Good Stuff for last.
      (error (rewrap-simple-error c new-error-type)))))
