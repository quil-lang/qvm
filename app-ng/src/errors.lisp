(in-package :qvm-app-ng)

(define-condition rpc-error (simple-error)
  ((http-status
    :initarg :http-status
    :initform tbnl:+http-internal-server-error+
    :reader rpc-error-http-status))
  (:report (lambda (condition stream)
             (format stream "QVM RPC Error:~@[ ~A~]"
                     (tbnl:reason-phrase (rpc-error-http-status condition)))
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
  (def-rpc-error rpc-bad-request-error tbnl:+http-bad-request+
    "Error signalled when caller makes an invalid RPC request.")
  (def-rpc-error rpc-parameter-parse-error tbnl:+http-bad-request+
    "Error signalled when a user provides a bad input parameter."))
