;;;; server-abstraction.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defclass vhost (tbnl:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  (:default-initargs
   :address (error "Host address must be specified.")
   :document-root nil
   :error-template-directory nil
   :persistent-connections-p t))

(defmethod tbnl:acceptor-status-message ((acceptor vhost) http-status-code &key error &allow-other-keys)
  (if (eql http-status-code tbnl:+http-internal-server-error+)
      (with-output-to-string (s)
        (setf (tbnl:content-type*) "application/json; charset=utf-8")
        (yason:encode
         (alexandria:plist-hash-table
          (list "error_type" "qvm_error"
                "status" error))
         s))
      (call-next-method)))

(defun create-prefix/method-dispatcher (prefix method handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
starts with the string PREFIX."
  (lambda (request)
    (and (eq method (tbnl:request-method request))
         (let ((mismatch (mismatch (tbnl:script-name request) prefix
                                   :test #'char=)))
           (and (or (null mismatch)
                    (>= mismatch (length prefix)))
                handler)))))


(defmethod tbnl:acceptor-dispatch-request ((vhost vhost) request)
  ;; try REQUEST on each dispatcher in turn
  (mapc (lambda (dispatcher)
          (let ((handler (funcall dispatcher request)))
            (when handler               ; Handler found. FUNCALL it and return result
              (return-from tbnl:acceptor-dispatch-request (funcall handler request)))))
        (dispatch-table vhost))
  (call-next-method))
