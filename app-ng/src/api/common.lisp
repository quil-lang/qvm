;;;; api/common.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:qvm-app-ng)

(define-condition api-method-not-implemented (error)
  ((method :initarg :method
           :reader api-method
           :documentation "The name of the API method.")
   (simulation-method :initarg :simulation-method
                      :reader active-simulation-method
                      :documentation "The active QVM simulation method."))
  (:report (lambda (condition stream)
             (format stream "API method ~A not supported when QVM is using ~A simulation method."
                     (api-method condition)
                     (active-simulation-method condition))))
  (:documentation "Indicates that the given API method is not implemented for a specific simulation method."))

(defun api-method-not-implemented-error (method)
  "Signal that the given API method named METHOD is not implemented."
  (error 'api-method-not-implemented
         :method method
         :simulation-method *simulation-method*))
