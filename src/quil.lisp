;;;; src/quil.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Some auxiliary routines to further process Quil code.

(defgeneric gate-definition-to-gate (gate-def)
  (:documentation "Convert a parsed Quil gate definition to a usable, executable gate."))

(defmethod gate-definition-to-gate ((gate-def quil:static-gate-definition))
  (let* ((name (quil:gate-definition-name gate-def))
         (entries (quil:gate-definition-entries gate-def))
         (dim (isqrt (length entries))))
    (make-instance 'simple-gate
                   :name name
                   :dimension dim
                   :matrix (apply #'make-matrix dim entries))))

(defmethod gate-definition-to-gate ((gate-def quil:parameterized-gate-definition))
  (flet ((lambda-form (params dimension entries)
           `(lambda ,params
              (declare (ignorable ,@params))
              (make-matrix ,dimension ,@entries))))
    (let* ((name (quil:gate-definition-name gate-def))
           (entries (quil:gate-definition-entries gate-def))
           (params (quil:gate-definition-parameters gate-def))
           (dim (isqrt (length entries))))
      (assert (every #'symbolp params))
      (make-instance 'parameterized-gate
                     :name name
                     :dimension dim
                     :arity (length params)
                     :matrix-function (compile nil (lambda-form params dim entries))))))
