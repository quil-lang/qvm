;;;; quil.lisp
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
  (labels ((substitute-param (p sym body)
             (subst sym p body :test #'eq))
           (substitute-params (params syms body)
             (if (null params)
                 body
                 (substitute-params (rest params)
                                    (rest syms)
                                    (substitute-param (first params)
                                                      (first syms)
                                                      body))))
           (lambda-form (params dimension entries)
             (let ((syms (mapcar (lambda (p) (gensym (quil:param-name p))) params)))
               `(lambda ,syms
                  (make-matrix ,dimension
                               ,@(substitute-params params syms entries))))))
    (let* ((name (quil:gate-definition-name gate-def))
           (entries (quil:gate-definition-entries gate-def))
           (params (quil:gate-definition-parameters gate-def))
           (dim (isqrt (length entries))))
      (make-instance 'parameterized-gate
                     :name name
                     :dimension dim
                     :arity (length params)
                     :matrix-function (compile nil (lambda-form params dim entries))))))
