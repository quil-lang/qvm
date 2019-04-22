;;;; api/effect-change.lisp
;;;;
;;;; Author: Erik Davis
;;;;         Robert Smith

(in-package #:qvm-app)

(defgeneric perform-run-for-effect (simulation-method quil num-qubits &key gate-noise measurement-noise)
  (:method (simulation-method quil num-qubits &key gate-noise measurement-noise)
    (declare (ignore gate-noise measurement-noise))
    (api-method-not-implemented-error 'perform-run-for-effect))
  (:documentation "Executes a quil program for the purpose of updating a shared or persistent wavefunction."))

(defmethod perform-run-for-effect ((simulation-method (eql 'pure-state)) quil num-qubits &key gate-noise measurement-noise)
  (%execute-quil simulation-method quil num-qubits gate-noise measurement-noise))

(defmethod perform-run-for-effect ((simulation-method (eql 'full-density-matrix)) quil num-qubits &key gate-noise measurement-noise)
  (%execute-quil simulation-method quil num-qubits gate-noise measurement-noise))

(defun |POST-effect/id| (request args)
  (let ((key (cdr (assoc "id" args :test #'string=)))
        (js (extract-json-payload request)))
    (check-for-quil-instrs-field js)
    (let* ((quil (let ((quil:*allow-unresolved-applications* t))
                   (safely-parse-quil-string (get-quil-instrs-field js))))
           (num-qubits (cl-quil:qubits-needed quil))
           (qvm (lookup-persistent-qvm-for-ip key (tbnl:real-remote-addr request))))
      (overwrite-execution-parameters-according-to-program qvm quil)
      (%execute qvm)
      (load-time-value
       (yason-encode-to-string t)))))
