;;;; api/effect-change.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:qvm-app)

(defgeneric perform-run-for-effect (simulation-method quil num-qubits &key gate-noise measurement-noise)
  (:method (simulation-method quil num-qubits &key gate-noise measurement-noise)
    (declare (ignore gate-noise measurement-noise))
    (api-method-not-implemented-error 'perform-run-for-effect))
  (:documentation "Executes a quil program for the purpose of updating a shared or persistent wavefunction."))

(defmethod perform-run-for-effect ((simulation-method (eql 'pure-state)) quil num-qubits &key gate-noise measurement-noise)
  (unless **persistent-wavefunction**
    (warn "RUN-FOR-EFFECT called statelessly."))
  (%execute-quil simulation-method quil num-qubits gate-noise measurement-noise))

(defmethod perform-run-for-effect ((simulation-method (eql 'full-density-matrix)) quil num-qubits &key gate-noise measurement-noise)
  (unless **persistent-wavefunction**
    (warn "RUN-FOR-EFFECT called statelessly."))
  (%execute-quil simulation-method quil num-qubits gate-noise measurement-noise))
