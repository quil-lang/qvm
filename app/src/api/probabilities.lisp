;;;; api/probabilities.lisp
;;;;
;;;; Author: Erik Davis

(in-package  #:qvm-app)

(defgeneric perform-probabilities (simulation-method quil num-qubits &key gate-noise measurement-noise)
  (:method (simulation-method quil num-qubits &key gate-noise measurement-noise)
    (declare (ignore gate-noise measurement-noise))
    (api-method-not-implemented-error 'perform-probabilities))
  (:documentation "Executes a program and returns the resulting probability distribution over the computational basis."))

(defmethod perform-probabilities ((simulation-method (eql 'pure-state)) quil num-qubits &key gate-noise measurement-noise)
  (let* ((qvm (%execute-quil simulation-method quil num-qubits gate-noise measurement-noise))
         (amplitudes (qvm::amplitudes qvm))
         (probabilities (make-array (length amplitudes) :element-type 'double-float)))
    (loop :for i :below (length amplitudes)
          :do (setf (aref probabilities i)
                    (qvm::probability (aref amplitudes i))))
    (values qvm
            probabilities)))

(defmethod perform-probabilities ((simulation-method (eql 'full-density-matrix)) quil num-qubits &key gate-noise measurement-noise)
  (let* ((qvm (%execute-quil simulation-method quil num-qubits gate-noise measurement-noise)))
    (values qvm
            (qvm::density-qvm-basis-probabilities qvm))))
