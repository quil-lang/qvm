;;;; api/wavefunction.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defgeneric perform-wavefunction (simulation-method quil num-qubits &key gate-noise measurement-noise)
  (:method (simulation-method quil num-qubits &key gate-noise measurement-noise)
    (declare (ignore gate-noise measurement-noise))
    (api-method-not-implemented-error 'perform-wavefunction)))

(defmethod perform-wavefunction ((simulation-method (eql 'pure-state)) quil num-qubits &key gate-noise measurement-noise)
  (%execute-quil simulation-method quil num-qubits gate-noise measurement-noise))

(defmethod perform-wavefunction ((simulation-method (eql 'full-density-matrix)) quil num-qubits &key gate-noise measurement-noise)
  (%execute-quil simulation-method quil num-qubits gate-noise measurement-noise))

(defun %execute-quil (simulation-method quil num-qubits gate-noise measurement-noise)
  (check-type quil quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  (let ((qvm (make-appropriate-qvm simulation-method quil num-qubits gate-noise measurement-noise))
        timing)
    (qvm:load-program qvm quil)
    (format-log ':debug "Running experiment on ~A" (class-name (class-of qvm)))
    (with-timing (timing)
      (with-timeout
        (qvm:run qvm)))
    (format-log ':debug "Finished in ~D ms" timing)
    qvm))
