;;;; measurement.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defun index-to-address (index qubit)
  (let ((left (ash index (- qubit)))
        (right (ldb (byte qubit 0) index)))
    (logior (ash left (1+ qubit))
            right)))

;; This can be implemented in terms of STATE-PROBABILITIES.
(defun qubit-probability (qvm qubit)
  "The probability that the qubit addressed by QUBIT is 1."
  #+#:alternate-implementation
  (second (state-probabilities qvm (nat-tuple qubit)))
  (- 1.0d0 (loop :for i :below (expt 2 (1- (number-of-qubits qvm)))
                 :for address := (index-to-address i qubit)
                 :sum (probability (aref (amplitudes qvm) address)))))

(defun normalize-wavefunction (qvm)
  "Normalize the wavefunction, making it a unit vector in the constituent Hilbert space."
  (let ((amps (amplitudes qvm)))
    (loop :for amp :across amps
          :sum (probability amp) :into square-norm
          :finally (map-into amps
                             (let ((norm (sqrt square-norm)))
                               (lambda (amp)
                                 (/ amp norm)))
                             amps)))
  ;; Return the normalized QVM.
  qvm)

(defun force-measurement (measured-value qubit qvm)
  "Force the quantum system QVM to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the amplitudes of all other qubits accordingly."
  (check-type measured-value bit)
  (map-relevant-amplitudes
   qvm
   (lambda (combo)
     (declare (ignore combo))
     (let* ((x (extract-amplitudes qvm (nat-tuple qubit))))
       ;; Collapse x.
       (ecase measured-value
         (0 (setf (aref x 1) #C(0.0D0 0.0D0)))
         (1 (setf (aref x 0) #C(0.0D0 0.0D0))))
       ;; Insert it back.
       (insert-amplitudes qvm x (nat-tuple qubit))))
   (nat-tuple-remove (qubit-numbers qvm) qubit))
  (normalize-wavefunction qvm))

(defun measure (qvm q c)
  "Non-deterministically perform a measurement on the qubit addressed by Q in the quantum virtual machine QVM. Store the bit in at the classical bit memory address C. If C is instead NIL, don't store."
  (let* ((r (random 1.0d0))
         (cbit (if (<= r (qubit-probability qvm q))
                   1
                   0)))
    ;; Force the non-deterministic measurement.
    (force-measurement cbit q qvm)

    ;; Store the classical bit if necessary.
    (unless (null c)
      (setf (classical-bit qvm c) cbit))

    ;; Return the qvm.
    qvm))
