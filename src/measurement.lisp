;;;; measurement.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defun index-to-address (index qubit)
  "Given an amplitude index INDEX, find the amplitude address for qubit QUBIT.

Specifically, given an integer whose bit string is

    INDEX = LLLLLRRRR,

compute the address

    Result = LLLLL0RRRR

which is the index with a zero injected at the QUBIT'th position."
  (let ((left (ash index (- qubit)))
        (right (ldb (byte qubit 0) index)))
    (logior (ash left (1+ qubit))
            right)))

(defun qubit-probability (qvm qubit)
  "The probability that the qubit addressed by QUBIT is 1."
  #+#:alternate-implementation
  (second (state-probabilities qvm (nat-tuple qubit)))

  ;; Sum up all the probabilities of the qubit QUBIT being 0, and
  ;; compute the complement of that.
  (- 1.0d0 (loop :for i :below (expt 2 (1- (number-of-qubits qvm)))
                 :for address := (index-to-address i qubit)
                 :sum (probability (aref (amplitudes qvm) address)))))

(defun multi-qubit-probability (qvm &rest qubits)
  "Compute the probability that the qubits QUBITS will measure to 1 within the quantum virtual machine QVM."
  (first (last (state-probabilities qvm (apply #'nat-tuple qubits)))))

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
   (nat-tuple-complement (number-of-qubits qvm) (nat-tuple qubit)))
  (normalize-wavefunction qvm))

(defun measure (qvm q c)
  "Non-deterministically perform a measurement on the qubit addressed by Q in the quantum virtual machine QVM. Store the bit at the classical bit memory address C. If C is instead NIL, don't store.

Return two values:

    1. The resulting QVM.
    2. The measured classical bit."
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
    (values qvm cbit)))

(defun parallel-measure (qvm qubits)
  "Perform a measurement on the list of qubits QUBITS within the QVM, returning two values:

    1. The resulting QVM.
    2. A list of the measurements in the same order as the provided qubits."
  (loop :for q :in qubits
        :collect (multiple-value-bind (new-qvm cbit)
                     (measure qvm q nil)
                   (setf qvm new-qvm)
                   cbit) :into measurements
        :finally (return (values qvm measurements))))
