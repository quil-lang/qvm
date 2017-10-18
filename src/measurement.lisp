;;;; measurement.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(declaim (inline index-to-address))
(defun index-to-address (index qubit state)
  "Given an amplitude index INDEX, find the amplitude address for the INDEX'th basis state with QUBIT in the STATE state.

Specifically, given an integer whose bit string is

    INDEX = LLLLRRRR,

compute the address

    Result = LLLL{0,1}RRRR

which is the index with a {1, 0} injected at the QUBIT'th position."
  (declare (type nat-tuple-element qubit)
           (type amplitude-address index)
           (type bit state)
           #.*optimize-dangerously-fast*)
  (dpb state (byte 1 qubit) (inject-bit index qubit)))

(defun qubit-probability (qvm qubit)
  "The probability that the physical qubit addressed by QUBIT is 1."
  (declare #.*optimize-dangerously-fast*
           (inline probability))
  (let ((wavefunction (amplitudes qvm)))
    (declare (type quantum-state wavefunction))
    (psum-dotimes (i (half (length wavefunction)))
      (let ((address (index-to-address i qubit 1)))
        (declare (type amplitude-address address))
        (probability (aref wavefunction address))))))

(defun force-measurement (measured-value qubit qvm excited-probability)
  "Force the quantum system QVM to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the amplitudes of all other qubits accordingly.

EXCITED-PROBABILITY should be the probability that QUBIT measured to |1>, regardless of what it's being forced as.
"
  (declare #.*optimize-dangerously-fast*
           (type bit measured-value)
           (type (flonum 0) excited-probability)
           (type nat-tuple-element qubit))
  (let* ((wavefunction (amplitudes qvm))
         (annihilated-state (- 1 measured-value))
         (inv-norm (if (zerop annihilated-state)
                       (/ (sqrt excited-probability))
                       (/ (sqrt (- (flonum 1) excited-probability))))))
    (declare (type quantum-state wavefunction)
             (type bit annihilated-state)
             (type (flonum 0) inv-norm))
    ;; Here we step through all of the wavefunction amplitudes,
    ;; simultaneously doing the projection ( psi[i] = 0 ) and the
    ;; wavefunction renormalization ( psi[i] *= inv-norm ).
    (if (<= *qubits-required-for-parallelization* (number-of-qubits qvm))
        (lparallel:pdotimes (i (length wavefunction))
          (declare (type amplitude-address i))
          (if (= annihilated-state (ldb (byte 1 qubit) i))
              (setf (aref wavefunction i) (cflonum 0))
              (setf (aref wavefunction i) (* inv-norm (aref wavefunction i)))))
        (dotimes (i (length wavefunction))
          (declare (type amplitude-address i))
          (if (= annihilated-state (ldb (byte 1 qubit) i))
              (setf (aref wavefunction i) (cflonum 0))
              (setf (aref wavefunction i) (* inv-norm (aref wavefunction i)))))))

  ;; Return the QVM.
  qvm)

(defun measure (qvm q c)
  "Non-deterministically perform a measurement on the qubit addressed by Q in the quantum virtual machine QVM. Store the bit at the classical bit memory address C. If C is instead NIL, don't store.

Return two values:

    1. The resulting QVM.
    2. The measured classical bit."
  (let* ((r (random 1.0d0))
         (excited-probability (qubit-probability qvm q))
         (cbit (if (<= r excited-probability)
                   1
                   0)))
    ;; Force the non-deterministic measurement.
    (force-measurement cbit q qvm excited-probability)

    ;; Store the classical bit if necessary.
    (unless (null c)
      (setf (classical-bit qvm c) cbit))

    ;; Return the qvm.
    (values qvm cbit)))

