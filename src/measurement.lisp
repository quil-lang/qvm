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

(defun measure-all (qvm)
  "Measure all of the qubits in the quantum virtual machine QVM. Return the (modified) QVM, and the measured bit values for qubits 0, ..., n in a list (B0 ... Bn) where Bk is the bit-value measurement of qubit k."
  (flet ((index-to-bits (n)
           (loop :for i :below (number-of-qubits qvm)
                 :collect (ldb (byte 1 i) n))))
    (let* ((wf (amplitudes qvm))
           (basis-state (sample-wavefunction-as-distribution
                         wf
                         (flonum (random 1.0d0)))))
      ;; Reset to |0>
      (bring-to-zero-state wf)
      ;; Rotate the amplitude to BASIS-STATE.
      (rotatef (aref wf 0)
               (aref wf basis-state))

      ;; Return.
      (values qvm (index-to-bits basis-state)))))

(defun sample-wavefunction-as-distribution (wf p)
  "Sample the wavefunction as if it was a probability distribution.

Specifically, let C(b) = \sum_{k=0}^{b} |wf[k]|^2. Compute the smallest b' such that C(b') > p."
  (declare (optimize (speed 0) safety debug)
           (type quantum-state wf)
           (type flonum p))
  (assert (and (<= 0 p 1)))
  (labels ((middle (min max)
             (assert (plusp (- max min 1)))
             (floor (+ min max) 2))

           (find-it (sum< min max)
             (declare (type non-negative-fixnum min max)
                      (type flonum sum<))
             (assert (< min max))

             (let ((sum (+ sum<
                           (psum-dotimes (i (- max min))
                             (let ((i (the non-negative-fixnum (+ i min))))
                               (probability (aref wf i)))))))
               (declare (type flonum sum))
               (cond
                 ;; We didn't find it here. We need to keep
                 ;; searching rightward.
                 ((<= sum p)
                  (values nil sum))

                 ;; We did find it, and the range is small.
                 ((= min (1- max))
                  (values min sum))

                 ;; We did find it, but we need to refine.
                 (t
                  (let ((m (middle min max)))
                    (multiple-value-bind (left-idx left-sum)
                        (find-it sum< min m)
                      (if left-idx
                          (values left-idx left-sum)
                          (find-it left-sum m max)))))))))
    (or (find-it (flonum 0) 0 (length wf))
        ;; We've reached the upper bound of the array.
        (1- (length wf)))))
