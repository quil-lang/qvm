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

(defmethod measure ((qvm pure-state-qvm) q c)
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

(defmethod measure-all ((qvm pure-state-qvm))
  (flet ((index-to-bits (n)
           (loop :for i :below (number-of-qubits qvm)
                 :collect (ldb (byte 1 i) n))))
    (let* ((wf (amplitudes qvm))
           (basis-state (sample-wavefunction-as-distribution-in-parallel-truly
                         wf
                         (flonum (random 1.0d0)))))
      ;; Reset to |0>
      (bring-to-zero-state wf)
      ;; Rotate the amplitude to BASIS-STATE.
      (rotatef (aref wf 0)
               (aref wf basis-state))

      ;; Return.
      (values qvm (index-to-bits basis-state)))))

(defun sample-wavefunction-as-distribution-in-parallel (wf p)
  (sample-wavefunction-as-distribution wf (list p)))

(defun sample-wavefunction-as-distribution-in-parallel-truly (wf p)
  "Sample the wavefunction as if it was a probability distribution.

Specifically, let C(b) = \sum_{k=0}^{b} |wf[k]|^2. Compute the smallest b' such that C(b') > p."
  (declare #.*optimize-briskly*
           (type quantum-state wf)
           (type flonum p))
  (assert (and (<= 0 p 1)))
  (let ((min 0)
        (max (length wf))
        (sum< (flonum 0)))
    (declare (type non-negative-fixnum min max)
             (type flonum sum<))
    (assert (< min max))
    ;; Starting with the full range of indices we find the correct
    ;; index by iteratively sub dividing the interval [min, max) in
    ;; two, then verifying which half b' is in.
    ;;
    ;; Given a WF of length L, the number of elements squared and
    ;; summed is
    ;;
    ;; L/2 + L/4 + L/8 + ... = L/2 (1-(1/2)^(1+log2(L)))) / (1-1/2) ~= L
    ;;
    ;; TODO: Figure out recursive version that works for multiple
    ;; samples p
    (loop :until (= min (1- max))
          :do
             (assert (plusp (- max min 1)))
             (let* ((mid (floor (+ min max) 2))

                    ;; sum values in lower half of interval
                    (sum (+ sum<
                            (psum-dotimes (i (the non-negative-fixnum (- mid min)))
                              (let ((i (the non-negative-fixnum (+ i min))))
                                (probability (aref wf i)))))))
               (declare (type non-negative-fixnum mid)
                        (type flonum sum))
               (cond
                 ;; We didn't find it in the lower half
                 ;; Update the interval to the upper half
                 ((<= sum p)
                  (setf min mid)
                  (setf sum< sum))

                 ;; We found it, update interval to lower half
                 (t
                  (setf max mid)))))
    min))

(defun sample-wavefunction-as-distribution (wf ps)
  "Implementation of SAMPLE-WAVEFUNCTION-AS-DISTRIBUTION-IN-PARALLEL-TRULY, unparallelized,
 but for every probability in PS."
  (declare #.*optimize-briskly*
           (type quantum-state wf)
           (type (vector flonum) ps))
  (let ((cumsum 0.0)
        (len (length ps))
        (sampled-indices (make-array (length ps) :initial-element 0
                                     :element-type 'fixnum
                                     :fill-pointer 0)))
    (declare (type flonum cumsum)
             (type (vector fixnum) sampled-indices))
    (dotimes (idx (length wf) sampled-indices)
      (incf cumsum (probability (aref wf idx)))
      (loop :while (and (< (length sampled-indices) len)
                        (< (aref ps (length sampled-indices)) cumsum))
            :do (vector-push idx sampled-indices)))))
