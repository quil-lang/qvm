;;;; measurement.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

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

(defmethod measure ((qvm pure-state-qvm) q)
  (check-type q nat-tuple-element)
  (assert (< q (number-of-qubits qvm)) (qvm q)
          "Trying to measure qubit ~D on a QVM with only ~D qubit~:P."
          q
          (number-of-qubits qvm))
  (let* ((r (random 1.0d0))
         (excited-probability (wavefunction-excited-state-probability (amplitudes qvm) q))
         (cbit (if (<= r excited-probability)
                   1
                   0)))
    ;; Force the non-deterministic measurement.
    (force-measurement cbit q qvm excited-probability)
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
  (sample-wavefunction-as-distribution wf
                                       (make-array 1
                                                   :initial-element p
                                                   :element-type 'flonum)))

(declaim (ftype (function (non-negative-fixnum non-negative-fixnum) non-negative-fixnum) midpoint)
         (inline midpoint))
(defun midpoint (a b)
  "Find the midpoint of two non-negative fixnums A and B where A <= B."
  (declare (type non-negative-fixnum a b)
           #.*optimize-dangerously-fast*)
  ;; avoid overflow by not doing (A+B)/2.
  (+ a (floor (the non-negative-fixnum (- b a)) 2)))

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
             (let* ((mid (midpoint min max))
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

(defun sample-wavefunction-multiple-times (wf num-samples)
  "Produce NUM-SAMPLES bitstring samples of the wavefunction WF according to its interpretation as a probability distribution."
  (declare #.*optimize-briskly*
           (type quantum-state wf)
           (type non-negative-fixnum num-samples))
  (loop :with cdf := (cumulative-distribution-function wf)
        :with samples := (make-array num-samples :element-type 'amplitude-address
                                                 :initial-element 0)
        :for trial :below num-samples
        :for p := (random 1.0d0)
        :do (setf (aref samples trial)
                  (loop :with min := 0
                        :with max := (1- (length wf))
                        :for mid := (midpoint min max)
                        ;; We write the binary search in the following
                        ;; way for the following reasons:
                        ;;
                        ;;     * We do not rely on the implementation
                        ;;       of MIDPOINT, except that
                        ;;
                        ;;         A < (MIDPOINT A B) < B
                        ;;
                        ;;       provided B - A > 1.
                        ;;
                        ;;     * We ensure that every iteration of the
                        ;;       loop will either reduce the size of
                        ;;       the interval (i.e., B - A diminishes
                        ;;       in size), or we terminate.
                        ;;
                        ;;     * We don't depend on (AREF CDF MIN)
                        ;;       being the same as (AREF CDF MID) in
                        ;;       the case that our interval reduced to
                        ;;       one of length 1.
                        ;;
                        ;;     * We only do lookups in CDF as needed.
                        :do (cond
                              ((= min max)       (return min))
                              ((= 1 (- max min)) (return (if (< (aref cdf min) p)
                                                             max
                                                             min)))
                              ((< (aref cdf mid) p) (setf min mid))
                              (t                    (setf max mid)))))
        :finally (return samples)))

;; XXX: Note that this doesn't follow the API of the above exactly,
;; since PS is a vector and not a single number.
(defun sample-wavefunction-as-distribution (wf ps)
  "Implementation of SAMPLE-WAVEFUNCTION-AS-DISTRIBUTION-IN-PARALLEL-TRULY, unparallelized,
 but for every probability in the PS (type: (VECTOR FLONUM)). Return a
 vector of sampled amplitude addresses."
  (declare #.*optimize-briskly*
           (type quantum-state wf)
           (type (vector flonum) ps))
  (let* ((cumsum (flonum 0))
         (len (length ps))
         (sampled-indices (make-array len :initial-element 0
                                          :element-type 'amplitude-address
                                          :fill-pointer 0)))
    (declare (type flonum cumsum)
             (type (vector amplitude-address) sampled-indices))
    (dotimes (idx (length wf) sampled-indices)
      (incf cumsum (probability (aref wf idx)))
      (loop :while (and (< (length sampled-indices) len)
                        (< (aref ps (length sampled-indices)) cumsum))
            :do (vector-push idx sampled-indices)))))
