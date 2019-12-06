;;;; measurement.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defgeneric force-measurement (measured-value qubit state excited-probability)
  (:documentation "Manipulate the STATE as to force the QUBIT in STATE to collapse to the MEASURED-VALUE. EXCTIED-PROBABILITY is the probability that the specified QUBIT measures to |1>, regardless of the MEASURED-VALUE it is being forced to."))

(defmethod force-measurement (measured-value qubit (state pure-state) excited-probability)
  "Force the quantum system of the PURE-STATE STATE to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the amplitudes of all other qubits accordingly. EXCITED-PROBABILITY should be the probability that QUBIT measured to |1>, regardless of what it's being forced as.
"
  (declare #.*optimize-dangerously-fast*
           (type bit measured-value)
           (type (flonum 0) excited-probability)
           (type nat-tuple-element qubit))
  (let* ((wavefunction (state-elements state))
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
    (if (<= *qubits-required-for-parallelization* (num-qubits state))
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
  ;; Return the STATE.
  state)

(defmethod force-measurement (measured-value qubit (state density-matrix-state) 
                              excited-probability)
  "Force the density-matrix-state STATE to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the density matrix appropriately.EXCITED-PROBABILITY should be the probability that QUBIT measured to |1>, regardless of what it's being forced as.
"
  ;; A measurement of qubit i corresponds to roughly this:
  ;; If outcome = 0, set rows/columns corresponding to i = 1 to zero
  ;; If outcome = 1, set rows/columns corresponding to i = 0 to zero

  ;; The normalization condition on the density matrix is that the
  ;; diagonal entries sum to 1, so we have to rescale the remaining
  ;; nonzero entries. This is easier than the wavefunction case, where
  ;; the normalization condition is that the sum of squares is 1.
  (let* ((annihilated-state (- 1 measured-value))
         (inv-norm (if (zerop annihilated-state)
                       (/ excited-probability)
                       (/ (- (flonum 1) excited-probability))))
         (num-qubits (num-qubits state))
         (vec-density (state-elements state)))
    (pdotimes (k (length vec-density) state)
      ;; Check whether the row or column index refers to an annihilated state
      (if (or (= annihilated-state (ldb (byte 1 qubit) k))
              (= annihilated-state (ldb (byte 1 (+ qubit num-qubits)) ; in the above parlance, a "ghost" qubit
                                        k)))
          (setf (aref vec-density k) (cflonum 0))
          (setf (aref vec-density k)
                (* inv-norm (aref vec-density k)))))))


(defgeneric get-excited-state-probability (state qubit)
  (:documentation "Get the excited state probability from the wavefunction (if PURE-STATE) or density matrix (if DENSITY-MATRIX-STATE).")
  
  (:method ((state pure-state) qubit)
    (wavefunction-excited-state-probability (state-elements state) qubit))
  
  (:method ((state density-matrix-state) qubit)
    (check-type state density-matrix-state)
    (let ((rho (matrix-view state)))
      (declare (type density-operator-matrix-view rho))
      ;; This is a sum along the diagonal of the DIM x DIM density matrix
      ;; Only indices with qubit in excited state contribute
      (psum-dotimes (k (expt 2 (1- (num-qubits state))))
        (let ((i (index-to-address k qubit 1)))
          (realpart (aref rho i i)))))))

(defmethod measure ((qvm base-qvm) q)
  (check-type q nat-tuple-element)
  (assert (< q (number-of-qubits qvm)) (qvm q)
          "Trying to measure qubit ~D on a QVM with only ~D qubit~:P."
          q
          (number-of-qubits qvm))
  (let* ((r (random 1.0d0))
         (excited-probability (get-excited-state-probability (state qvm) q))
         (cbit (if (<= r excited-probability)
                   1
                   0)))
    ;; Force the non-deterministic measurement.
    (force-measurement cbit q (state qvm) excited-probability)
    ;; Return the qvm.
    (values qvm cbit)))

(defmethod apply-measure-discard-to-state (qvm (state pure-state) (instr quil:measure-discard))
  ;; Simply measure the qubit in INSTR on the QVM.
  (measure qvm (quil:qubit-index (quil:measurement-qubit instr))))

(defmethod apply-measure-discard-to-state (qvm (state density-matrix-state) (instr quil:measure-discard))
  (let ((ρ (matrix-view (state qvm)))
        (q (quil:qubit-index (quil:measurement-qubit instr))))
    (dotimes (i (array-dimension ρ 0))
      (dotimes (j (array-dimension ρ 1))
        ;; Zeroing out the non-basis state projectors (the
        ;; off-diagonal projectors: |0><1| and |1><0|)
        (unless (logbitp q (logeqv i j))
          (setf (aref ρ i j) (cflonum 0))))))
  qvm)

(defmethod measure-all ((qvm base-qvm))
  (measure-all-state (state qvm) qvm))

(defgeneric measure-all-state (state qvm)
  (:documentation "The protocol for MEASURE-ALL differs by the STATE type of the QVM."))

(defmethod measure-all-state ((state pure-state) (qvm base-qvm))
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

(defmethod measure-all-state ((state density-matrix-state) (qvm base-qvm))
  (multiple-value-bind (qvm-ret measured-bits)
      (naive-measure-all qvm)
    (values
     qvm-ret
     measured-bits)))

;;; This is what the QAM does.
(defun naive-measure-all (qam)
  (let ((measured-bits nil))
    (loop :for q :from (1- (number-of-qubits qam)) :downto 0
          :do (multiple-value-bind (ret-qam bit)
                  (measure qam q)
                (push bit measured-bits)
                (setf qam ret-qam)))
    (values
     qam
     measured-bits)))

(defun sample-wavefunction-as-distribution-in-parallel (wf p)
  (sample-wavefunction-as-distribution wf
                                       (make-array 1
                                                   :initial-element p
                                                   :element-type 'flonum)))

(declaim (ftype (function (alexandria:non-negative-fixnum alexandria:non-negative-fixnum) alexandria:non-negative-fixnum) midpoint)
         (inline midpoint))
(defun midpoint (a b)
  "Find the midpoint of two non-negative fixnums A and B where A <= B."
  (declare (type alexandria:non-negative-fixnum a b)
           #.*optimize-dangerously-fast*)
  ;; avoid overflow by not doing (A+B)/2.
  (+ a (floor (the alexandria:non-negative-fixnum (- b a)) 2)))

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
    (declare (type alexandria:non-negative-fixnum min max)
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
                            (psum-dotimes (i (the alexandria:non-negative-fixnum (- mid min)))
                              (let ((i (the alexandria:non-negative-fixnum (+ i min))))
                                (probability (aref wf i)))))))
               (declare (type alexandria:non-negative-fixnum mid)
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


(defun perturb-measured-bits (qvm measured-bits readout-povms)
  "Randomly perturb the values of the bits in MEASURED-BITS in
accordance with any available readout POVMs on the QVM. Returns an
updated list of measured bits."
  ;; This models purely classical bit flips of the measurement record
  ;; which captures the reality of noisy low power dispersive
  ;; measurements of superconducting qubits very well. Here the
  ;; dominant source of error is misclassifying a readout signal due
  ;; to thermal noise that corrupts the signal on its return path out
  ;; of the cryostat.
  (loop :for i :below (number-of-qubits qvm)
        :for c :in measured-bits
        :collect (let ((povm (gethash i readout-povms)))
                   (if povm
                       (destructuring-bind (p00 p01 p10 p11) povm
                         (perturb-measurement c p00 p01 p10 p11))
                       c))))

(defun sample-wavefunction-multiple-times (wf num-samples)
  "Produce NUM-SAMPLES bitstring samples of the wavefunction WF according to its interpretation as a probability distribution."
  (declare #.*optimize-briskly*
           (type quantum-state wf)
           (type alexandria:non-negative-fixnum num-samples))
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
  "Implementation of SAMPLE-WAVEFUNCTION-AS-DISTRIBUTION-IN-PARALLEL-TRULY, un-parallelized,
 but for every probability in the PS (type: (VECTOR FLONUM)). Return a
 vector of sampled amplitude addresses."
  (declare #.*optimize-briskly*
           (type quantum-state wf)
           (type (vector flonum) ps))
  (let* ((cumsum (flonum 0))
         (len (length ps))
         (sampled-indices (make-array len :initial-element 0
                                          :element-type 'amplitude-address)))
    (declare (type flonum cumsum)
             (type alexandria:array-length len)
             (type (simple-array amplitude-address (*)) sampled-indices))
    (dotimes (idx (length wf) sampled-indices)
      (incf cumsum (probability (aref wf idx)))
      (loop :with i :of-type alexandria:array-index := 0
            :while (and (< i len) (< (aref ps i) cumsum))
            :do (setf (aref sampled-indices i) idx)
                (incf i)))))
