;;;; src/density-qvm.lisp
;;;;
;;;; Authors: Robert Smith
;;;;          Erik Davis

(in-package #:qvm)

;;; This file implements density matrix evolution.


;;; General Overview

;;; The general approach taken here is that the DENSITY-QVM inherits
;;; most of its behavior from the PURE-STATE-QVM, but provides more
;;; specific methods for a few operations (namely TRANSITION, MEASURE,
;;; and MEASURE-ALL). A density matrix ρ of n qubits is represented in
;;; a PURE-STATE-QVM as a 2n qubit vector of amplitudes, vec(ρ). On
;;; the other hand, the DENSITY-QVM also maintains a 2^n x 2^n array
;;; displaced to vec(ρ), for convenience. See below for the definition
;;; of vec(ρ).

;;; Another feature of our implementation is that support for noisy
;;; operations and measurements mirrors the interface presented by
;;; NOISY-QVM. In particular, the DENSITY-QVM presents
;;; NOISY-GATE-DEFINITIONS and READOUT-POVMS slots, and has a similar
;;; interface for updating these slots (through the SET-NOISY-GATE and
;;; SET-READOUT-POVM methods, respectively).


(defclass density-qvm (pure-state-qvm)
  ((matrix-view :reader density-matrix-view
                :initform nil
                :documentation "This is a 2D array displaced to the underlying QVM amplitudes.")
   (temporary-state :accessor temporary-state
                    :initform nil
                    :documentation "Sometimes the simulation needs to use some temporary state. We save it so we don't have to alloc/dealloc frequently.")
   (noisy-gate-definitions :initarg :noisy-gate-definitions
                           :accessor noisy-gate-definitions
                           :initform (make-hash-table :test 'equalp)
                           :documentation "Noisy gate definitions that, if present, override those stored in GATE-DEFINITIONS.")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table)
    :documentation "Noisy readout encoded as diagonal single qubit
    POVM given as a 4-element list

          (p(0|0) p(0|1)
           p(1|0) p(1|1))

which for each qubit gives the probability p(j|k) of measuring outcome
j given actual state k. Note that we model purely classical readout
error, i.e., the post measurement qubit state is always k, but the
recorded outcome j may be different."))
  (:documentation "A density matrix simulator."))


;;; Creation and Initialization

(defmethod initialize-instance :after ((qvm density-qvm) &rest args)
  (declare (ignore args))
  ;; PURE-STATE-QVM does its own allocation, which we don't want, so
  ;; here we make sure that the AMPLITUDES slot has a vector of the
  ;; right size (e.g. it was constructed by MAKE-DENSITY-QVM).
  (assert (and (slot-boundp qvm 'amplitudes)
               (typep (slot-value qvm 'amplitudes) 'quantum-state))
          ()
          "Density QVM cannot be initialized with AMPLITUDES unbound. Consider using MAKE-DENSITY-QVM, or explicitly providing the :AMPLITUDES initarg..")
  (let* ((num-qubits (number-of-qubits qvm))
         (dim (expt 2 num-qubits)))
    (assert (= (length (amplitudes qvm)) (expt dim 2))
            ()
            "Density QVM has AMPLITUDES slot initially bound to a vector of length ~D, but expected length ~D."
            (length (amplitudes qvm))
            (expt dim 2))
    (setf (slot-value qvm 'matrix-view)
          (make-array (list dim dim)
                      :element-type (array-element-type (amplitudes qvm))
                      :displaced-to (amplitudes qvm)))))


(defmethod reset-quantum-state ((qvm density-qvm))
  ;; It just so happens that the pure, zero state is the same in
  ;; this formalism, i.e., a 1 in the first entry.
  (bring-to-zero-state (amplitudes qvm))
  qvm)


(defun make-density-qvm (num-qubits &rest initargs &key allocation &allow-other-keys)
  ;; The amplitudes store vec(ρ), i.e. the entries of the density
  ;; matrix ρ in row-major order. For a system of N qubits, ρ has
  ;; dimension 2^N x 2^N, hence a total of 2^(2N) entries.

  ;; The initial state is the pure zero state, which is
  ;; represented by all zero entries except for a 1 in the first
  ;; position. See also RESET-QUANTUM-STATE, which we avoid
  ;; calling here because it performs an additional full traversal
  ;; of the vector.
  (let* ((expected-size (expt 2 (* 2 num-qubits)))
         ;; See also MAKE-QVM for this kind of code.
         (allocation
           (etypecase allocation
             (null
              (make-instance 'lisp-allocation :length expected-size))
             (string
              (make-instance 'posix-shared-memory-allocation :length expected-size
                                                             :name allocation))
             (t
              (assert (= (expt 2 (* 2 num-qubits)) (allocation-length allocation)))
              allocation)))
         (amplitudes (getf initargs ':amplitudes)))
    (multiple-value-bind (amps fin)
        (if (null amplitudes)
            (allocate-vector allocation)
            amplitudes)
      (check-type amps quantum-state)
      ;; Go into the zero state.
      (bring-to-zero-state amps)
      (setf (aref amps 0) (cflonum 1))
      (let ((density-qvm (apply #'make-instance 'density-qvm
                                :number-of-qubits num-qubits
                                :amplitudes amps
                                initargs)))
        (unless (null fin) (tg:finalize density-qvm fin))
        density-qvm))))

(defun full-density-number-of-qubits (vec-density)
  "Computes the number of qubits encoded by a vectorized density matrix."
  (1- (integer-length (isqrt (length vec-density)))))


;;; Superoperators

;;; Ordinary gates, as well as user-specified "Kraus operators", are
;;; represented by a SUPEROPERATOR type. The quil syntax for
;;; specifying Kraus operators is the same here as in the NOISY-QVM --
;;; namely, through pragmas a user may specify a "noisy gate" on a
;;; specific set of qubits, and during DENSITY-QVM evaluation such a
;;; noisy gate definition will replace the usual unitary one. The
;;; primary difference between the DENSITY-QVM and the NOISY-QVM in
;;; this regard is that application of a noisy gate in the DENSITY-QVM
;;; is completely deterministic and "folds all of the noisy" into the
;;; density matrix, whereas the NOISY-QVM is nondeterministic and
;;; tracks only a specific realization of the gate noise.


(adt:defdata superoperator
  "Representation of a linear operator on density operators."
  ;; Let ' mean † aka conjugate transpose.
  ;;
  ;; ρ ↦ U ρ U'
  (single-kraus quil:gate)
  ;; ρ ↦ ∑ᵢ Aᵢ ρ Aᵢ'
  (kraus-list list))

(defmethod set-noisy-gate ((qvm density-qvm) gate-name qubits kraus-ops)
  (check-kraus-ops kraus-ops)
  ;; Wrap a matrix in a gate in a superoperator...
  (setf (gethash (list gate-name qubits) (noisy-gate-definitions qvm))
        (kraus-list (mapcar #'lift-matrix-to-superoperator kraus-ops))))

(defmethod set-readout-povm ((qvm density-qvm) qubit povm)
  (check-povm povm)
  (setf (gethash qubit (readout-povms qvm)) povm)
  nil)

(defun lift-matrix-to-superoperator (mat)
  "Converts a magicl matrix MAT into a superoperator."
  (single-kraus
   (make-instance 'quil:simple-gate
                  :name (string (gensym "KRAUS-TEMP"))
                  :matrix mat)))

(defgeneric conjugate-entrywise (gate)
  (:documentation "Construct a new gate from GATE with corresponding matrix entries conjugated.")
  (:method ((gate quil:simple-gate))
    (make-instance 'quil:simple-gate
                   :name (concatenate 'string (quil:gate-name gate) "*")
                   :matrix (magicl:conjugate-entrywise (quil:gate-matrix gate))))
  (:method ((gate quil:permutation-gate))
    (make-instance 'quil:permutation-gate
                   :name (concatenate 'string (quil:gate-name gate) "*")
                   :permutation (quil:permutation-gate-permutation gate)))
  (:method ((gate quil:parameterized-gate))
    (make-instance 'quil:parameterized-gate
                   :name (concatenate 'string (quil:gate-name gate) "*")
                   :dimension (quil:gate-dimension gate)
                   :matrix-function #'(lambda (&rest parameters)
                                        (magicl:conjugate-entrywise
                                         (apply #'quil:gate-matrix gate parameters))))))


(defmethod apply-superoperator (sop vec-density qubits ghost-qubits &key temporary-storage params)
  "Apply a superoperator SOP to a vectorized density matrix
VEC-DENSITY, where QUBITS and GHOST-QUBITS are tuples of qubit indices
which the superoperator acts on (from the left and right
respectively). The computation may require TEMPORARY-STORAGE, a vector
of the same length as VEC-DENSITY. If no TEMPORARY-STORAGE is
provided, it will be allocated as needed. Returns the pair of updated
VEC-DENSITY and (perhaps freshly allocated) TEMPORARY-STORAGE."
  (check-type sop superoperator)
  ;; We use the following law to help our calculation:
  ;;
  ;;     vec(AρB) = (A ⊗ Bᵀ)vec(ρ)
  ;;
  ;; where vec(·) is row-major vectorization. The quantity vec(ρ) is
  ;; the VEC-DENSITY argument.
  (adt:match superoperator sop
    ((single-kraus U)
     ;; (U ⊗ U'ᵀ) = (U ⊗ U*), where * is entrywise conjugate.
     (let ((U* (conjugate-entrywise U)))
       (apply #'apply-gate U* vec-density qubits params)
       (apply #'apply-gate U  vec-density ghost-qubits params)
       (values vec-density temporary-storage)))
    ((kraus-list list)
     (cond
       ;; Empty. Just treat as identity.
       ((endp list)
        (values vec-density temporary-storage))
       ;; Degenerate case of just 1 superoperator.
       ((endp (rest list))
        (apply-superoperator (first list) vec-density qubits ghost-qubits
                             :temporary-storage temporary-storage
                             :params params))
       ;; General (and super expensive) case.
       (t
        ;; XXX FIXME: We could eliminate one copy if our APPLY-GATE
        ;; function could understand a source and destination array.
        (let ((pristine (copy-seq vec-density))
              (sum      (fill (or temporary-storage (copy-seq vec-density)) (cflonum 0))))
          (dolist (sub-sop list)
            ;; Apply the operator.
            (apply-superoperator sub-sop vec-density qubits ghost-qubits :params params)
            ;; Increment our running sum.
            (map-into sum #'+ sum vec-density)
            ;; Reset vec-density to a pristine state.
            ;;
            ;; XXX FIXME: Note that on the last loop, this is
            ;; wasteful!
            (replace vec-density pristine))
          ;; Replace our vec-density with our computed map.
          (replace vec-density sum)
          ;; Let pristine be wild and free for the GC to catch.
          (setf pristine nil)
          ;; Return our purchase, including temporary storage we've
          ;; allocated.
          (values vec-density sum)))))))


(defmethod transition ((qvm density-qvm) (instr quil:gate-application))
  (assert (typep (quil:application-operator instr) 'quil:named-operator) ; TODO XXX support gate modifiers
          (instr)
          "The density QVM doesn't support gate modifiers.")
  (let*  ((gate-name (quil::operator-description-name (quil:application-operator instr)))
          (gate (pull-teeth-to-get-a-gate instr))
          (params (mapcar (lambda (p) (force-parameter p qvm))
                          (quil:application-parameters instr)))
          (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
          (ghosts (mapcar (alexandria:curry #'+ (number-of-qubits qvm)) qubits))
          (sop    (or (gethash (list gate-name qubits)
                               (noisy-gate-definitions qvm))
                      (single-kraus gate))))

    (multiple-value-bind (new-density temp-storage)
        (apply-superoperator sop
                             (amplitudes qvm)
                             (apply #'nat-tuple qubits)
                             (apply #'nat-tuple ghosts)
                             :temporary-storage (temporary-state qvm)
                             :params params)
      (declare (ignore new-density))
      (setf (temporary-state qvm) temp-storage))

    (incf (pc qvm))
    qvm))


;;; Measurement

;;; In the PURE-STATE-QVM there is only one sensible meaning for a
;;; measurement: outcomes are sampled according to their respective
;;; probabilities, and then wavefunction collapse occurs. In the
;;; DENSITY-QVM, this could in principle be augmented by a second
;;; notion of measurement: namely, the outcome probabilities allow us
;;; to compute an "expected" outcome, which is generically a mixed
;;; state. The situation here is analogous to the question of gate
;;; noise, where one must choose between working with a specific
;;; realization of the noise process or its entire
;;; distribution. Whereas for noise we prefer the latter, for
;;; measurement we prefer the former, because i) it is necessary to
;;; force collapse when measurements are needed for classical control,
;;; and ii) it is what most people expect anyways.


(defun density-qvm-qubit-probability (qvm qubit)
  "The probability that the physical qubit addressed by QUBIT is 1."
  (check-type qvm density-qvm)
  (let ((vec-density (amplitudes qvm))
        (dim (expt 2 (number-of-qubits qvm))))
    ;; This is a sum along the diagonal of the DIM x DIM density matrix
    ;; Only indices with qubit in excited state contribute
    (psum-dotimes (k (half dim))
      (let ((i (index-to-address k qubit 1)))
        (realpart
         (aref vec-density (+ i (* i dim))) ; this is rho[i,i]
         )))))

(defun density-qvm-measurement-probabilities (qvm)
  "Computes the probability distribution of measurement outcomes (a vector)
  associated with the specified density matrix QVM.

  For example, if (NUMBER-OF-QUBITS QVM) is 2, then this will return a vector
  
  #(p[0,0] p[0,1] p[1,0] p[1,1]) 

  where p[i,j] denotes the probability that a simultaneous measurement of qubits 0,1
  results in the outcome i,j. 
  "
  (check-type qvm density-qvm)
  (let* ((vec-density (amplitudes qvm))
         (dim (expt 2 (number-of-qubits qvm)))
         (probabilities (make-array dim :element-type 'flonum :initial-element (flonum 0))))
    (loop :for i :below dim
          :do (setf (aref probabilities i)
                    (realpart
                     (aref vec-density (+ i (* i dim)))))
          :finally (return probabilities))))


(defun density-qvm-force-measurement (measured-value qubit qvm excited-probability)
  "Force the QVM to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the density matrix appropriately.

EXCITED-PROBABILITY should be the probability that QUBIT measured to |1>, regardless of what it's being forced as.
"
  (check-type qvm density-qvm)
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
         (num-qubits (number-of-qubits qvm))
         (vec-density (amplitudes qvm)))
    (pdotimes (k (length vec-density))
      ;; Check whether the row or column index refers to an annihilated state
      (if (or (= annihilated-state (ldb (byte 1 qubit) k))
              (= annihilated-state (ldb (byte 1 (+ qubit num-qubits)) ; in the above parlance, a "ghost" qubit
                                        k)))
          (setf (aref vec-density k) (cflonum 0))
          (setf (aref vec-density k)
                (* inv-norm (aref vec-density k)))))))


(defmethod measure ((qvm density-qvm) q)
  (let* ((r (random 1.0d0))
         (excited-probability (density-qvm-qubit-probability qvm q))
         (cbit (if (<= r excited-probability)
                   1
                   0)))
    ;; Force the non-deterministic measurement.
    (density-qvm-force-measurement cbit q qvm excited-probability)
    ;; Return the qvm.
    (values qvm cbit)))

(defmethod transition ((qvm density-qvm) (instr quil:measure))
  (call-next-method qvm instr)
  (corrupt-measurement-outcome qvm instr)
  qvm)

(defmethod transition ((qvm density-qvm) (instr quil:measure-discard))
  (let ((ρ (density-matrix-view qvm))
        (q (quil:qubit-index (quil:measurement-qubit instr))))
    (dotimes (i (array-dimension ρ 0))
      (dotimes (j (array-dimension ρ 1))
        ;; Zeroing out the non-basis state projectors (the
        ;; off-diagonal projectors: |0><1| and |1><0|)
        (unless (logbitp q (logeqv i j))
          (setf (aref ρ i j) (cflonum 0))))))
  (incf (pc qvm))
  qvm)

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

(defmethod measure-all ((qvm density-qvm))
  (multiple-value-bind (qvm-ret measured-bits)
      (naive-measure-all qvm)
    (values
     qvm-ret
     (perturb-measured-bits qvm-ret measured-bits))))

;;; Don't compile things for the density-qvm.
(defmethod compile-loaded-program ((qvm density-qvm))
  qvm)
