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

(deftype density-operator-matrix-view ()
  "The matrix view of a density operator."
  `(and (array cflonum (* *))
        (not simple-array)))

(defclass density-qvm (pure-state-qvm)
  ((state :accessor state
          :initarg :state)
   (noisy-gate-definitions :initarg :noisy-gate-definitions
                           :accessor noisy-gate-definitions
                           :initform (make-hash-table :test 'equalp))
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table)))
  (:documentation "A density matrix simulator."))


;;; Creation and Initialization

(defmethod amplitudes ((qvm density-qvm))
  (amplitudes (state qvm)))

(defmethod (setf amplitudes) (new-amplitudes qvm)
  (setf (amplitudes (state qvm)) new-amplitudes))

(defmethod temporary-state ((qvm density-qvm))
  (temporary-state (state qvm)))

(defmethod (setf temporary-state) (temp-storage (qvm density-qvm))
  (setf (temporary-state (state qvm)) temp-storage))

(defmethod density-matrix-view ((qvm density-qvm))
  (matrix-view (state qvm)))


(defmethod initialize-instance :after ((qvm density-qvm) &rest args)
  (declare (ignore args))
  ;; PURE-STATE-QVM does its own allocation, which we don't want, so
  ;; here we make sure that the AMPLITUDES slot has a vector of the
  ;; right size (e.g. it was constructed by MAKE-DENSITY-QVM).
  (when (or (not (slot-boundp qvm 'state))
            (null (slot-value qvm 'state)))
      (setf (state qvm) (make-instance 'density-matrix-state :num-qubits (number-of-qubits qvm)))
      (set-to-zero-state (state qvm))))


(defun make-density-qvm (num-qubits &key (allocation nil) &allow-other-keys)
  (make-instance 'density-qvm :number-of-qubits num-qubits 
                              :state (make-density-matrix-state 
                                      num-qubits  
                                      :allocation allocation)))


(defmethod reset-quantum-state ((qvm density-qvm))
  ;; It just so happens that the pure, zero state is the same in
  ;; this formalism, i.e., a 1 in the first entry.
  (bring-to-zero-state (amplitudes qvm))
  qvm)

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


(defmethod transition ((qvm density-qvm) (instr quil:gate-application))
  (assert (typep (quil:application-operator instr) 'quil:named-operator) ; TODO XXX support gate modifiers
          (instr)
          "The density QVM doesn't support gate modifiers.")
  (let*  ((gate (pull-teeth-to-get-a-gate instr))
          (params (mapcar (lambda (p) (force-parameter p qvm))
                          (quil:application-parameters instr)))
          (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))
    (apply #'apply-gate-state gate (state qvm) qubits params)
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

(defun density-qvm-measurement-probabilities (qvm)
  "Computes the probability distribution of measurement outcomes (a vector)
  associated with the specified density matrix QVM.

  For example, if (NUMBER-OF-QUBITS QVM) is 2, then this will return a vector
  
  #(p[0,0] p[0,1] p[1,0] p[1,1]) 

  where p[i,j] denotes the probability that a simultaneous measurement of qubits 0,1
  results in the outcome i,j. 
  "
  (check-type qvm density-qvm)
  (let* ((vec-density (amplitudes (state qvm)))
         (dim (expt 2 (number-of-qubits qvm)))
         (probabilities (make-array dim :element-type 'flonum :initial-element (flonum 0))))
    (loop :for i :below dim
          :do (setf (aref probabilities i)
                    (realpart
                     (aref vec-density (+ i (* i dim)))))
          :finally (return probabilities))))


(defun density-state-force-measurement (measured-value qubit state excited-probability)
  "Force the QVM to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the density matrix appropriately.

EXCITED-PROBABILITY should be the probability that QUBIT measured to |1>, regardless of what it's being forced as.
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
         (vec-density (amplitudes state)))
    (pdotimes (k (length vec-density))
      ;; Check whether the row or column index refers to an annihilated state
      (if (or (= annihilated-state (ldb (byte 1 qubit) k))
              (= annihilated-state (ldb (byte 1 (+ qubit num-qubits)) ; in the above parlance, a "ghost" qubit
                                        k)))
          (setf (aref vec-density k) (cflonum 0))
          (setf (aref vec-density k)
                (* inv-norm (aref vec-density k)))))))

(defmethod apply-classical-readout-noise ((qvm density-qvm) (instr quil:measure))
  (%corrupt-qvm-memory-with-povm qvm instr (readout-povms qvm)))

(defmethod transition :around ((qvm density-qvm) (instr quil:measurement))
  ;; perform actual measurement
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))


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


;;; Don't compile things for the density-qvm.
(defmethod compile-loaded-program ((qvm density-qvm))
  qvm)

;;; TODO: FIXME: we should be able to compile density operator stuff
;;; just fine.
(defmethod compile-instruction ((qvm density-qvm) isn)
  (declare (ignore qvm))
  isn)
