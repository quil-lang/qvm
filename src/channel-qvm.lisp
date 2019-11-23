;;;; channel-qvm.lisp
;;;;
;;;; Authors: Sophia Ponte, Nikolas Tezak, Erik Davis

(in-package #:qvm)

;;; General Overview:

;;; This file implements a CHANNEL-QVM which can interpret a rule-based
;;; NOISE-MODEL and apply kraus operators to its state. This
;;; NOISE-MODEL is a collection of NOISE-RULES that trigger
;;; applications of channels based on the syntactic properties of
;;; instructions in a CHANNEL-QVM program. Each NOISE-RULE contains a
;;; NOISE-PREDICATE and a list of OPERATION-ELEMENTS. The
;;; NOISE-PREDICATE describes how to match qvm instructions, and which
;;; instructions to match. The OPERATION-ELEMENTS are the physical
;;; description of the channel, represented as a list of kraus
;;; maps. At each qvm instruction, the OPERATION-ELEMENTS of a
;;; NOISE-RULE whose NOISE-PREDICATE matches that instruction is
;;; applied to the state of a the CHANNEL-QVM. Only one rule can be
;;; matched by an instruction. If multiple NOISE-RULES match the
;;; current instruction, the rule with the highest PRIORITY is
;;; applied.

;;; The CHANNEL-QVM supports both PURE-STATEs and
;;; DENSITY-MATRIX-STATEs. For a PURE-STATE STATE, the CHANNEL-QVM
;;; stochastically applies kraus operators by selecting one at
;;; random. Instead, if the CHANNEL-QVM uses a DENSITY-MATRIX-STATE,
;;; the kraus operators are converted to superoperators before they
;;; are applied to the density matrix.

(defgeneric apply-all-kraus-maps (qvm instr kraus-maps)
  (:documentation "Apply every kraus map in the list KRAUS-MAPS to the state of the system."))

(defgeneric apply-kraus-map (qvm instr kraus-ops)
  (:documentation "Apply noise from a kraus map KRAUS-OPS to the system. Randomly select a kraus operator from KRAUS-OPS using inverse transform sampling to apply to the STATE of the QVM system."))

;;; The CHANNEL-QVM requires an explicitly defined NOISE-MODEL. At the
;;; TRANSITION to each new instruction, the CHANNEL-QVM checks to see
;;; if any NOISE-RULES are matched by the instruction, and if so, the
;;; CHANNEL-QVM applies the corresponding noise defined by the
;;; NOISE-RULE.
(defclass channel-qvm (base-qvm)
  ((noise-model
    :initarg :noise-model
    :accessor noise-model))
  (:default-initargs
   :noise-model (make-noise-model ()))
  (:documentation "The CHANNEL-QVM is a QVM that supports a fully explicit NOISE-MODEL. The NOISE-MODEL is an explicit definition of where and how different channels should be applied to a program running in the CHANNEL-QVM."))

(defmethod initialize-instance :after ((qvm channel-qvm) &rest args)
  ;; Initializes an instance of a CHANNEL-QVM. If the STATE is not
  ;; specified, default to a PURE-STATE.
  (declare (ignore args))
  (when (or (not (slot-boundp qvm 'state))
            (null (slot-value qvm 'state)))
    (%set-state (make-pure-state (number-of-qubits qvm))
                qvm))
  ;; The the NOISE-MODEL is not empty, allocate space for
  ;; TRIAL-AMPLITUDES in a PURE-STATE state. (This method is not
  ;; defined for a DMS)
  (when (or (noise-rules (noise-model qvm))
            (plusp (hash-table-count (superoperator-definitions qvm))))
    (check-allocate-computation-space (state qvm))))

(defmethod transition :before ((qvm channel-qvm) (instr quil:gate-application))
  ;; Before applying the current instruction INSTR, check if any
  ;; NOISE-RULES match the current instruction, and if they do apply
  ;; the noise associated with the first matching NOISE-RULE.
  (let* ((noise-rules (noise-rules (noise-model qvm)))
         (first-matching-rule (find-matching-rule noise-rules instr ':before)))
    (when first-matching-rule
      (apply-all-kraus-maps qvm instr (operation-elements first-matching-rule)))
    qvm))

(defmethod transition :after ((qvm channel-qvm) (instr quil:gate-application))
  ;; After applying the current instruction INSTR, check if any
  ;; NOISE-RULES match the instruction. If there is a match, apply the
  ;; noise associated with the first matching NOISE-RULE.
  (let* ((noise-rules (noise-rules (noise-model qvm)))
         (first-matching-rule (find-matching-rule noise-rules instr ':after)))
    (when first-matching-rule
      (apply-all-kraus-maps qvm instr (operation-elements first-matching-rule)))
    qvm))

(defmethod transition :around ((qvm channel-qvm) (instr quil:measurement))
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))

(defmethod apply-all-kraus-maps ((qvm channel-qvm) (instr quil:gate-application) kraus-maps)
  ;; Apply each kraus map in the list KRAUS-MAPS to the state of the system, using the qubits from the current instruction INSTR.
  (let ((qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))
    (dolist (kraus-map kraus-maps)
      (apply-gate-state (convert-to-kraus-list kraus-map)
                        (state qvm)
                        qubits))))

(defun rule-matches-instr-p (rule instr position)
  "Check if RULE is matched by instruction data INSTR and the POSITION of the match request."
  (let* ((noise-predicate (predicate-function (noise-predicate rule)))
         (noise-position (noise-position (noise-predicate rule))))
    (and (funcall noise-predicate instr) (eq position noise-position))))

(defun find-matching-rule (rules instr position)
  "Return the first rule that matches INSTR/POSITION in the list RULES."
  (find-if (lambda (rule) (rule-matches-instr-p rule instr position)) rules))

(defun check-kraus-ops (kraus-ops)
  "Verify that a list KRAUS-OPS of Kraus operators given as MAGICL:MATRIX objects encodes a proper kraus map. In particular, we require that the Kraus operators are all of equal matrix dimension with equal numbers of rows and columns. Furthermore, to ensure that the Kraus map preserves trace, they must be normalized such that sum_{j=1}^n K_j^H K_j = I, where I is the identity matrix of equal dimensions."
  (let* ((m (magicl:matrix-rows (first kraus-ops)))
         (n (magicl:matrix-cols (first kraus-ops)))
         (kraus-sum (magicl:make-zero-matrix m n)))
    (assert (= m n) ((first kraus-ops)) "The Kraus operators be square matrices.")
    (loop :for k :in kraus-ops
          :do
             (assert (= m (magicl:matrix-rows k) (magicl:matrix-cols k))
                     (k)
                     "All Kraus operators must have matching dimensions")
             ;; This MAGICL provided BLAS:ZGEMM call effectively performs the following operation
             ;; KRAUS-SUM -> KRAUS-SUM + K^H . K
             (magicl.blas-cffi:%zgemm
              "C" "N" m m m
              (complex 1d0) (magicl::matrix-data k) m (magicl::matrix-data k) m
              (complex 1d0) (magicl::matrix-data kraus-sum) m))

    ;; Warning, if this consistently leads to assertion errors increase the
    ;; tolerance *DEFAULT-ZERO-COMPARISON-EPSILON*
    (let ((magicl::*default-zero-comparison-epsilon* 1d-5))
      (assert
       (magicl:identityp kraus-sum)
       (kraus-sum)
       "The Kraus map must preserve trace or equivalently this matrix ~
        ~S must be equal to the identity" 
       kraus-sum))) t)

;;; Measurement

(defmethod measure-all-state ((state pure-state) (qvm channel-qvm))
  ;; Measure all QUBITS in PURE-STATE state of the the CHANNEL-QVM QVM
  ;; and apply readout noise if necessary.
  (declare (ignore qvm))
  (multiple-value-bind (qvm-ret measured-bits)
      (call-next-method)
    (values
     qvm-ret
     (perturb-measured-bits qvm-ret measured-bits (readout-povms (noise-model qvm))))))

(defmethod measure-all-state ((state density-matrix-state) (qvm channel-qvm))
  ;; Measure all QUBITS in the DENSITY-MATRIX-STATE sttate of the
  ;; CHANNEL-WVM QVM and apply readout noise if necessary.
  (multiple-value-bind (qvm-ret measured-bits)
      (naive-measure-all qvm)
    (values
     qvm-ret
     (perturb-measured-bits qvm-ret measured-bits (readout-povms (noise-model qvm))))))

(defgeneric apply-classical-readout-noise (qvm instr)
  (:documentation "Given a QVM and a (measurement) instruction INSTR, corrupt the readout bit according to the POVM specifications of QVM.")
  ;; Ignore for a measure-discard
  (:method ((qvm channel-qvm) (instr quil:measure-discard))
    (declare (ignore qvm instr))
    nil)
  ;; Apply POVM from noise model to qvm
  (:method ((qvm channel-qvm) (instr quil:measure))
    (%corrupt-qvm-memory-with-povm qvm instr (readout-povms (noise-model qvm))))
  ;; Source instruction before application.
  (:method ((qvm channel-qvm) (instr compiled-measurement))
    (apply-classical-readout-noise qvm (source-instruction instr))))

(defun %corrupt-qvm-memory-with-povm (qvm instr povm-map)
  "Apply POVM-MAP to the measured result of the INSTR application."
  (check-type instr quil:measure)
  (let* ((q (quil:qubit-index (quil:measurement-qubit instr)))
         (a (quil:measure-address instr))
         (c (dereference-mref qvm a))
         (povm (gethash q povm-map)))
    (when povm
      (destructuring-bind (p00 p01 p10 p11) povm
        (setf (dereference-mref qvm a)
              (perturb-measurement c p00 p01 p10 p11))))))

(defun perturb-measurement (actual-outcome p00 p01 p10 p11)
  "Given the readout error encoded in the assignment probabilities P00, P01, P10, P11, randomly sample the observed (potentially corrupted) measurement outcome."
  (check-type actual-outcome bit)
  (check-type p00 (double-float 0.0d0 1.0d0))
  (check-type p01 (double-float 0.0d0 1.0d0))
  (check-type p10 (double-float 0.0d0 1.0d0))
  (check-type p11 (double-float 0.0d0 1.0d0))
  (let ((r (random 1.0d0)))
    (ecase actual-outcome
      ((0) (if (<= r p00) 0 1))
      ((1) (if (<= r p01) 0 1)))))

(defun check-povm (povm)
  "Verify that the list POVM contains a valid single qubit diagonal POVM, given as the list (p(0|0) p(0|1) p(1|0) p(1|1))."
  (destructuring-bind (p00 p01 p10 p11) povm
    (check-type p00 (double-float 0.0d0 1.0d0))
    (check-type p01 (double-float 0.0d0 1.0d0))
    (check-type p10 (double-float 0.0d0 1.0d0))
    (check-type p11 (double-float 0.0d0 1.0d0))
    (assert (cl-quil::double= 1.0d0 (+ p00 p10)))
    (assert (cl-quil::double= 1.0d0 (+ p01 p11)))))

;;; Don't compile things for the CHANNEL-QVM.
(defmethod compile-loaded-program ((qvm channel-qvm))
  qvm)

(defmethod compile-instruction ((qvm channel-qvm) isn)
  (declare (ignore qvm))
  isn)
