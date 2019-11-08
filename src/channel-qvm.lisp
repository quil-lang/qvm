;;;; channel-qvm.lisp
;;;;
;;;; Authors: Sophia Ponte, Nikolas Tezak, Erik Davis

(in-package #:qvm)

;;; Implements a QVM that can interpret a rule-based NOISE-MODEL and
;;; apply kraus operators to its state. This NOISE-MODEL is a
;;; collection of NOISE-RULES that trigger applications of channels
;;; based on the syntactic properties of instructions in a CHANNEL-QVM
;;; program. Each NOISE-RULE contains a NOISE-PREDICATE and a list of
;;; OPERATION-ELEMENTS. The NOISE-PREDICATE describes how to match qvm
;;; instructions, and which instructions to match. The
;;; OPERATION-ELEMENTS are the physical description of the channel,
;;; represented as a list of kraus maps. At each qvm instruction, the
;;; OPERATION-ELEMENTS of a NOISE-RULE whose NOISE-PREDICATE matches
;;; that instruction is applied to the state of a the
;;; CHANNEL-QVM. Only one rule can be matched by an instruction. If
;;; multiple NOISE-RULES match the current instruction, the rule with
;;; the highest PRIORITY is applied.

(defgeneric apply-all-kraus-maps (qvm instr kraus-maps)
  (:documentation "Applies every kraus map in the list KRAUS-MAPS to the state of the system."))

(defgeneric apply-kraus-map (qvm instr kraus-ops)
  (:documentation "Applies noise from a kraus map to the system. Randomly select a kraus operator from the kraus map KRAUS-OPS using inverse transform sampling to apply to the state of the system."))

;;; The CHANNEL-QVM requires an explicitly defined NOISE-MODEL. At the
;;; TRANSITION to each new instruction, the CHANNEL-QVM checks to see
;;; if any NOISE-RULES are matched by the instruction, and if so, the
;;; CHANNEL-QVM applies the corresponding noise defined by the
;;; NOISE-RULE.
(defclass channel-qvm (pure-state-qvm)
  ((original-amplitudes
    :reader original-amplitudes
    :documentation "A reference to the original pointer of amplitude memory, so the amplitudes can sit in the right place at the end of a computation.")
   (trial-amplitudes
    :accessor %trial-amplitudes
    :documentation "A second wavefunction used when applying a noisy quantum channel. Applying a Kraus map generally requires evaluating psi_j = K_j * psi for several different j, making it necessary to keep the original wavefunction around.  This value should be a QUANTUM-STATE whose size is compatible with the number of qubits of the CHANNEL-QVM. The actual values can be initialized in any way because they will be overwritten. As such, it merely is scratch space for intermediate computations, and hence should not be otherwise directly accessed.")
   (noise-model
    :initarg :noise-model
    :accessor noise-model))
  (:default-initargs
   :noise-model (make-noise-model nil))
  (:documentation "The CHANNEL-QVM is a QVM that supports a fully explicit NOISE-MODEL. The NOISE-MODEL is an explicit definition of where and how different channels should be applied to a program running in the CHANNEL-QVM."))

(defmethod initialize-instance :after ((qvm channel-qvm) &rest args)
  ;; Initializes an instance of a CHANNEL-QVM.
  (declare (ignore args))
  (check-type (noise-model qvm) noise-model)
  (setf
   ;; Initialize the TRIAL-AMPLITUDES to an empty array of the correct
   ;; size
   (%trial-amplitudes qvm) (make-lisp-cflonum-vector (expt 2 (number-of-qubits qvm)))
   ;; Save a pointer to the originally provided memory
   (slot-value qvm 'original-amplitudes) (amplitudes qvm)))

(defmethod run :after ((qvm channel-qvm))
  ;; Only copy if we really need to.
  (when (requires-swapping-p qvm)
    ;; Copy the correct amplitudes into place.
    (copy-wavefunction (amplitudes qvm) (original-amplitudes qvm))
    ;; Get the pointer back in home position. We want to swap them,
    ;; not overwrite, because we want the scratch memory to be intact.
    (rotatef (amplitudes qvm) (%trial-amplitudes qvm))))

(defun requires-swapping-p (qvm)
  "Does the  QVM require swapping of internal pointers?"
  (and (not (eq (amplitudes qvm) (original-amplitudes qvm)))
       #+sbcl (eq ':foreign (sb-introspect:allocation-information
                             (original-amplitudes qvm)))))

(defmethod transition ((qvm channel-qvm) (instr quil:gate-application))
  ;; If any NOISE-RULES are matched by INSTR, apply the
  ;; the kraus operators for that rule after applying the current INSTR.
  (let* ((gate (pull-teeth-to-get-a-gate instr))
         (params (mapcar (lambda (p) (force-parameter p qvm))
                         (quil:application-parameters instr)))
         (instr-qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (noise-rules (noise-rules (noise-model qvm)))
         (prepend-matched-rule (find-matching-rule noise-rules instr ':before))
         (append-matched-rule (find-matching-rule noise-rules instr ':after)))
    (when prepend-matched-rule
      (apply-all-kraus-maps qvm instr (operation-elements prepend-matched-rule)))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple instr-qubits) params)
    (when append-matched-rule
      (apply-all-kraus-maps qvm instr (operation-elements append-matched-rule)))
    (incf (pc qvm))
    qvm))

(defmethod transition :around ((qvm channel-qvm) (instr quil:measurement))
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))

(defun rule-matches-instr-p (rule instr position)
  "Check if RULE is matched by instruction data INSTR and the POSITION of the match request."
  (let* ((noise-predicate (predicate-function (noise-predicate rule)))
         (noise-position (noise-position (noise-predicate rule))))
    (and (funcall noise-predicate instr) (eq position noise-position))))

(defun find-matching-rule (rules instr position)
  "Return the first rule that matches INSTR/POSITION in the list RULES."
  (find-if (lambda (rule) (rule-matches-instr-p rule instr position)) rules))

(defmethod apply-all-kraus-maps ((qvm channel-qvm) (instr quil:gate-application) kraus-maps)
  ;; Apply each kraus map in the list KRAUS-MAPS to the state of the system.
  (dolist (kraus-map kraus-maps)
    (apply-kraus-map qvm instr kraus-map)))

(defmethod apply-kraus-map ((qvm channel-qvm) (instr quil:gate-application) kraus-ops)
  ;; Uniformly at random select one of the kraus operators in KRAUS-OPS to apply
  ;; for the transition.
  (let* ((amps (%trial-amplitudes qvm))
         (r (random 1.0d0))
         (summed 0.0d0)
         (logical-qubits (quil:application-arguments instr))
         (qubits (mapcar #'quil:qubit-index logical-qubits)))
    (declare (type quantum-state amps))
    ;; Randomly select one of the Kraus operators by inverse transform
    ;; sampling (cf [1]): We divide the unit interval [0,1] into n
    ;; bins where the j-th bin size equals the probability p_j with
    ;; which the j-th Kraus operator k_j should be applied. We know
    ;; that the Kraus operators are normalized such that p_j =
    ;; <psi|k_j^H k_j |psi> where x^H denotes hermitian conjugation of
    ;; x and can therefore perform this sampling lazily: First
    ;; generate a uniformly sampled random number r in [0,1]. Next,
    ;; find j such that
    ;;
    ;;       sum_{k=1}^{j-1} p_k < r <= sum_{k=1}^{j} p_k
    ;;
    ;; This is possible by evaluating only all p_k for k<=j. Then
    ;; pick this j as the choice of Kraus operator to apply.
    ;;
    ;; [1]: https://en.wikipedia.org/wiki/Inverse_transform_sampling
    (loop :for kj :in kraus-ops
          :do
             (replace amps (amplitudes qvm))
             (apply-matrix-operator (magicl-matrix-to-quantum-operator kj)
                                    amps
                                    (apply #'nat-tuple qubits))
             (incf summed (psum #'probability amps))
          :until (>= summed r))
    (rotatef (amplitudes qvm) (%trial-amplitudes qvm))
    (normalize-wavefunction (amplitudes qvm))))

(defmethod apply-classical-readout-noise ((qvm channel-qvm) (instr quil:measure-discard))
  (declare (ignore qvm instr))
  nil)

(defmethod apply-classical-readout-noise ((qvm channel-qvm) (instr quil:measure))
  (%corrupt-qvm-memory-with-povm qvm instr (readout-povms (noise-model qvm))))

(defmethod apply-classical-readout-noise ((qvm channel-qvm) (instr compiled-measurement))
  (apply-classical-readout-noise qvm (source-instruction instr)))

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
  "Verify that the list POVM contains a valid single qubit diagonal POVM. Also see the documentation for the READOUT-POVMS slot of NOISY-QVM."
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
