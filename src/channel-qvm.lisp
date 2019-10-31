;;;; channel-qvm.lisp
;;;;
;;;; Authors: Sophia Ponte, Nikolas Tezak, Erik Davis

(in-package #:qvm)

;;; Implements a QVM that can interpret a rule-based noise model and
;;; apply kraus operators to it's state.

(defgeneric apply-all-kraus-maps (qvm instr kraus-maps)
  (:documentation "Applies every kraus-map in the list KRAUS-MAPS to the state of the system."))


(defgeneric apply-kraus-map (qvm instr kraus-ops)
  (:documentation "Applies noise from a kraus map to the system. Randomly select a kraus operator from the kraus map KRAUS-OPS to apply to the state of the system."))


;;; The CHANNEL-QVM requires an explicitly defined NOISE-MODEL. This
;;; NOISE-MODEL should be a complete specification of the different
;;; types of noise that should be applied throughout the execution of
;;; a QUIL program, as well as when in the program's execution the
;;; noise should be applied. These decisions are made during
;;; TRANSITION. At each new instruction, the CHANNEL-QVM checks to see
;;; if any NOISE-RULES are matched by the instuction, and if so, the
;;; CHANNEL-QVM applied the corresponding noise.
(defclass channel-qvm (pure-state-qvm)
  ((original-amplitude-pointer
    :reader original-amplitude-pointer)
   (trial-amplitudes
    :accessor %trial-amplitudes)
   (noise-model
    :initarg :noise-model
    :accessor noise-model
    :initform nil))
  (:documentation "The CHANNEL-QVM is a QVM that supports a fully explicit NOISE-MODEL. The NOISE-MODEL is a list of NOISE-RULES, where each NOISE-RULE describes a type of noise and at which points in a program's execution that type of noise should be applied."))


(defmethod initialize-instance :after ((qvm channel-qvm) &rest args)
  ;; Initializes an instance of a CHANNEL-QVM"
  (declare (ignore args))
  (setf
   ;; Initialize the trial-amplitudes to an empty array of the correct
   ;; size
   (%trial-amplitudes qvm) (make-lisp-cflonum-vector (expt 2 (number-of-qubits qvm)))
   ;; Save a pointer to the originally provided memory
   (slot-value qvm 'original-amplitude-pointer) (amplitudes qvm)))


(defmethod run :after ((qvm channel-qvm))
  ;; Only copy if we really need to.
  (when (requires-swapping-p qvm)
    ;; Copy the correct amplitudes into place.
    (copy-wavefunction (amplitudes qvm) (original-amplitude-pointer qvm))
    ;; Get the pointer back in home position. We want to swap them,
    ;; not overwrite, because we want the scratch memory to be intact.
    (rotatef (amplitudes qvm) (%trial-amplitudes qvm))))


(defun requires-swapping-p (qvm)
  "Does the  QVM require swapping of internal pointers?"
  (and (not (eq (amplitudes qvm) (original-amplitude-pointer qvm)))
       #+sbcl (eq ':foreign (sb-introspect:allocation-information
                             (original-amplitude-pointer qvm)))))


(defmethod transition ((qvm channel-qvm) (instr quil:gate-application))
  ;; If any noise rules are matched by theinstruction INSTR, apply the
  ;; the kraus operators for that noise rule after applying the gate
  ;; for this instruction.
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
  "Check if rule is matched by instruction data and the position of the match request."
  (let* ((noise-predicate (predicate-function (noise-predicate rule)))
         (noise-position (noise-position (noise-predicate rule))))
    (and (funcall noise-predicate instr) (eq position noise-position))))


(defun find-matching-rule (rules instr position)
  "Return the first rule that matches INSTR/POSITION in the list RULES."
  (find-if (lambda (rule) (rule-matches-instr-p rule instr position)) rules))


(defmethod apply-all-kraus-maps ((qvm channel-qvm) (instr quil:gate-application) kraus-maps)
  ;; Apply all the kraus operators to the state of the system.
  (dolist (kraus-map kraus-maps)
    (apply-kraus-map qvm instr kraus-map)))


(defmethod apply-kraus-map ((qvm channel-qvm) (instr quil:gate-application) kraus-ops)
  ;; Randomly select one of the kraus operators in KRAUS-OPS to apply
  ;; for the transition.
  (let* ((amps (%trial-amplitudes qvm))
         (r (random 1.0d0))
         (summed 0.0d0)
         (logical-qubits (quil:application-arguments instr))
         (qubits (mapcar #'quil:qubit-index logical-qubits)))
    (check-type amps quantum-state)
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
  "Apply POVM to measurement."
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
  "Given the readout error encoded in the POVM (see documentation of NOISY-QVM), randomly sample the observed (potentially corrupted) measurement outcome."
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
