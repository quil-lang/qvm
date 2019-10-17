(in-package #:qvm)

;;; A noise predicate is a function that takes an instruction's gate
;;; and qubits, and returns either true if the instruction is a match,
;;; else false. The priority indicates the priority of the predicate
;;; (tbd) and the noise-position indicates whether the predicate
;;; should match before or after an instruction.
(defclass noise-pred ()
  ((predicate
    :initarg :predicate
    :accessor predicate
    :initform nil
    :documentation "Does the instruction trigger the application of a channel?")
   (priority
    :initarg :priority
    :accessor priority
    :initform 1 ; Default priority is 1
    :documentation "For ordering when checking for an instruction's  matching predicates.")
   (noise-position
    :initarg :noise-position
    :accessor noise-position
    :initform nil
    :type noise-pos
    :documentation "Should the application of a channel happen before or after the instruciton?"))
  (:documentation "A NOISE-PREDICATe describes a collection of program instructions. When used in a NOISE-RULE, the NOISE-PREDICATE describes which instructions should be matched by the noise described in the OPERATION-ELEMENTS."))


(deftype noise-pos ()
  "Describes the 'position' of a noise rule relative to an instruction. Should the noise be applied BEFORE or AFTER the instruction is executed? " 
  '(member :before :after))


(defmethod initialize-instance :after ((pred noise-pred) &rest args)
  ;; Assert that the NOISE-POSITION of the predicate is of type
  ;; NOISE-POS, and that PRIORITY is of type INTEGER
  (declare (ignore args))
  (check-type (noise-position pred) noise-pos)
  (check-type (priority pred) integer))


(defmethod make-noise-pred (predicate priority noise-position)
  ;; Creates a NOISE-PRED with the given PREDICATE (callable),
  ;; PRIORITY, and NOISE-POSITION
  (make-instance 'noise-pred :predicate predicate :priority priority :noise-position noise-position))


(defmethod predicate-and (np1 np2)
  ;; Logical AND of 2 noise predicates. The noise position is taken
  ;; from NP1 and the priority is taken to be the min (smaller in
  ;; value) priority between the two predicates.
  (let* ((conjunction (alexandria:conjoin (predicate np1) (predicate np2)))
         (priority (min (priority np1) (priority np2)))
         (noise-position (noise-position np1)))
    (make-noise-pred conjunction priority noise-position)))


(defmethod predicate-or (np1 np2)
  ;; Logical OR of 2 noise predicates. The noise position is taken
  ;; from NP1 and the priority is taken to be the min (smaller in
  ;; value) priority between the two predicates.
  (let* ((disjunction (alexandria:disjoin (predicate np1) (predicate np2)))
         (priority (min (priority np1) (priority np2)))
         (noise-position (noise-position np1)))
    (make-noise-pred disjunction priority noise-position)))


(defmethod predicate-not (np)
  ;; Logical NOT of a noise predicate
  (make-noise-pred (complement (predicate np)) (priority np) (noise-position np)))


;;; A noise rule consists of a noise predicate and a list of operation
;;; elements. The operation elements describe the noise as kraus
;;; operators, and the noise-predicate is a function that takes an
;;; instruction and returns true if the operation-elements should be
;;; applied after the instruction.
(defclass noise-rule ()
  ((noise-predicate
    :initarg :noise-predicate
    :accessor noise-predicate
    :initform nil
    :documentation "A function that is true when an instruciton is matched, false otherwise")
   (operation-elements
    :initarg :operation-elements
    :accessor operation-elements
    :initform ()
    :documentation "Operation elements (kraus operators) to apply when the predicate is satisfied"))
  (:documentation "A noise rule consists of noise data (OPERATION-ELEMENTS) and a specificiation of where the noise data should be applied to the program (NOISE-PREDICATE)."))


(defun min-rule-priority-p (r1 r2)
  "Comparator function for the priority of 2 noise rules. "
  (<= (priority (noise-predicate r1)) (priority (noise-predicate r2))))


(defun make-noise-rule (predicate operation-elements)
  "Returns a noise rule with the specified PREDICATE and OPERATION-ELEMENTS. "
  (make-instance 'noise-rule :noise-predicate predicate
                             :operation-elements operation-elements))


;;; A noise model consists of a set of noise rules and readout-povms.
;;; The noise rules define quantum channels and where they should be
;;; applied, and the readout-povms describe the noisy readout
;;; probabilities.
(defclass noise-model ()
  ((noise-rules
    :initarg :noise-rules
    :accessor noise-rules
    :initform nil
    :documentation "A list of noise-rules (predicate channel pairs)")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table :test 'eql)
    :documentation "A map of qubit to readout noise assignment probabilities. These are treated differently from noise channels (for now)."))
  (:documentation "A NOISE-MODEL is a collection of NOISE-RULES, describing what type of noise should be applied to a program that goes through a qvm, and where that noise should be applied."))


(defmethod initialize-instance :after ((nm noise-model) &rest args)
  ;; Ensure the noise rules are sorted according to priority order
  (declare (ignore args))
  (let ((rules (noise-rules nm)))
    (setf (slot-value nm 'noise-rules) (sort rules #'min-rule-priority-p))))


(defun (setf qubit-povm) (povm nm qubit)
  "Set the povm for a qubit in this noise model."
  (check-povm povm)
  (setf (gethash qubit (readout-povms nm)) povm))


(defun make-noise-model (noise-rules)
  "Returns a noise model with noise-rules."
  (make-instance 'noise-model :noise-rules noise-rules))


(defmethod add-noise-models (nm1 nm2)
  ;; Combines two noise models by concatenating their rules. This way,
  ;; either rule can be matched by an instruction.
  (let* ((rules (append (noise-rules nm1) (noise-rules nm2)))
         (sorted-rules (sort rules #'min-rule-priority-p)))
    (make-noise-model sorted-rules)))


(defmethod multiply-noise-models (nm1 nm2)
  ;; Combines two noise models in a way such that both set of rules,
  ;; or one and not the other could be matched by an instruction.
  (let ((rules (loop :for rule1 in (noise-rules nm1)
                     :append (loop :for rule2 in (noise-rules nm2)
                                   :collect (make-noise-rule
                                             (predicate-and (noise-predicate rule1) 
                                                            (noise-predicate rule2)) 
                                             (list (operation-elements rule1)
                                                   (operation-elements rule2)))
                                   :collect (make-noise-rule
                                             (predicate-and
                                              (noise-predicate rule1)
                                              (predicate-not (noise-predicate rule2))) 
                                             (operation-elements rule1))
                                   :collect (make-noise-rule
                                             (predicate-and
                                              (predicate-not (noise-predicate rule1)) 
                                              (noise-predicate rule2))
                                             (operation-elements rule2))))))

    (make-noise-model (sort rules #'min-rule-priority-p))))


(defun match-strict-qubits (&rest qubits)
  "The returned function is true if QUBITS exactly equals the instruction's qubits."
  (lambda (instr)
    (and (typep instr 'quil:gate-application)
         (equal qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))))


(defun match-any-n-qubits (n qubit-list)
  "The returned function is true if there is any intersection between the instruction's qubits and the QUBIT-LIST for an N-qubit operation. We need to specify N in the rule because a 2 qubit gate CNOT 0 1 could match a rule with qubits that has operation elements for a 1q gate. We want to prevent this, so we require the user to specify the number of qubits expected in the gate."
  (lambda (instr) (and (typep instr 'quil:gate-application)
                       (= n (list-length (mapcar #'quil:qubit-index (quil:application-arguments instr))))
                       (intersection qubit-list (mapcar #'quil:qubit-index (quil:application-arguments instr))))))


(defun match-strict-gate (gate)
  "The returned function is true if the instruciton's gate is exactly equal to GATE."
  (lambda (instr) (and (typep instr 'quil:gate-application)
                       (string= gate (cl-quil::application-operator-root-name instr)))))


(defun match-any-gates (&rest gates)
  "The returned function is true if there is any intersection between the instruction's gates and GATES."
  (lambda (instr) (and (typep instr 'quil:gate-application)
                       (member (cl-quil::application-operator-root-name instr) gates :test #'string=))))

(defun match-all-nq-gates (n)
  "The returned function is true if the instruction operates on N qubits."
  (lambda (instr) (and (typep instr 'quil:gate-application)
                       (= n (length (mapcar #'quil:qubit-index (quil:application-arguments instr)))))))

(defun match-all-gates ()
  "The returned function is true if the instruction contains a gate (not a measure)."
  (lambda (instr) (typep instr 'quil:gate-application)))


(defun match-measure ()
  "The returned function is true if the instruction is a MEASURE."
  (lambda (instr) (typep instr 'quil:measurement)))


(defun match-measure-at-strict (qubit)
  "The returned function is true if the instruciton is a measure on the specified QUBIT."
  (lambda (instr) (and (typep instr 'quil:measurement)
                       (= qubit (quil:qubit-index (quil:measurement-qubit instr))))))


(defun match-measure-at-any (&rest qubits)
  "The returned function is true if the instruciton is a measure on any of the specified QUBITS."
  (lambda (instr) (and (typep instr 'quil:measurement)
                       (member (quil:qubit-index (quil:measurement-qubit instr)) qubits))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric apply-all-kraus-operators (qvm instr kraus-maps)
  (:documentation "Applies every kraus-map in the list KRAUS-MAPS to the state of the system."))


(defgeneric apply-kraus-ops (qvm instr kraus-ops)
  (:documentation "Applies noise from a kraus map to the system. Randomly select a kraus operator from the kraus map KRAUS-OPS to apply to the state of the system."))


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


(defun requires-swapping-p (qvm)
  "Does the noisy qvm QVM require swapping of internal pointers?"
  (and (not (eq (amplitudes qvm) (original-amplitude-pointer qvm)))
       #+sbcl (eq ':foreign (sb-introspect:allocation-information
                             (original-amplitude-pointer qvm)))))


(defmethod run :after ((qvm channel-qvm))
  ;; Only copy if we really need to.
  (when (requires-swapping-p qvm)
    ;; Copy the correct amplitudes into place.
    (copy-wavefunction (amplitudes qvm) (original-amplitude-pointer qvm))
    ;; Get the pointer back in home position. We want to swap them,
    ;; not overwrite, because we want the scratch memory to be intact.
    (rotatef (amplitudes qvm) (%trial-amplitudes qvm))))



(defmethod transition ((qvm channel-qvm) (instr quil:gate-application))
  ;; If any noise rules are matched by theinstruction INSTR, apply the
  ;; the kraus operators for that noise rule after applying the gate
  ;; for this instruction.
  (let* ((gate   (pull-teeth-to-get-a-gate instr))
         (params (mapcar #'(lambda (p) (force-parameter p qvm))
                         (quil:application-parameters instr)))
         (instr-qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (noise-rules (noise-rules (noise-model qvm)))
         (prepend-kraus-maps (match-rules noise-rules instr :before))
         (append-kraus-maps (match-rules noise-rules instr :after)))
    (when prepend-kraus-maps (apply-all-kraus-operators qvm instr prepend-kraus-maps))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple instr-qubits) params)
    (when append-kraus-maps (apply-all-kraus-operators qvm instr append-kraus-maps))
    (incf (pc qvm))
    qvm))


(defmethod transition :around ((qvm channel-qvm) (instr quil:measurement))
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))


(defun rule-matches-instr-p (rule instr position)
  "Check if rule is matched by instruction data and the position of the match request."
  (let* ((noise-predicate (predicate (noise-predicate rule)))
         (noise-position (noise-position (noise-predicate rule))))
    (and (funcall noise-predicate instr) (string= position noise-position))))


(defun match-rules (rules instr position)
  "Return the operation elements for the first matching rule."
  (loop :for rule in rules
        :when (rule-matches-instr-p rule instr position)
          :return (operation-elements rule)))


(defmethod apply-all-kraus-operators ((qvm channel-qvm) (instr quil:gate-application) kraus-maps)
  ;; Apply all the kraus operators to the state of the system.
  (loop :for kraus-map :in kraus-maps
        :do
           (check-kraus-ops kraus-map)
           (apply-kraus-ops qvm instr kraus-map)))


(defmethod apply-kraus-ops ((qvm channel-qvm) (instr quil:gate-application) kraus-ops)
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
  (%corrupt-readout qvm instr (readout-povms (noise-model qvm))))


(defmethod apply-classical-readout-noise ((qvm channel-qvm) (instr compiled-measurement))
  (apply-classical-readout-noise qvm (source-instruction instr)))


(defun %corrupt-readout (qvm instr povm-map)
  (check-type instr quil:measure)
  (let* ((q (quil:qubit-index (quil:measurement-qubit instr)))
         (a (quil:measure-address instr))
         (c (dereference-mref qvm a))
         (povm (gethash q povm-map)))
    (when povm
      (destructuring-bind (p00 p01 p10 p11) povm
        (setf (dereference-mref qvm a)
              (perturb-measurement c p00 p01 p10 p11))))))


;; Duplicate function in noisy-qvm.lisp
(defun perturb-measurement (actual-outcome p00 p01 p10 p11)
  "Given the readout error encoded in the POVM (see documentation of NOISY-QVM)
randomly sample the observed (potentially corrupted) measurement outcome."
  (check-type actual-outcome bit)
  (check-type p00 (double-float 0.0d0 1.0d0))
  (check-type p01 (double-float 0.0d0 1.0d0))
  (check-type p10 (double-float 0.0d0 1.0d0))
  (check-type p11 (double-float 0.0d0 1.0d0))
  (let ((r (random 1.0d0)))
    (ecase actual-outcome
      ((0) (if (<= r p00) 0 1))
      ((1) (if (<= r p01) 0 1)))))


;; Duplicate function in noisy-qvm.lisp
(defun check-povm (povm)
  "Verify that the list POVM contains a valid single qubit diagonal POVM.
Also see the documentation for the READOUT-POVMS slot of NOISY-QVM."
  (destructuring-bind (p00 p01 p10 p11) povm
    (check-type p00 (double-float 0.0d0 1.0d0))
    (check-type p01 (double-float 0.0d0 1.0d0))
    (check-type p10 (double-float 0.0d0 1.0d0))
    (check-type p11 (double-float 0.0d0 1.0d0))
    (assert (cl-quil::double= 1.0d0 (+ p00 p10)))
    (assert (cl-quil::double= 1.0d0 (+ p01 p11)))))
