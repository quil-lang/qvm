(in-package #:qvm)

;;; A noise predicate is a function that takes an instruction's gate
;;; and qubits, and returns either true if the instruction is a match,
;;; else false. The priority indicates the priority of the predicate
;;; (tbd) and the noise-position indicates whether the predicate
;;; should match before or after an instruction.
(defclass noise-pred ()
  ((predicate
    :initarg :predicate
    :reader predicate
    :initform nil
    :documentation "Does the instruction trigger the application of a channel?")
   (priority
    :initarg :priority
    :reader priority
    :initform 10 ; Default priority is 10
    :documentation "For ordering when checking for an instruction's  matching predicates. The convention we use is that more specific predicates get higher priorities.")
   (noise-position
    :initarg :noise-position
    :reader noise-position
    :initform nil
    :type noise-pos
    :documentation "Should application of a channel happen before or after the instruction?"))
  (:documentation "A NOISE-PREDICATe describes a collection of program instructions. When used in a NOISE-RULE, the NOISE-PREDICATE describes which instructions should be matched by the noise described in the OPERATION-ELEMENTS."))

(defconstant +MAX-PRIORITY+ 20)
(defconstant +MIN-PRIORITY+ 0)


(deftype noise-pos ()
  "Describes the position of a noise rule relative to an instruction. Should the noise be applied BEFORE or AFTER the instruction is executed?" 
  '(member :before :after))


(defmethod initialize-instance :after ((pred noise-pred) &rest args)
  ;; Assert that the NOISE-POSITION of the predicate is of type
  ;; NOISE-POS, and that PRIORITY is of type INTEGER
  (declare (ignore args))
  (assert (typep (noise-position pred) 'noise-pos))
  (assert (typep  (priority pred) 'integer))
  (let ((pr (priority pred)))
    (assert (<= +MIN-PRIORITY+ pr +MAX-PRIORITY+) (pr)
            "PRIORITY of a NOISE-PREDICATE must be between MIN-PRIORITY and MAX-PRIORITY")))


(defun make-noise-pred (predicate priority noise-position)
  "Creates a NOISE-PRED with the given PREDICATE (function designator), PRIORITY, and NOISE-POSITION."
  (make-instance 'noise-pred :predicate predicate 
                             :priority priority 
                             :noise-position noise-position))


(defun predicate-and (np1 np2)
  "Logical AND of 2 noise predicates. The NOISE-POSITION is taken from NP1 and the priority is taken to be the min (smaller in value) priority between the two predicates."
  (let* ((conjunction (alexandria:conjoin (predicate np1) (predicate np2)))
         (priority (max (priority np1) (priority np2)))
         (noise-position (noise-position np1)))
    (make-noise-pred conjunction priority noise-position)))


(defun predicate-or (np1 np2)
  "Logical OR of 2 noise predicates. The NOISE-POSITION is taken from NP1 and the priority is taken to be the min (smaller in value) priority between the two predicates."
  (let* ((disjunction (alexandria:disjoin (predicate np1) (predicate np2)))
         (priority (max (priority np1) (priority np2)))
         (noise-position (noise-position np1)))
    (make-noise-pred disjunction priority noise-position)))


(defmethod predicate-not (np)
  "Logical NOT of a NOISE-PREDICATE NP. The PRIORITY of the predicate is 'inverted', by taking the difference between it and +MAX-PRIORITY+."
  (make-noise-pred (complement (predicate np)) (- +MAX-PRIORITY+ (priority np)) (noise-position np)))


;;; A noise rule consists of a noise predicate and a list of operation
;;; elements. The operation elements describe the noise as kraus
;;; operators, and the noise-predicate is a function that takes an
;;; instruction and returns true if the operation-elements should be
;;; applied after the instruction.
(defclass noise-rule ()
  ((noise-predicate
    :initarg :noise-predicate
    :accessor noise-predicate
    :documentation "A function that is true when an instruciton is matched, false otherwise.")
   (operation-elements
    :initarg :operation-elements
    :reader operation-elements
    :documentation "Operation elements to apply when the predicate is satisfied. This should be a list of kraus maps. Each kraus map is a list of kraus operators."))
  (:documentation "A noise rule consists of noise data (OPERATION-ELEMENTS) and a specificiation of where the noise data should be applied to the program (NOISE-PREDICATE)."))


(defun rule-priority>= (r1 r2)
  "Comparator function for the priority of 2 noise rules. Returns true if the priority of R1 is <= to the priority of R2."
  (>= (priority (noise-predicate r1)) (priority (noise-predicate r2))))


(defun make-noise-rule (predicate &rest kraus-maps)
  "Returns a noise rule with the specified PREDICATE and KRAUS-MAPS. Check the kraus ops in each kraus map in KRAUS-MAPS. KRAUS-MAPS is a list of kraus maps."
  (mapc #'check-kraus-ops kraus-maps)
  (make-instance 'noise-rule :noise-predicate predicate
                             :operation-elements kraus-maps))


;;; A noise model consists of a set of noise rules and readout-povms.
;;; The noise rules define quantum channels and where they should be
;;; applied, and the readout-povms describe the noisy readout
;;; probabilities.
(defclass noise-model ()
  ((noise-rules
    :initarg :noise-rules
    :accessor noise-rules
    :initform nil
    :documentation "A list of noise-rules. Each NOISE-RULE is a NOISE-PREDICATE and a list of kraus maps.")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table :test 'eql)
    :documentation "A map of qubit to readout noise assignment probabilities."))
  (:documentation "A NOISE-MODEL is a collection of NOISE-RULES, describing what type of noise should be applied to a program that goes through a qvm, and where that noise should be applied."))


(defmethod initialize-instance :after ((nm noise-model) &rest args)
  ;; Ensure the noise rules are sorted according to priority order
  (declare (ignore args))
  (let ((rules (noise-rules nm)))
    (setf (slot-value nm 'noise-rules) (sort (copy-list rules) #'rule-priority>=))))


(defun (setf qubit-povm) (povm nm qubit)
  "Set the povm for a qubit in this noise model."
  (check-povm povm)
  (setf (gethash qubit (readout-povms nm)) povm))


(defun make-noise-model (noise-rules)
  "Returns a noise model with noise-rules."
  (make-instance 'noise-model :noise-rules noise-rules))


(defun add-noise-models (nm1 nm2)
  "Combines two noise models by concatenating their rules. This way, either rule can be matched by an instruction."
  (make-noise-model (append (noise-rules nm1) (noise-rules nm2))))


(defun multiply-noise-models (nm1 nm2)
  "Combines two noise models in a way such that both set of rules, or one and not the other could be matched by an instruction."
  (make-noise-model
   (loop :for rule1 :in (noise-rules nm1)
         :append (loop :for rule2 :in (noise-rules nm2)
                       :collect (apply #'make-noise-rule
                                       (predicate-and (noise-predicate rule1) 
                                                      (noise-predicate rule2))
                                       (append (operation-elements rule1) 
                                               (operation-elements rule2)))
                       :collect (apply #'make-noise-rule 
                                       (predicate-and (noise-predicate rule1)                    
                                                      (predicate-not (noise-predicate rule2)))
                                       (operation-elements rule1))
                       :collect (apply #'make-noise-rule 
                                       (predicate-and (noise-predicate rule2)                    
                                                      (predicate-not (noise-predicate rule1)))
                                       (operation-elements rule2))))))


(defun match-strict-qubits (&rest qubits)
  "The returned function is true if QUBITS exactly equals the instruction's qubits."
  (lambda (instr)
    (and (typep instr 'quil:gate-application)
         (equal qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))))


(defun match-any-n-qubits (n qubit-list)
  "The returned function is true if there is any intersection between the instruction's qubits and the QUBIT-LIST for an N-qubit operation. We need to specify N in the rule because a 2 qubit gate CNOT 0 1 could match a rule with qubits that has operation elements for a 1q gate. We want to prevent this, so we require the user to specify the number of qubits expected in the gate."
  (lambda (instr) 
    (and (typep instr 'quil:gate-application)
         (= n (length (mapcar #'quil:qubit-index (quil:application-arguments instr))))
         (intersection qubit-list (mapcar #'quil:qubit-index (quil:application-arguments instr))))))


(defun match-strict-gate (gate)
  "The returned function is true if the instruction's gate is exactly equal to GATE."
  (lambda (instr) 
    (and (typep instr 'quil:gate-application)
         (quil::plain-operator-p (quil:application-operator instr))
         (string= gate (cl-quil::application-operator-root-name instr)))))


(defun match-any-gates (&rest gates)
  "The returned function is true if there is any intersection between the instruction's gates and GATES."
  (lambda (instr) 
    (and (typep instr 'quil:gate-application)
         (quil::plain-operator-p (quil:application-operator instr))
         (member (cl-quil::application-operator-root-name instr) gates :test #'string=))))


(defun match-all-nq-gates (n)
  "The returned function is true if the instruction operates on N qubits."
  (lambda (instr) 
    (and (typep instr 'quil:gate-application)
         (= n (length (quil:application-arguments instr))))))


(defun match-all-gates ()
  "The returned function is true if the instruction contains a gate (not a measure)."
  (lambda (instr) 
    (typep instr 'quil:gate-application)))


(defun match-measure ()
  "The returned function is true if the instruction is a MEASURE."
  (lambda (instr) 
    (typep instr 'quil:measurement)))


(defun match-measure-at-strict (qubit)
  "The returned function is true if the instruciton is a measure on the specified QUBIT."
  (lambda (instr) 
    (and (typep instr 'quil:measurement)
         (= qubit (quil:qubit-index (quil:measurement-qubit instr))))))


(defun match-measure-at-any (&rest qubits)
  "The returned function is true if the instruciton is a measure on any of the specified QUBITS."
  (lambda (instr) 
    (and (typep instr 'quil:measurement)
         (member (quil:qubit-index (quil:measurement-qubit instr)) qubits))))


(defun match-instr-idxs (parsed-prog &rest idxs)
  "The returned function is true if the index of the INSTR in the program PARSED-PROG matches IDX."
  (lambda (instr)
    (member (position instr (cl-quil::parsed-program-executable-code parsed-prog) :test 'eq) idxs)))

