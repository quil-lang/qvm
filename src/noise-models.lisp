;;;; noise-models.lisp
;;;;
;;;; Author: Sophia Ponte

(in-package #:qvm)

;;; Implementation of rule-based noise models for the QVM. These rules
;;; are meant to express locations in a qvm program to inject noise in
;;; the form of kraus operators.  A NOISE-MODEL is made up of a list
;;; of NOISE-RULES, where each NOISE-RULE is a NOISE-PREDICATE and a
;;; list of OPERATION-ELEMENTS. The NOISE-PREDICATE defines how to
;;; match QUIL instructions around which to apply the kraus operators
;;; defined by the OPERATION-ELEMENTS. 
;;;
;;; Example: A common noise model is the depolarization channel. To
;;; model depolarization on a qubit, inject depolarization operators
;;; after every gate on that qubit. We can build this noise model
;;; using NOISE-RULES in the following way.
;;;
;;; (let ((noise-predicate  (make-noise-predicate (match-strict-qubits 0) 19 :after))
;;;       (operation-elements (depolarizing-kraus-map .15)
;;;       (noise-rule (make-noise-rule noise-predicate operation-elements)
;;;       (noise-model (make-noise-model (list noise-rule))
;;;       (program "X 0; Y 1; H 0")
;;;
;;; The application of the above NOISE-MODEL to a CHANNEL-QVM running
;;; the program defined above would be to apply the OPERATION-ELEMENTS
;;; after the first and third instructions. The first argument in
;;; MAKE-NOISE-PREDICATE is a predicate function that returns true
;;; when an instruction it receives operates on exactly qubit 0. The
;;; next argument, 19, is a NOISE-PRIORITY, which indicates that this
;;; noise predicate should be attempted to be matched before other
;;; lower priority rules. The third argument :after, is the
;;; NOISE-POSITION, which tells the qvm to apply the
;;; OPERATION-ELEMENTS for this rule after the INSTR.  We can also
;;; define readout noise by setting the READOUT-POVMS for qubits in
;;; the QVM using the QUBIT-POVM setf function.

;;; These values are relatively arbitrary limits for NOISE-PREDICATE
;;; priorty. They are used to order noise models for matching to
;;; instructions.
(defconstant +maxpriority+ 20)
(defconstant +minpriority+ 0)
(alexandria:define-constant +default-noise-predicate-name+ "NOISE-NAME" :test #'string=)

(deftype noise-priority () `(integer ,+minpriority+ ,+maxpriority+))

(deftype noise-position ()
  "Describes the position of a noise rule relative to an instruction. Should the noise be applied :BEFORE or :AFTER the instruction is executed?" 
  '(member :before :after))

;;; A NOISE-PREDICATE describes how and when to match instructions in
;;; a program. The PREDICATE-FUNCTION is a function that takes an
;;; instruction's gate and qubits, and returns either true if the
;;; instruction is a match, else false. The PRIORITY indicates the
;;; priority of the predicate and the NOISE-POSITION indicates whether
;;; the predicate should match before or after an instruction. The
;;; optional NAME parameter is used to name the NOISE-RULE that
;;; contains a NOISE-PREDICATE.
(defclass noise-predicate ()
  ((predicate-function
    :initarg :predicate-function
    :reader predicate-function
    :documentation "Does the instruction trigger the application of a channel?")
   (priority
    :initarg :priority
    :reader priority
    :documentation "For ordering when checking for an instruction's matching predicates. The convention we use is that more specific predicates get higher priorities.")
   (noise-position
    :initarg :noise-position
    :reader noise-position
    :type noise-position
    :documentation "Should application of a channel happen before or after the instruction?")
   (name
    :initarg :name
    :reader name
    :documentation "Name or descriptor for noise."))
  (:default-initargs
   :predicate-function nil
   :priority 10
   :noise-position ':after
   :name +default-noise-predicate-name+)
  (:documentation "A NOISE-PREDICATE describes a collection of program instructions. When used in a NOISE-RULE, the NOISE-PREDICATE describes which instructions should be matched by the noise described in the OPERATION-ELEMENTS. Optionally, the NOISE-PREDICATE can be given a NAME. If none is provided, use the default 'NOISE-NAME'."))

(defmethod initialize-instance :after ((predicate noise-predicate) &rest args)
  ;; Assert that the NOISE-POSITION of the PREDICATE is of type
  ;; NOISE-POSITION, and that PRIORITY is of type INTEGER
  (declare (ignore args))
  (assert (typep (noise-position predicate) 'noise-position))
  (assert (typep (priority predicate) 'integer))
  (assert (typep (priority predicate) 'noise-priority)))

(defun make-noise-predicate (predicate-function priority noise-position &optional (name +DEFAULT-NOISE-PREDICATE-NAME+))
  "Creates a NOISE-PREDICATE with the given PREDICATE-FUNCTION (function designator), PRIORITY, and NOISE-POSITION."
  (make-instance 'noise-predicate :predicate-function predicate-function
                                  :priority priority
                                  :noise-position noise-position
                                  :name name))

(defun predicate-and (noise-predicate1 noise-predicate2)
  "Logical AND of 2 noise predicates. The NOISE-POSITION is taken from NOISE-PREDICATE1 and the priority is taken to be the max PRIORITY between the two predicates."
  (let* ((new-name (and-predicate-names (name noise-predicate1) (name noise-predicate2)))
         (conjunction (alexandria:conjoin (predicate-function noise-predicate1) 
                                          (predicate-function noise-predicate2)))
         (priority (max (priority noise-predicate1) (priority noise-predicate2)))
         (noise-position (noise-position noise-predicate1)))
    (make-noise-predicate conjunction priority noise-position new-name)))

(defun predicate-or (noise-predicate1 noise-predicate2)
  "Logical OR of 2 noise predicates. The NOISE-POSITION is taken from NOISE-PREDICATE1 and the priority is taken to be the max PRIORITY between the two predicates."
  (let* ((new-name (or-predicate-names (name noise-predicate1) (name noise-predicate2)))
         (disjunction (alexandria:disjoin (predicate-function noise-predicate1) 
                                          (predicate-function noise-predicate2)))
         (priority (max (priority noise-predicate1) (priority noise-predicate2)))
         (noise-position (noise-position noise-predicate1)))
    (make-noise-predicate disjunction priority noise-position new-name)))

(defmethod predicate-not (noise-predicate)
  "Logical NOT of a NOISE-PREDICATE. The PRIORITY of the predicate is 'inverted', by taking the difference between itself and +MAXPRIORITY+."
  (make-noise-predicate (complement (predicate-function noise-predicate))
                        (- +maxpriority+ (priority noise-predicate))
                        (noise-position noise-predicate)
                        (not-predicate-name (name noise-predicate))))

(defun and-predicate-names (name1 name2)
  "Returns the concatenated predicate name (NAME1 & NAME2)"
  (concatenate 'string "(" name1 " & " name2 ")"))

(defun or-predicate-names (name1 name2)
  "Returns the concatenated predicate name NAME1|NAME2"
  (concatenate 'string  "(" name1 " | " name2 ")" ))

(defun not-predicate-name (name)
  "Returns the concatenated predicate name !NAME"
  (concatenate 'string "!" name))

;;; A NOISE-RULE consists of a NOISE-PREDICATE and a list of
;;; OPERATION-ELEMENTS. The OPERATION-ELEMENTS are kraus maps describe
;;; the noise associated with the rule. The NOISE-PREDICATE describes
;;; the rule: how and when in a program to apply the noise.
(defclass noise-rule ()
  ((noise-predicate
    :initarg :noise-predicate
    :accessor noise-predicate
    :documentation "A function that is true when an instruction is matched, false otherwise.")
   (operation-elements
    :initarg :operation-elements
    :reader operation-elements
    :documentation "A list of kraus maps to apply when the rule is matched. Each kraus map is a list of kraus operators."))
  (:documentation "A NOISE-RULE consists of noise data (OPERATION-ELEMENTS) and a specification of where the noise data should be applied to the program (NOISE-PREDICATE)."))

(defun rule-priority>= (rule1 rule2)
  "Comparator function for the PRIORITY of 2 noise rules. Returns true if the priority of R1 is >= to the priority of R2."
  (>= (priority (noise-predicate rule1)) (priority (noise-predicate rule2))))

(defun make-noise-rule (predicate &rest kraus-maps)
  "Returns a noise rule with the specified PREDICATE and list KRAUS-MAPS."
  (mapc #'check-kraus-ops kraus-maps)
  (make-instance 'noise-rule :noise-predicate predicate
                             :operation-elements kraus-maps))


;;; A NOISE-MODEL consists of a set of NOISE-RULES and READOUT-POVMS.
;;; The NOISE-RULES describe quantum channels and where they should be
;;; applied, and the READOUT-POVMS describe the noisy readout
;;; probabilities.
(defclass noise-model ()
  ((noise-rules
    :initarg :noise-rules
    :accessor noise-rules
    :documentation "A list of NOISE-RULEs.")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :documentation "A hash table of qubit to readout noise assignment probabilities, given as a 4-element list ((p(0|0) p(0|1) p(1|0) p(1|1))."))
  (:default-initargs
   :noise-rules nil
   :readout-povms (make-hash-table :test 'eql))
  (:documentation "A NOISE-MODEL is a collection of NOISE-RULES, describing what type of noise should be applied to a program that goes through a QVM, and where that noise should be applied."))

(defmethod initialize-instance :after ((nm noise-model) &rest args)
  ;; Ensure the NOISE-RULES in the NOISE-MODEL are sorted according to priority order
  (declare (ignore args))
  (let ((rules (noise-rules nm)))
    (setf (slot-value nm 'noise-rules) (sort (copy-list rules) #'rule-priority>=))))

(defun (setf qubit-povm) (povm noise-model qubit)
  "Set the POVM for a QUBIT in this NOISE-MODEL."
  (check-povm povm)
  (setf (gethash qubit (readout-povms noise-model)) povm))

(defun make-noise-model (noise-rules)
  "Returns a noise model with NOISE-RULES."
  (make-instance 'noise-model :noise-rules noise-rules))

(defun add-noise-models (nm1 nm2)
  "Combines two noise models NM1 and NM2 by concatenating their rules. This way, either rule can be matched by an instruction."
  (make-noise-model (append (noise-rules nm1) (noise-rules nm2))))

(defun multiply-noise-models (nm1 nm2)
  "Combines two noise models NM1 and NM2 in a way such that both set of rules, or one and not the other could be matched by an instruction."
  (make-noise-model
   (loop :for r1 :in (noise-rules nm1)
         :append (loop :for r2 :in (noise-rules nm2)
                       :collect (apply #'make-noise-rule
                                       (predicate-and (noise-predicate r1)
                                                      (noise-predicate r2))
                                       (append (operation-elements r1)
                                               (operation-elements r2)))
                       :collect (apply #'make-noise-rule
                                       (predicate-and (noise-predicate r1)                
                                                      (predicate-not (noise-predicate r2)))
                                       (operation-elements r1))
                       :collect (apply #'make-noise-rule
                                       (predicate-and (noise-predicate r2)                   
                                                      (predicate-not (noise-predicate r1)))
                                       (operation-elements r2))))))


;;; Predicate functions

(defun match-strict-qubits (&rest qubits)
  "The returned function is true if QUBITS exactly equals the gate application's qubits in the same order (CNOT 0 1 does NOT match CNOT 1 0)."
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
  "The returned function is true if the instruction is a GATE-APPLICATION that is exactly equal to GATE."
  (lambda (instr) 
    (and (typep instr 'quil:gate-application)
         (quil::plain-operator-p (quil:application-operator instr))
         (string= gate (cl-quil::application-operator-root-name instr)))))

(defun match-any-gates (&rest gates)
  "The returned function is true if there is any intersection between the instruction's gate and GATES."
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
  "The returned function is true if the instruction contains a gate (not a MEASURE)."
  (lambda (instr) 
    (typep instr 'quil:gate-application)))

(defun match-measure ()
  "The returned function is true if the instruction is a MEASURE."
  (lambda (instr) 
    (typep instr 'quil:measurement)))

(defun match-measure-at-strict (qubit)
  "The returned function is true if the instruciton is a MEASURE on the specified QUBIT."
  (lambda (instr) 
    (and (typep instr 'quil:measurement)
         (= qubit (quil:qubit-index (quil:measurement-qubit instr))))))

(defun match-measure-at-any (&rest qubits)
  "The returned function is true if the instruciton is a MEASURE on any of the specified QUBITS."
  (lambda (instr) 
    (and (typep instr 'quil:measurement)
         (member (quil:qubit-index (quil:measurement-qubit instr)) qubits))))

(defun match-instr-idxs (parsed-prog &rest idxs)
  "The returned function is true if the index of the INSTR in the program PARSED-PROG matches IDXS."
  (lambda (instr)
    (member (position instr (cl-quil::parsed-program-executable-code parsed-prog) :test 'eq) idxs)))
