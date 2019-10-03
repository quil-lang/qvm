(in-package #:qvm)

;; A noise predicate is a function that takes an instruction's gate and qubits, and returns either 
;; true if the instruction is a match, else false. The priority indicates the priority of the predicate (tbd) 
;; and the noise-position indicates whether the predicate should match before or after an instruction. 
(defclass noise-pred ()
  ((predicate
    :initarg :predicate
    :accessor predicate
    :initform nil
    :documentation "Does the instruction trigger the application of a channel?")
   (priority
    :initarg :priority
    :accessor priority
    :initform nil
    :documentation "For ordering when checking for an instruction's  matching predicates.")
   (noise-position
    :initarg :noise-position
    :accessor noise-position
    :initform nil
    :documentation "Should the application of a channel happen before or after the instruciton?")))


(defmethod make-noise-predicate (predicate priority noise-position)
  "Creates a noise predicate with the given predicate (callable), priority, and noise position"
  (make-instance 'noise-pred :predicate predicate :priority priority :noise-position noise-position))


(defmethod predicate-and (np1 np2)
  "Logical AND of 2 noise predicates"
  (let* ((conjunction (alexandria:conjoin (predicate np1) (predicate np2)))
         (priority (max (priority np1) (priority np2)))
         (noise-position (noise-position np1)))
    (make-noise-predicate conjunction priority noise-position)))


(defmethod predicate-or (np1 np2)
  "Logical OR of 2 noise predicates"
  (let* ((disjunction (alexandria:disjoin (predicate np1) (predicate np2)))
         (priority (max (priority np1) (priority np2)))
         (noise-position (noise-position np1)))
    (make-noise-predicate disjunction priority noise-position)))


(defmethod predicate-not (np)
  "Logical NOT of a noise predicate"
  (make-noise-predicate (complement (predicate np)) (priority np) (noise-position np)))


;; A noise rule consists of a noise predicate and a list of operation elements. The operation elements
;; describe the noise as kraus operators, and the noise-predicate is a function that takes an instruction
;; and returns true if the operation-elements should be applied after the instruction. 
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
    :documentation "Operation elements (kraus operators) to apply when the predicate is satisfied")))


(defun make-noise-rule (predicate operation-elements)
  "Returns a noise rule with the specified prediate and operation elements. "
  (make-instance 'noise-rule :noise-predicate predicate 
                             :operation-elements operation-elements))


;; A noise model consists of a set of noise rules and readout-povms. The noise rules define quantum channels and where they should be
;; applied,  and the readout-povms describe the noisy readout probabilities. 
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
    :documentation "A map of qubit to readout noise assignment probabilities. These are treated differently from noise channels (for now).")))


(defun make-noise-model (noise-rules)
  "Returns a noise model with noise-rules."
  (make-instance 'noise-model :noise-rules noise-rules))


(defmethod add-noise-models (nm1 nm2)
  " Combines two noise models by concatenating their rules. This way, either rule can be matched by an instruction. "
  (make-noise-model (append (noise-rules nm1) (noise-rules nm2))))


(defmethod multiply-noise-models (nm1 nm2)
  "Combines two noise models in a way such that both set of rules, or one and not the other could be matched by an instruction."
  (let ((nrs (loop :for rule1 in (noise-rules nm1) 
                   :append (loop :for rule2 in (noise-rules nm2)
                                 :collect (make-noise-rule 
                                           (predicate-and (noise-predicate rule1) (noise-predicate rule2)) 
                                           (list (operation-elements rule1) (operation-elements rule2)))
                                 :collect (make-noise-rule 
                                           (predicate-and (noise-predicate rule1) (predicate-not (noise-predicate rule2))) 
                                           (operation-elements rule1))
                                 :collect (make-noise-rule 
                                           (predicate-and (predicate-not (noise-predicate rule1)) (noise-predicate rule1)) 
                                           (operation-elements rule2))))))
    (make-noise-model nrs)))


;; The following are examples of predicates! 
(defun match-strict-qubits (qubits)
  (lambda (instr)
    (and (typep instr 'quil:gate-application)
         (equal qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))))


(defun match-any-qubits (qubits)
  "The returned function is true if there is any intersection between the instruction's qubits and qubits."
  (lambda (instr) (and (typep instr 'quil:gate-application)
                       (intersection qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))))


(defun match-strict-gates (gates)
  "The returned function is true if the instruciton's gates are exactly equal to gates"
  (lambda (instr) (and (typep instr 'quil:gate-application)
                       (equal gates (list (cl-quil::application-operator-name instr))))))


(defun match-any-gates (gates)
  "The returned function is true if there is any intersection between the instruction's gates and gates."
  (lambda (instr) (and (typep instr 'quil:gate-application)
                       (instr-intersect gates (list (cl-quil::application-operator-name instr))))))


(defun match-all-gates ()
  "The returned function is true if the instruction contains a gate (not a measure)."
  (lambda (instr) (typep instr 'quil:gate-application)))


(defun match-measure ()
  "The returned function is true if the instruction is a MEASURE."
  (lambda (instr) (typep instr 'quil:measurement)))


(defun match-measure-at-strict (qubit)
  "The returned function is true if the instruciton is a measure on the specified qubit."
  (lambda (instr) (and (typep instr 'quil:measurement)
                       (equal qubit (mapcar #'quil:qubit-index (quil:application-arguments instr))))))


(defun match-measure-at-any (qubits)
  "The returned function is true if the instruciton is a measure on any of the specified qubits. "
  (lambda (instr) (and (typep instr 'quil:measurement)
                       (intersection qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))))

(defmethod instr-intersect (l1 l2)
  "Check if there are any gates in common between L1 and L2. L1 and L2 are  lists of string."
  (loop :named outer :for inst1 in l1
        :do (loop :named inner :for inst2 in l2
                  :do (when (string= inst1 inst2) (return-from outer "A")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass channel-qvm (pure-state-qvm)
  ((original-amplitude-pointer
    :reader original-amplitude-pointer)
   (trial-amplitudes
    :accessor %trial-amplitudes)
   (noise-model
    :initarg :noise-model
    :accessor noise-model
    :initform nil)))


(defmethod initialize-instance :after ((qvm channel-qvm) &rest args)
  " Initializes an instance of a krausqvm"
  (declare (ignore args)) 
  (setf
   ;; Initialize the trial-amplitudes to an empty array of the correct size
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


(defmethod set-readout-povm ((qvm channel-qvm) qubit povm)
  " Set a readout povm for a qubit on this qvm"
  (check-povm povm)
  (setf (gethash qubit (readout-povms (noise-model qvm))) povm)
  nil)


(defmethod transition ((qvm channel-qvm) (instr quil:gate-application))
  "If any noise rules are matched by this instruction, apply the the kraus operators
for that noise rule after applying the gate for this instruction
"
  (let* ((gate   (pull-teeth-to-get-a-gate instr))
         (params (mapcar #'(lambda (p) (force-parameter p qvm))
                         (quil:application-parameters instr)))
         (instr-qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
      
         (noise-rules (noise-rules (noise-model qvm)))
         (prepend-kraus-ops (match-rules noise-rules instr "before"))
         (append-kraus-ops (match-rules noise-rules instr "after")))
    (when prepend-kraus-ops (apply-all-kraus-operators qvm instr prepend-kraus-ops))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple instr-qubits) params)
    (when append-kraus-ops (apply-all-kraus-operators qvm instr append-kraus-ops))
    (incf (pc qvm))
    qvm))


(defmethod transition :around ((qvm channel-qvm) (instr quil:measurement))
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noisee ret-qvm instr)
    ret-qvm))


(defun matches-rule (rule instr position)
  "check if rule is matched by instruction data and the position of the match request"
  (let* ((noise-predicate (predicate (noise-predicate rule)))
         (noise-position (noise-position (noise-predicate rule))))
    (and (funcall noise-predicate instr) (string= position noise-position))))


(defun match-rules (rules instr position)
  "return the operation elements for the first matching rule"
  (loop :for rule in rules
        :when (matches-rule rule instr position)
          :return (operation-elements rule)))


(defmethod apply-all-kraus-operators ((qvm channel-qvm) (instr quil:gate-application) kraus-operators)
  "Apply all the kraus operators to the state of the system."
  (loop :for kops :in kraus-operators
        :do
           (check-kraus-ops kops)
           (apply-kraus-ops qvm instr kops)))


(defmethod apply-kraus-ops ((qvm channel-qvm) (instr quil:gate-application) kraus-ops)
  (format t "instr! ~a~%" instr)
  (let* ((amps (%trial-amplitudes qvm))
         (r (random 1.0d0))
         (summed-p 0.0d0)
         (logical-qubits (quil:application-arguments instr))
         (qubits (mapcar #'quil:qubit-index logical-qubits))) ;; this needs to be some kind of reference to operational qubits. Have another dict? 
    (check-type amps quantum-state)
    (loop :for kj :in kraus-ops
          :do
             (replace amps (amplitudes qvm))
             (apply-matrix-operator (magicl-matrix-to-quantum-operator kj) amps (apply #'nat-tuple qubits))
             (incf summed-p (psum #'probability amps))
          :until (>= summed-p r))
    (rotatef (amplitudes qvm) (%trial-amplitudes qvm))
    (normalize-wavefunction (amplitudes qvm))))


(defgeneric apply-classical-readout-noisee (qvm instr)
  (:documentation "Given a QVM and a (measurement) instruction INSTR, corrupt the readout bit according to the POVM specifications of QVM.")
  ;; Pure state QVM has no readout noise.
  (:method ((qvm pure-state-qvm) (instr quil:measurement))
    (declare (ignore qvm instr))
    nil)
  ;; Readout noise only happens to the resulting classical bit (i.e.,
  ;; it's classical noise). As such, discarding that bit doesn't
  ;; warrant any sort of special treatment.
  (:method ((qvm approx-qvm) (instr quil:measure))
    (%corrupt-readout qvm instr (readout-povms qvm))
    )
  (:method ((qvm channel-qvm) (instr quil:measure-discard))
    (declare (ignore qvm instr))
    nil)
  ;; We do have a readout bit, and we want to corrupt it.
  (:method ((qvm channel-qvm) (instr quil:measure))
    (%corrupt-readout qvm instr (readout-povms (noise-model qvm))))
  ;; For compiled measurements, refer to the source of that
  ;; instruction.
  (:method ((qvm channel-qvm) (instr compiled-measurement))
    (apply-classical-readout-noise qvm (source-instruction instr))))


(defun %corrupt-readout (qvm instr povm-map)
  (check-type instr quil:measure)
  (let* ((q (quil:qubit-index (quil:measurement-qubit instr)))
         (a (quil:measure-address instr))
         (c (dereference-mref qvm a))
         (povm (gethash q povm-map))
         )
    (when povm
      (destructuring-bind (p00 p01 p10 p11) povm
        (setf (dereference-mref qvm a)
              (perturb-measurement c p00 p01 p10 p11))))))


;; TODO declare inlinabldee, turn on optimization
(defun perturb-measurement (actual-outcome p00 p01 p10 p11)
  "Given the readout error encoded in the POVM (see documentation of NOISY-QVM)
randomly sample the observed (potentially corrupted) measurement outcome."
  (check-type actual-outcome bit)
  (check-probabilities p00 p01 p10 p11)
  (let ((r (random 1.0d0)))
    (ecase actual-outcome
      ((0) (if (<= r p00) 0 1))
      ((1) (if (<= r p01) 0 1)))))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Temporary functions for testing


(defun make-nm1 ()
  (let* ((r1 (make-noise-rule (match-any-qubits (list 0 1)) (list (generate-damping-kraus-ops 2 1))))
         (nm (make-noise-model (list r1))))
    nm))


(defun make-nm2 ()
  (let* ((r2 (make-noise-rule (match-strict-gates (list "Y")) (list (generate-damping-kraus-ops 3 1))))
         (nm2 (make-noise-model (list r2))))
    nm2))

(defun make-noisy-qvm ()
  (let* ((p1 (make-noise-predicate (match-all-gates) 1 "after"))
         (r1 (make-noise-rule p1 (list (generate-damping-kraus-ops 2 1))))
         (nm (make-noise-model (list r1)))
         (qvm (make-instance 'channel-qvm :number-of-qubits 2 :noise-model nm)))
    qvm))

(defun build-noise-model ()
  (let* ((pred1 (make-noise-predicate (match-strict-qubits (list 0 1)) 1 "after"))
          (pred2 (make-noise-predicate (match-strict-gates (list "X" "Z")) 1 "after"))
          (kraus-elems1 (list (generate-damping-kraus-ops 2 1)))
          (kraus-elems2 (list (generate-damping-dephasing-kraus-ops 3 1)))
          (rule1 (make-noise-rule pred1 kraus-elems1))
          (rule2 (make-noise-rule pred2 kraus-elems2)))
    (make-noise-model (list rule1 rule2))))

;TODO: 2Q ANY!!
(defun test ()
  (let* (
         (q (make-noisy-qvm))
         (numshots 100)
         (qubit 0)
         (program "DECLARE R0 BIT; X 0; Y 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         )
    (set-readout-povm q 0 (list .9d0 .1d0 .1d0 .9d0))
    (load-program q parsed-program :supersede-memory-subsystem t)
    (run q)))



