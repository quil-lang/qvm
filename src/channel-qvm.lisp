(in-package #:qvm)

(defclass noise-pred ()
  ((predicate
    :initarg :predicate
    :accessor predicate
    :initform nil
    :documentation "does the instruction trigger the application of a channel?")
   (priority
    :initarg :priority
    :accessor priority
    :initform nil
    :documentation "for ordering when checking for an instruction's  matching predicates.")
   (noise-position
    :initarg :noise-position
    :accessor noise-position
    :initform nil
    :documentation "should the application of a channel happen before or after the instruciton?")))


(defclass noise-rule ()
  ((noise-predicate
    :initarg :noise-predicate
    :accessor noise-predicate
    :initform nil 
    :documentation "a function that is true when an instruciton is matched, false otherwise")
   (operation-elements
    :initarg :operation-elements
    :accessor operation-elements
    :initform ()  
    :documentation "operation elements (kraus operators) to apply when the predicate is satisfied")))


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
    :documentation "A map of qubit to readout noise assignment probabilities. These are treated differently from noise channels (for now)."
    )))


(defmethod add-noise-models (nm1 nm2)
  " Combines two noise models by concatenating their rules. This way, either rule can be matched by an instruction. "
  (make-noise-model (append (noise-rules nm1) (noise-rules nm2))))


(defmethod multiply-noise-models (nm1 nm2)
  "Combines two noise models in a way such that both set of rules, or one and not the other could be matched by an instruction."
  (let ((nrs (loop :for rule1 in (noise-rules nm1) :append
    (loop :for rule2 in (noise-rules nm2)
          :collect (make-noise-rule (alexandria:conjoin (noise-predicate rule1) (noise-predicate rule2)) (list (operation-elements rule1) (operation-elements rule2) ))
          :collect (make-noise-rule (alexandria:conjoin (noise-predicate rule1) (complement (noise-predicate rule2))) (operation-elements rule1))
          :collect (make-noise-rule (alexandria:conjoin (complement (noise-predicate rule1)) (noise-predicate rule1)) (operation-elements rule2))))) )
    (make-noise-model nrs)))
 

(defun match-strict-qubits (qubits)
  "The returned function is true if the instruciton's qubits are exactly equal to qubits"
   (lambda (i-qubits i-name) (equal qubits i-qubits)))


(defun match-any-qubits (qubits)
  "The returned function is true if there is any intersection between the instruction's qubits and qubits."
   (lambda (i-qubits i-name) (intersection qubits i-qubits )) )


(defun match-strict-gates (gates)
    "The returned function is true if the instruciton's gates are exactly equal to gates"
   (lambda (i-qubits i-name) (equal gates i-name))
  )

(defun match-any-gates (gates)
    "The returned function is true if there is any intersection between the instruction's gates and gates."
  (lambda (i-qubits i-name) (instr-intersect gates i-name ))
  )

(defun match-all-gates ()
  "The returned function is true if the instruction contains a gate (not a measure)."
  (lambda (i-qubits i-name) (not (equal (list "MEASURE") i-name)))
  )


(defun match-measure ()
  "The returned function is true if the instruction is a MEASURE."
   (lambda (i-qubits i-name) (equal (list "MEASURE") i-name))
  )

(defun match-measure-at-strict (qubit)
  "The returned function is true if the instruciton is a measure on the specified qubit."
   (lambda (i-qubits i-name) (and (equal qubit i-qubits) (string= "MEASURE" i-name)))
  )


(defun match-measure-at-any (qubits)
  "The returned function is true if the instruciton is a measure on any of the specified qubits. "
  #' (lambda (i-qubits i-name) (and (intersection qubits i-qubits) (string= "MEASURE" i-name)))
  )


(defmethod instr-intersect (l1 l2)
  "Check if there are any gates in common between l1 and l2. 1 and l2 are  lists of string."
  (loop named outer :for inst1 in l1
        do (loop named inner :for inst2 in l2
                 do (if (string= inst1 inst2) (return-from outer "A")))))

(defun make-noise-model (noise-rules)
  "Returns a noise model with noise-rules."
  (make-instance 'noise-model :noise-rules noise-rules))


(defun make-noise-rule (predicate operation-elements)
  "Returns a noise rule with the specified prediate and operation elements. "
  (make-instance 'noise-rule :noise-predicate predicate :operation-elements operation-elements))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass channel-qvm (pure-state-qvm)
  ((original-amplitude-pointer
    :reader original-amplitude-pointer)
   (trial-amplitudes
    :accessor %trial-amplitudes)
   (noise-model
    :initarg :noise-model
    :accessor noise-model
    :initform nil))))


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
                             (original-amplitude-pointer qvm)))
       #-sbcl t))


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
         (instr-name (list (cl-quil::application-operator-name instr)))
         (noise-rules (noise-rules (noise-model qvm)))
         (kraus-ops (match-rules noise-rules instr-qubits instr-name)))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple instr-qubits) params)
    (if kraus-ops (apply-all-kraus-operators qvm instr kraus-ops))
    (incf (pc qvm))
    qvm))


(defmethod transition :around ((qvm channel-qvm) (instr quil:measurement))
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))


(defun match-rules (rules instr-qubits instr-name)
  "return the operation elements for the first matching rule"
  (loop :for rule in rules
        :when (funcall (noise-predicate rule) instr-qubits instr-name)
          :return (operation-elements rule)))


(defmethod apply-all-kraus-operators ((qvm channel-qvm) (instr quil:gate-application) kraus-operators)
  "Apply all the kraus operators to the state of the system."
  (loop :for kops :in kraus-operators
        :do
           (check-kraus-ops kops)
           (apply-kraus-ops qvm instr kops)))


(defmethod apply-kraus-ops ((qvm channel-qvm) (instr quil:gate-application) kraus-ops)
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


(defgeneric apply-classical-readout-noise (qvm instr)
  (:documentation "Given a QVM and a (measurement) instruction INSTR, corrupt the readout bit according to the POVM specifications of QVM.")
  ;; Pure state QVM has no readout noise.
  (:method ((qvm pure-state-qvm) (instr quil:measurement))
    (declare (ignore qvm instr))
    nil)
  ;; Readout noise only happens to the resulting classical bit (i.e.,
  ;; it's classical noise). As such, discarding that bit doesn't
  ;; warrant any sort of special treatment.
  (:method ((qvm krausqvm) (instr quil:measure-discard))
    (declare (ignore qvm instr))
    nil)
  ;; We do have a readout bit, and we want to corrupt it.
  (:method ((qvm krausqvm) (instr quil:measure))
    (%corrupt-qvm-memory-with-povm qvm instr))
  ;; For compiled measurements, refer to the source of that
  ;; instruction.
  (:method ((qvm krausqvm) (instr compiled-measurement))
    (apply-classical-readout-noise qvm (source-instruction instr))))


;; TODO declare inlinabldee, turn on optimization
(defun perturb-measurement (actual-outcome p00 p01 p10 p11)
  "Given the readout error encoded in the POVM (see documentation of NOISY-QVM)
randomly sample the observed (potentially corrupted) measurement outcome."
                                        ; (format t "actual: ~a p0|0: ~a p1|0: ~a" actual-outcome p00 p10)
  (check-type actual-outcome bit)
  (check-probabilities p00 p01 p10 p11)
  (let ((r (random 1.0d0)))
    (ecase actual-outcome
      ((0) (if (<= r p00) 0 1))
      ((1) (if (<= r p01) 0 1)))))


(defun %corrupt-qvm-memory-with-povm (qvm instr)
  (check-type instr quil:measure)
  (let* ((q (quil:qubit-index (quil:measurement-qubit instr)))
         (a (quil:measure-address instr))
         (c (dereference-mref qvm a))
         (povm (gethash q (readout-povms (noise-model qvm)))))
    (when povm
      (destructuring-bind (p00 p01 p10 p11) povm
        (setf (dereference-mref qvm a)
              (perturb-measurement c p00 p01 p10 p11))))))


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
     nm
     ))

  
(defun make-nm2 ()
   (let* ((r2 (make-noise-rule (match-strict-gates (list "Y")) (list (generate-damping-kraus-ops 3 1))))
          (nm2 (make-noise-model (list r2))))
     nm2))

(defun make-noisy-qvm ()
  (let* ((r1 (make-noise-rule (match-any-qubits (list 0 1)) (list (generate-damping-kraus-ops 2 1))))
         (r2 (make-noise-rule (match-strict-gates (list "Y")) (list (generate-damping-kraus-ops 3 1))))
         (nm (make-noise-model (list r1 r2)))
         (qvm (make-instance 'channel-qvm :number-of-qubits 2 :noise-model nm)))
    qvm)
  )

(defun test ()
  (let* ((pred1 (match-any-qubits1 (list 0 1)))
         (q (make-noisy-qvm))
         (numshots 100)
         (qubit 0)
         (program "DECLARE R0 BIT; X 0; Y 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         )
    (set-readout-povm q 0 (list .9d0 .1d0 .1d0 .9d0))
    (load-program q parsed-program :supersede-memory-subsystem t)
    (run q)
    )
  )



