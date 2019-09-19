 (in-package #:qvm)

(defclass noise-rule ()
  ((gates
    :initarg :gates
    :accessor gates
    :initform nil
    :documentation "a list of gate types to match. Should be a list of string, same as the keys in (gate-definitions qvm. I think that when a custom gate is added to QVM it gets added to gate-definitions?")
   (measures
    :initarg :measures
    :accessor measures
    :initform nil
    :documentation "a list of measurements to match.")
   (qubits
    :initarg :qubits
    :accessor qubits
    :initform nil
    :documentation "a list of qubits to match")
   (loc
    :initarg :loc
    :accessor loc
    :initform "after"
    :documentation "whether to insert the noise rule before or after instruction")
   (kraus-operators
    :initarg :kraus-operators
    :accessor kraus-operators
    :initform nil
    :documentation "kraus operators for this noise rule")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform nil
    :documentation "readout povms")))


(defclass krausqvm (pure-state-qvm)
  ((noise-rules
    :initarg :noise-rules
    :accessor noise-rules
    :initform nil
    :documentation "list of noise rules for this qvm")
   (original-amplitude-pointer
    :reader original-amplitude-pointer)
   (trial-amplitudes
    :accessor %trial-amplitudes)))


(defmethod initialize-instance :after ((qvm krausqvm) &rest args)
  " Initializes an instance of a kraus-qvm"
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


(defmethod run :after ((qvm krausqvm))
  ;; Only copy if we really need to.
  (when (requires-swapping-p qvm)
    ;; Copy the correct amplitudes into place.
    (copy-wavefunction (amplitudes qvm) (original-amplitude-pointer qvm))
    ;; Get the pointer back in home position. We want to swap them,
    ;; not overwrite, because we want the scratch memory to be intact.
    (rotatef (amplitudes qvm) (%trial-amplitudes qvm))))


(defmethod transition ((qvm krausqvm) (instr quil:gate-application))
  (let* ((gate   (pull-teeth-to-get-a-gate instr))
        (params (mapcar #'(lambda (p) (force-parameter p qvm))
                        (quil:application-parameters instr)))
        (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
        (curr-idx (pc qvm))
        (ant-rules (get-matches qvm instr qubits "before"))
       ; (post-rules (get-matches qvm instr qubits  "after"))
        )
   
   (format t "~a~%" ant-rules)
    
  ; (if ant-rules (apply-all-kraus-rules qvm instr ant-rules))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
   ; (if post-rules (apply-all-kraus-rules qvm instr post-rules))
    (incf (pc qvm))
    qvm))


(defmethod match-rule (rule instr qubits pos)

  (format t "~a~%" qubits)
  (format t "~a~%" (qubits rule))
  (if (eq (qubits rule) qubits) 
      (kraus-operators rule)
      nil
      )
  )


(defmethod get-matches ((qvm krausqvm) (instr quil:gate-application) qubits pos)
  (setf matched-rules ())
  (loop :for nr in (noise-rules qvm)
        if (match-rule nr instr qubits pos)
          do (append matched-rules (list nr)
                     )
          
          )
  )


(defmethod apply-all-kraus-rules ((qvm krausqvm) (instr quil:gate-application) rule-idx)
  "Apply all the relevant kraus operators (for all of the noise sources) to the state of the program"
  (loop :for kops :in (gethash rule-idx (noise-rules qvm))
        :do
           (check-kraus-ops kops)
           (apply-kraus-ops qvm instr kops)))


(defmethod apply-kraus-ops ((qvm krausqvm)(instr quil:gate-application) kraus-ops)
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



(defun test-kraus2 ()
  "Tests noise on a qvm with noise rules and a povm on a single qubit. "
  (let* ((ones 0)
         (noise (list (make-instance 'noise-rule :qubits 0)))
         (qvm (make-instance 'krausqvm :number-of-qubits 1 :noise-rules noise))
         (numshots 100)
         (qubit 0)
         (program "DECLARE R0 BIT; X 0; X 0; X 0;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         )
     
    (load-program qvm parsed-program :supersede-memory-subsystem t)
    (run qvm)
    )
  )
