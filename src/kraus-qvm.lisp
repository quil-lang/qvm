(in-package #:qvm)

(defclass kraus-qvm (pure-state-qvm)
  ((noise-rules
    :initarg :noise-rules
    :accessor noise-rules
    :initform nil)
   (noise-preds
    :initarg :noise-preds
    :accessor noise-preds
    :initform nil)
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table :test 'eql))
   (original-amplitude-pointer
    :reader original-amplitude-pointer)
   (trial-amplitudes
    :accessor %trial-amplitudes))) 


(defmethod initialize-instance :after ((qvm kraus-qvm) &rest args)
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


(defmethod run :after ((qvm kraus-qvm))
  ;; Only copy if we really need to.
  (when (requires-swapping-p qvm)
    ;; Copy the correct amplitudes into place.
    (copy-wavefunction (amplitudes qvm) (original-amplitude-pointer qvm))
    ;; Get the pointer back in home position. We want to swap them,
    ;; not overwrite, because we want the scratch memory to be intact.
    (rotatef (amplitudes qvm) (%trial-amplitudes qvm))))


(defmethod apply-all-kraus-rules ((qvm kraus-qvm) (instr quil:gate-application) rule-idx)
  "Apply all the relevant kraus operators (for all of the noise sources) to the state of the program"
  (loop :for kops :in (gethash rule-idx (noise-rules qvm))
        :do
           (check-kraus-ops kops)
           (apply-kraus-ops qvm instr kops)))


(defmethod apply-kraus-ops ((qvm kraus-qvm)(instr quil:gate-application) kraus-ops)
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


(defmethod get-match (predicate qvm target-idx)
  "return the idx of the matching instruction if there is a match for it"
  (if (and (noise-preds qvm) (noise-rules qvm))
        (if (equal (gethash target-idx (noise-preds qvm)) predicate) target-idx)))


(defmethod transition ((qvm kraus-qvm) (instr quil:gate-application))
  (let* ((gate   (pull-teeth-to-get-a-gate instr))
        (params (mapcar #'(lambda (p) (force-parameter p qvm))
                        (quil:application-parameters instr)))
        (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
        (curr-idx (pc qvm))
        (match-before (get-match "before" qvm curr-idx))
        (match-after (get-match "after" qvm curr-idx))
        )
    (if match-before (apply-all-kraus-rules qvm instr match-before))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
    (if match-after (apply-all-kraus-rules qvm instr match-after))
    (incf (pc qvm))
    qvm))


(defmethod transition :around ((qvm kraus-qvm) (instr quil:measurement))
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))


(defgeneric apply-classical-readout-noise (qvm instr)
  (:documentation "Given a QVM and a (measurement) instruction INSTR, corrupt the readout bit according to the POVM specifications of QVM.")
  ;; Pure state QVM has no readout noise.
  (:method ((qvm pure-state-qvm) (instr quil:measurement))
    (declare (ignore qvm instr))
    nil)
  ;; Readout noise only happens to the resulting classical bit (i.e.,
  ;; it's classical noise). As such, discarding that bit doesn't
  ;; warrant any sort of special treatment.
  (:method ((qvm kraus-qvm) (instr quil:measure-discard))
    (declare (ignore qvm instr))
    nil)
  ;; We do have a readout bit, and we want to corrupt it.
  (:method ((qvm kraus-qvm) (instr quil:measure))
    (%corrupt-qvm-memory-with-povm qvm instr))
  ;; For compiled measurements, refer to the source of that
  ;; instruction.
  (:method ((qvm kraus-qvm) (instr compiled-measurement))
    (apply-classical-readout-noise qvm (source-instruction instr))))


(defun %corrupt-qvm-memory-with-povm (qvm instr)
  (check-type instr quil:measure)
  (let* ((q (quil:qubit-index (quil:measurement-qubit instr)))
           (a (quil:measure-address instr))
           (c (dereference-mref qvm a))
           (povm (gethash q (readout-povms qvm))))
      (when povm
        (destructuring-bind (p00 p01 p10 p11) povm
          (setf (dereference-mref qvm a)
                (perturb-measurement c p00 p01 p10 p11))))))


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


(defun check-kraus-ops (kraus-ops)
  "Verify that a list KRAUS-OPS of Kraus operators given as MAGICL:MATRIX objects encodes a proper
Kraus map. In particular, we require that the Kraus operators are all of equal matrix dimension with equal numbers
of rows and columns. Furthermore, to ensure that the Kraus map preserves trace, they must be normalized such that
  sum_{j=1}^n K_j^H K_j = I
where I is the identity matrix of equal dimensions."
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
        ~S must be equal to the identity" kraus-sum))) t) 


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun test-kraus ()
  "Tests noise on a kraus-qvm with noise rules and no qubit povms. "
  (let ((ones 0)
        (qvm (make-instance 'kraus-qvm :number-of-qubits 1 :noise-rules (kraus-rules 2) :noise-preds (kraus-preds 2)))
        (numshots 100)
        (program "DECLARE R0 BIT; X 0; X 0; X 0;  MEASURE 0 R0"))
     (loop :repeat numshots
       :do (incf ones (do-noisy-measurement qvm 0 program)))
    (format t "result: ~a" (float (/ ones numshots)))))


(defun test-povm ()
  "Tests noise on a kraus-qvm with no noise rules and a povm on a single qubit."
  (let ((ones 0)
        (qvm (make-instance 'kraus-qvm :number-of-qubits 1))
        (numshots 100)
        (qubit 0)
        (program "DECLARE R0 BIT; X 0; X 0; X 0;  MEASURE 0 R0"))
     
    (set-readout-povm qvm qubit (list .9d0 .1d0 .1d0 .9d0 ) ) 
     (loop :repeat numshots
       :do (incf ones (do-noisy-measurement qvm 0 program)))
    (format t "result: ~a" (float (/ ones numshots)))))


(defun test-kraus-povm ()
  "Tests noise on a qvm with noise rules and a povm on a single qubit. "
  (let* ((ones 0)
        (qvm (make-instance 'kraus-qvm :number-of-qubits 1 :noise-rules (kraus-rules 2) :noise-preds (kraus-preds 2)))
        (numshots 100)
         (qubit 0)
        (program "DECLARE R0 BIT; X 0; X 0; X 0;  MEASURE 0 R0"))
    (set-readout-povm qvm qubit (list .9d0 .1d0 .1d0 .9d0 ) ) 
    (loop :repeat numshots
          :do (incf ones (do-noisy-measurement qvm qubit program)))
    (format t "result: ~a" (float (/ ones numshots)))))
    

(defun do-noisy-measurement (qvm q program)
  (bring-to-zero-state (amplitudes qvm))
  "Takes a qvm, qubit and program string, runs the program and returns the measured result of the qubit q"
  (let ((parsed-program (quil:parse-quil program)))
    (load-program qvm parsed-program :supersede-memory-subsystem t))
  (run qvm)
  (let ((measured-result (dereference-mref qvm (quil:mref "R0" q))))
    measured-result))


(defun kraus-preds (num-instrs)
  (setf k (make-hash-table))
  (loop for x from 0 to num-instrs
        do (setf (gethash x k) "after"))
  k)


(defun kraus-rules (num-instrs)
  (setf k (make-hash-table))
  ; make every even instr have a damping channel after it
  ; make every odd instr have a damping-dephasing channel after it
  (loop for x from 0 to num-instrs
        if (equal (mod x 2) 0)
        do  ( setf (gethash x k)  (list (generate-damping-kraus-ops 5 1)))
        else do (setf (gethash x k) (list (generate-damping-dephasing-kraus-ops 5 1))))
  
  (loop for x from 0 to num-instrs
        if (equal (mod x 2) 0)
        do  ( setf (gethash x k) (append (gethash x k) (list (generate-damping-kraus-ops 5 1)))))
  
  k)


(defmethod set-readout-povm ((qvm kraus-qvm) qubit povm)
  (check-povm povm)
  (setf (gethash qubit (readout-povms qvm)) povm)
  nil)

(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))
