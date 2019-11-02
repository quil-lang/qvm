;;;; basic-noise-qvm.lisp
;;;;
;;;; Author: Sophia Ponte

(in-package #:qvm)

;;; Implements a QVM that simulates T1, T2, T-PHI, READOUT, and/or DEPOLARIZATION noise
;;; after every instruction in a program.

(deftype probability () `(double-float 0.0d0 1.0d0))

(deftype valid-noise-value () `(real (0.0d0) *))

;;; The BASIC-NOISE-QVM supports a qubit-level noise model, intended
;;; to approximate the qubit noise on a QPU. The BASIC-NOISE-QVM
;;; allows the specification of amplitude damping, dephasing,
;;; depolarization, and readout noise for each qubit. During the
;;; BASIC-NOISE-QVM's TRANSITION, after each instruction is applied,
;;; qubit-level noise in the form of kraus operators is applied to the
;;; state.

;;; Example: Assume we want to simulate depolarizing noise on both
;;; qubits of a 2 qubit qvm. We can start by defining a
;;; BASIC-NOISE-QVM:
;;;
;;; (make-instance 'basic-noise-qvm :number-of-qubits 2 :avg-gate-time time)
;;;
;;; and then setting depolarization values for each of the qubits.
;;;
;;; (setf (qubit-depolarization qvm 0) depolarization-prob-q0)
;;; (setf (qubit-depolarization qvm 1) depolarization-prob-q1)
;;;
;;; If we later wanted to add T1 noise, we can continue by doing the following: 
;;;
;;; (setf (qubit-t1 qvm 0) t1-for-q0)
;;; (setf (qubit-t1 qvm 1) t1-for-q1)
;;;
;;; Now, when QVM runs a program, after each instruction the QVM
;;; generates the kraus operators for each type of noise defined (in
;;; this case: depolarizing and t1) and applies them to the state.
(defclass basic-noise-qvm (channel-qvm)
  ((t1-vals
    :initarg :t1-vals
    :accessor t1-vals
    :documentation "Hash table of qubit idx to T1 value for each qubit to simulate")
   (t2-vals
    :initarg :t2-vals
    :accessor t2-vals
    :documentation "Hash table of qubit idx to T2 value for each qubit to simulate")
   (tphi-vals
    :initarg :tphi-vals
    :accessor tphi-vals
    :documentation "Hash table of qubit idx to TPHI value for each qubit to simulate.")
   (depolarization-ops
    :initarg :depolarization-ops
    :accessor depolarization-ops
    :documentation "A hash table of qubit to depolarization operators, representing gate application noise. Gate over-rotations and under-rotations average out to the depolarizing channel.")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :documentation "A hash-table of qubit idx to a list of povms, representing the readout assignment probabilities (p(0|0) p(0|1) p(1|0) p(1|1)).")
   (avg-gate-time
    :initarg :avg-gate-time
    :reader avg-gate-time
    :documentation "To calculate the kraus operators for T1, T2, etc. noise, a gate time value is needed. Ideally, this gate time should be the duration of the gate preceding the application of the kraus noise. As an approximation, this value should represent the average gate time of the gates that will be run.")
   (elapsed-time
    :initarg :elapsed-time
    :accessor elapsed-time
    :documentation "A value to keep track of the elpased time of the program. The elapsed time is used for calculating the kraus operators of some noise value at a given point in the program."))
  (:default-initargs
   :t1-vals (make-hash-table :test 'eql)
   :tphi-vals (make-hash-table :test 'eql)
   :t2-vals (make-hash-table :test 'eql)
   :depolarization-ops (make-hash-table :test 'eql)
   :readout-povms (make-hash-table :test 'eql)
   :avg-gate-time (error ":AVG-GATE-TIME is a required initarg to BASIC-NOISE-QVM")
   :elapsed-time 0.0d0)
  (:documentation "BASIC-NOISE-QVM is a QVM that supports a noise model defined by T1, T2, T-phi, depolarization probability, and readout fidelities on each qubit. At each instruction, damping, dephasing, and depolarizing noise is applied for the qubits involved in the instruction. If READOUT-POVMS are defined for a qubit, the readout noise is applied after a MEASURE instruction on that qubit."))

(defmethod initialize-instance :after ((qvm basic-noise-qvm) &rest args)
  ;; If a BASIC-NOISE-QVM is initialized with T1-VALS, T2-VALS,
  ;; TPHI-VALS, DEPOLARIZATION-OPS, or READOUT-POVMs, check that they
  ;; are valid.
  (declare (ignore args))
  (maphash #'%check-noise-entry (t1-vals qvm))
  (maphash #'%check-noise-entry (tphi-vals qvm))
  (maphash #'%check-noise-entry (t2-vals qvm))
  (maphash #'%check-depol-entry (depolarization-ops qvm))
  (maphash #'%check-povm-entry (readout-povms qvm)))

(defun %check-depol-entry (qubit kraus-map)
  "Check that a key value pair in the DEPOLARIZATION-OPS slot is valid. The QUBIT must be a non-negative integer, and the KRAUS-MAP must be a valid kraus map. "
  (check-type qubit nat-tuple-element)
  (check-kraus-ops kraus-map))

(defun %check-noise-entry (qubit noise-val)
  "Check that a key value pair in T1-VALS, T2-VALS, or TPHI-VALS is valid. The QUBIT must be a NON-NEGATIVE INTEGER, and the NOISE-VAL must be of VALID-NOISE-VALUE type for T1-VALS, T2-VALS, and TPHI-VALS."
  (check-type qubit nat-tuple-element)
  (check-type noise-val valid-noise-value))

(defun %check-povm-entry (qubit povm)
  "Check that a key value pair in READOUT-POVMS is valid. The QUBIT must be a NON-NEGATIVE INTEGER, and the POVM must be a correctly formatted POVM."
  (check-type qubit nat-tuple-element)
  (check-povm povm))

(defun (setf qubit-t1) (t1 qvm qubit)
  "Evaluate that T1 is valid, and save it for the specified QUBIT in the QVM."
  (check-type t1 valid-noise-value)
  (setf (gethash qubit (t1-vals qvm)) t1))

(defun (setf qubit-t2) (t2 qvm qubit)
  "Evaluate that T2 is valid and save it for the specified QUBIT in the QVM."
  (check-type t2 valid-noise-value)
  (setf (gethash qubit (t2-vals qvm)) t2))

(defun (setf qubit-tphi) (tphi qvm qubit)
  "Evaluate that TPHI is valid and save it for the specified QUBIT in the QVM."
  (check-type tphi valid-noise-value)  
  (setf (gethash qubit (tphi-vals qvm)) tphi))

(defun (setf qubit-depolarization) (depolarization-probability qvm qubit)
  "Save the DEPOLARIZATION-PROBABILITY for QUBIT. The depolarizing channel represents the average gate over-rotation/under-rotation noise. It is specified by DEPOLARIZATION-PROBABILITY, which is the probability of overrotation/underrotation for the given QUBIT."
  (check-type depolarization-probability probability)
  (let ((kraus-ops (depolarizing-kraus-map depolarization-probability)))
    (check-kraus-ops kraus-ops)
    (setf (gethash qubit (depolarization-ops qvm)) kraus-ops)))

(defun (setf qubit-fro) (fro qvm qubit)
  "Save the readout POVMs for the FRO on the specified QUBIT on the QVM. The FRO is assumed to be the average readout fidelity (avg of p(1|1) and p(0|0)) so as an approximation, we set p(0|0) = p(1|1) = fR0 in the READOUT-POVMS assignment probabilities."
  (let ((one-minus-fro (- 1.0d0 fro)))
    (setf (gethash qubit (readout-povms qvm)) (list fro one-minus-fro one-minus-fro fro))))

(defmethod apply-all-kraus-maps ((qvm basic-noise-qvm) (instr quil:gate-application) kraus-ops)
  ;; Apply all the kraus operators for each type of noise
  ;; source. KRAUS-OPS is a list of ((damping-operators)
  ;; (dephasing-operators) (depolarization-operators)). If the
  ;; instruction involves more than one qubit, we need to tensor the
  ;; kraus operators for each qubit's error sources.
  (loop :for noise-operators :in kraus-ops
        :when noise-operators 
          :do (let ((simplified-kraus-ops (reduce #'kraus-kron noise-operators)))
                (when simplified-kraus-ops
                  (apply-kraus-map qvm instr simplified-kraus-ops)))))

(defmethod run :before ((qvm basic-noise-qvm))
  ;; Before running a new program on the BASIC-NOISE-QVM, reset the
  ;; ELAPSED-TIME to 0.
  (setf (elapsed-time qvm) 0.0d0))

(defmethod transition ((qvm basic-noise-qvm) (instr quil:gate-application))
  ;; For the current INSTR, collect and apply all the kraus operators
  ;; for all the sources of noise on the involved qubits, and evolve
  ;; the elapsed time of the qvm. The depolarization noise of a qubit
  ;; is time invariant, so we simply access the pre-computed kraus
  ;; operators for depolarization. However, the damping and dephasing
  ;; noise is time-dependant, so we need to construct the kraus
  ;; operators for this noise in place using the ELAPSED-TIME of the
  ;; QVM. After the computation, evolve the ELAPSED-TIME of the QVM.
  ;; Note: This is not an after method because we do not want to
  ;; invoke CHANNEL-QVM's TRANSITION method.
  (let* ((gate   (pull-teeth-to-get-a-gate instr))
         (params (mapcar (lambda (p) (force-parameter p qvm))
                         (quil:application-parameters instr)))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (curr-time (+ (avg-gate-time qvm) (elapsed-time qvm)))
         (damping-ops (loop :for q in qubits
                            :for q-t1 := (gethash q (t1-vals qvm))
                            :when q-t1 :collect (damping-kraus-map q-t1 curr-time)))
         (dephasing-ops (loop :for q :in qubits
                              :for q-tphi := (gethash q (tphi-vals qvm))
                              :for q-t1 := (gethash q (t1-vals qvm))
                              :for q-t2 := (gethash q (t2-vals qvm))
                              :when (calculate-dephasing-map q-tphi q-t1 q-t2 curr-time) 
                                :collect it))
         (depol-ops (loop :for q :in qubits 
                          :collect (gethash q (depolarization-ops qvm)))))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
    (apply-all-kraus-maps qvm instr (list damping-ops dephasing-ops depol-ops))
    (setf (elapsed-time qvm) curr-time)
    (incf (pc qvm))
    qvm))

(defun calculate-dephasing-map (qubit-tphi qubit-t1 qubit-t2 elapsed-time)
  "Build the dephasing map. If TPHI is not null, use that to calculate the dephasing map. In the absence of a TPHI value, use T1 and T2 to calculate TPHI if those values exist. T2 is upper bounded by 2 * T1."
  (cond 
    ;; First, try to use T-PHI to calculate the dephasing operators
    (qubit-tphi 
     (dephasing-kraus-map qubit-tphi elapsed-time))
    ;; Next, if there are values for QUBIT-T1 and QUBIT-T2,
    ;; calculate T-PHI
    ((and qubit-t2 qubit-t1) 
     (dephasing-kraus-map (tphi qubit-t1 qubit-t2) elapsed-time))
    ;; If either T1 or T2 is missing, an no T-PHI, return no kraus
    ;; operators.
    (t nil)))

(defmethod apply-classical-readout-noise ((qvm basic-noise-qvm) (instr quil:measure))
  ;; Apply the classical readout noise to the state of an BASIC-NOISE-QVM.
  (%corrupt-qvm-memory-with-povm qvm instr (readout-povms qvm)))

(defun tphi (t1 t2)
  "Calculate t_phi from T1 and T2. T-PHI = (2*T1*T2) / (2*T1 + T2). T2 must be strictly less than 2 * T1."
  (assert (< t2 (* 2.0d0 t1)) 
          (t1 t2)
          "T2  must be upper bounded by 2*T1")
  (check-type t1 valid-noise-value)
  (check-type t2 valid-noise-value)
  (/ (* 2.0d0 (* t1 t2))
     (+ t2 (* 2.0d0 t1))))

(defun damping-kraus-map (t1 elapsed-time)
  "Given a value for T1 and a ELAPSED-TIME, generates the kraus operators corresponding to the amplitude damping noise."
  (let* ((prob (- 1 (exp (/ (- elapsed-time) t1))))
         (k0 (magicl:make-complex-matrix 2 2 (list 0.0d0 0.0d0 (sqrt prob) 0.0d0)))
         (k1 (magicl:make-complex-matrix 2 2 (list 1.0d0 0.0d0 0.0d0  (sqrt (- 1.0d0 prob))))))
    (list k0 k1)))

(defun dephasing-kraus-map (t-phi elapsed-time)
  "Given a value for T-PHI (dephasing time) and a ELAPSED-TIME, calculates the kraus operators corresponding to the dephasing noise."
  (let*  ((prob (- 1 (exp (/ (- elapsed-time) t-phi))))
          (prob-k0 (/ prob 2))
          (prob-k1 (- 1 prob-k0))
          (base-matrices '("I" "Z")))
    (generate-kraus-ops base-matrices (list (sqrt prob-k0) (sqrt prob-k1)))))

(defun depolarizing-kraus-map (prob)
  "Given a probability of depolarization PROB, calculate the kraus operation elements for the depolarizing channel."
  (assert (< 0 prob 1) (prob) "DEPOLARIZATION-PROBABILITY must be between 0 and 1")
  (let* ((pk0 (sqrt (- 1 (* 3/4 prob))))
         (pkn (sqrt (/ prob 4)))
         (base-matrices '("I" "X" "Y" "Z"))
         (probs (list pk0 pkn pkn pkn)))
    (generate-kraus-ops base-matrices probs)))

(defun kraus-kron (k1s k2s)
  "Calculate the tensor product of two kraus maps K1S and K2S by tensoring their elems. If one of the kraus maps is NIL, tensor the other with the identity matrix."
  (let ((identity-matrix (magicl:make-identity-matrix 2)))
    (cond ((endp k1s) (loop :for k :in k2s
                            :collect (magicl:kron identity-matrix k)))
          ((endp k2s) (loop :for k :in k1s
                            :collect (magicl:kron k identity-matrix)))
          (t  (loop :for k1 :in k1s 
                    :append (loop :for k2 :in k2s
                                  :collect (magicl:kron k1 k2)))))))

(defun generate-kraus-ops (standard-matrices probs)
  "Builds a list of kraus operators from the base matrices in STANDARD-MATRICES, each scaled by a corresponding probability in PROBS."
  (loop :for mat :in standard-matrices
        :for prob :in probs
        :collect (magicl:scale prob (quil:gate-matrix
                                     (quil:gate-definition-to-gate
                                      (quil:lookup-standard-gate mat))))))




