;;;; approximate-qvm.lisp
;;;;
;;;; Author: Sophia Ponte

(in-package #:qvm)

;;; Implements a QVM that simulates T1, T2, READOUT,  and/or depolarizing noise
;;; after every instruction in a program.


;;; The APPROX-QVM supports a QUBIT-level noise model, intended to
;;; approximate the qubit noise on a QPU. The APPROX-QVM allows the
;;; specification of T1, T2, DEPOLARIZATION, and READOUT noise for
;;; each qubit. During the APPROX-QVM's TRANSITION, qubit-level noise
;;; is applied after each instruction for which qubit noise is
;;; specified.
(defclass approx-qvm (channel-qvm)
  ((t1-ops
    :initarg :t1-ops
    :accessor t1-ops
    :documentation "T1 noise for each qubit to simulate")
   (t2-ops
    :initarg :t2-ops
    :accessor t2-ops
    :documentation "T2 noise for each qubit to simulate")
   (depol-ops
    :initarg :depol-ops
    :accessor depol-ops
    :documentation "Gate application noise for each qubit to simulate. Gate overrotations and underrotations average out to the depolarizing channel.")
   (avg-gate-time
    :initarg :avg-gate-time
    :reader avg-gate-time
    :documentation "To calculate the kraus operators for T1, T2, etc.. noise, a gate time value is needed. Ideally, this gate time should be the  duration of the gate preceeding the application of the kraus noise. For now, since I haven't figured out a good way to get a qvm gate's time, I just made the gate time a slot that should represent the average gate time of gates that the qvm will run, and I use this value to calculate T1 and T2 noise. Set the GATE-TIME slot for approx-qvm. This value should represent the average  gate time of the gates that will be run.")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms))
  (:default-initargs
   :t1-ops (make-hash-table :test 'eql)
   :t2-ops (make-hash-table :test 'eql)
   :depol-ops (make-hash-table :test 'eql)
   :avg-gate-time (error ":AVG-GATE-TIME is a required initarg to APPROX-QVM")
   :readout-povms (make-hash-table :test 'eql))
  (:documentation "APPROX-QVM is a QVM that supports a noise model defined by T1, T2, and readout fidelities on each qubit. At each instruction, T1 noise and T2 noise is applied for the qubits involved in the instruction. Upon MEASURE, the readout noise is also applied for whichever qubit is being measured."))


(defmethod initialize-instance :after ((qvm approx-qvm) &rest args)
  ;; If an approx-qvm is initialized with T1, T2, or READOUT-POVM
  ;; values, check that they are valid.
  (declare (ignore args))
  (maphash #'%check-noise-entry (t1-ops qvm))
  (maphash #'%check-noise-entry (t2-ops qvm))
  (maphash #'%check-noise-entry (depol-ops qvm))
  (maphash #'%check-povm-entry (readout-povms qvm)))


(defun %check-noise-entry (qubit noise-ops)
  "Helper function to check that a key value pair in T1-OPS or T2-OPS is valid. The qubit must be a NON-NEGATIVE INTEGER, and the NOISE-OPS must be a valid kraus map."
  (assert (and (integerp qubit) (<= 0 qubit)))
  (check-kraus-ops noise-ops))


(defun %check-povm-entry (qubit povm)
  "Helper fucntion to check that a key value pair in READOUT-POVMS is valid. The QUBIT must be a NON-NEGATIVE INTEGER, and the POVM must be a correctly formatted POVM."
  (assert (and (integerp qubit) (<= 0 qubit)))
  (check-povm povm))


(defun (setf qubit-t1) (t1 qvm qubit)
  "Save the kraus operators for the T1 provided on the specific qubit."
  (let ((kraus-ops (generate-damping-kraus-map t1 (avg-gate-time qvm))))
    (check-kraus-ops kraus-ops)
    (setf (gethash qubit (t1-ops qvm)) kraus-ops)))


(defun (setf qubit-t2) (t2 qvm qubit)
  "Save the kraus operators for the T2  provided on the specified qubit."
  (let ((kraus-ops (generate-dephasing-kraus-map t2 (avg-gate-time qvm))))
    (check-kraus-ops kraus-ops)
    (setf (gethash qubit (t2-ops qvm)) kraus-ops)))


(defun (setf qubit-depolarization) (depol-prob qvm qubit)
  "Save the kraus operators for the depolarizing channel on QUBIT. The depolarizing channel represents the average gate overrotation/underrotation noise. It is specified by DEPOL-PROB, which is the probability of overrotation/underrotation for the given QUBIT."
  (let ((kraus-ops (generate-depolarizing-kraus-map depol-prob)))
    (check-kraus-ops kraus-ops)
    (setf (gethash qubit (depol-ops qvm)) kraus-ops)))


(defun (setf qubit-fro) (fro qvm qubit)
  "Save the readout povms for the fro provided on the specified qubit. The fR0 is assumed to be the average readout fidelity (avg of p(1|1) and p(0|0)) so as an approximation, we set p(0|0) = p(1|1) = fR0 in the readout povm assignment probabilities."
  (setf (gethash qubit (readout-povms qvm)) (list fro (- 1.0d0 fro) (- 1.0d0 fro) fro)))


(defmethod apply-all-kraus-maps ((qvm approx-qvm) (instr quil:gate-application) kraus-ops)
  ;; Apply all the kraus operators for each type of noise
  ;; source. KRAUS-OPS is a list of ( (t1-operators) (t2-operators)),
  ;; where t1-operators is a list of t1 noise maps for each qubit in
  ;; the instruction.  If the instruction is a 2q gate, we need to
  ;; tensor the kraus operators for each qubit's error source.
  (loop :for noise-operators :in kraus-ops
        :do (let ((simplified-kraus-ops  (reduce #'kraus-kron noise-operators)))
              (when simplified-kraus-ops
                (apply-kraus-map qvm instr simplified-kraus-ops)))))


(defmethod transition ((qvm approx-qvm) (instr quil:gate-application))
  ;; For the current instruction in the program, apply the gate
  ;; corresponding to that instruction, and then apply all the kraus
  ;; operators for all the sources of noise following that
  ;; instruction.
  (let* ((gate   (pull-teeth-to-get-a-gate instr))
         (params (mapcar (lambda (p) (force-parameter p qvm))
                         (quil:application-parameters instr)))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (t1-ops (loop :for q :in qubits :collect (gethash q (t1-ops qvm))))
         (t2-ops (loop :for q :in qubits :collect (gethash q (t2-ops qvm))))
         (depol-ops (loop :for q :in qubits :collect (gethash q (depol-ops qvm))))
         (kraus-ops (list t1-ops t2-ops depol-ops)))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
    (apply-all-kraus-maps qvm instr kraus-ops)
    (incf (pc qvm))
    qvm))


(defmethod apply-classical-readout-noise ((qvm approx-qvm) (instr quil:measure))
  ;; Apply the classical readout noise to the state of an APPROX-QVM
  (%corrupt-readout qvm instr (readout-povms qvm)))


(defun tphi (t1 t2)
  "Calculate t_phi from T1 and T2. t_phi = (2*t1*t2) / (2*t1 + t2). If both T1 and T2 are 0, TPHI = 0."
  (if (and (plusp t1) (plusp t2))
      (/ (* 2 (* t1 t2))
         (+ t2 (* 2 t1)))
      0))


(defun generate-damping-kraus-map (t1 gate-time)
  "Given a value for T1 and a GATE-TIME, generates the kraus operators corresponding to the t1 noise."
  (let* ((prob (/ gate-time t1))
         (k0 (magicl:make-complex-matrix 2 2 (list 0 0 (sqrt prob) 0)))
         (k1 (magicl:make-complex-matrix 2 2 (list 1 0 0  (sqrt (- 1 prob))))))
    (list k0 k1)))


(defun generate-dephasing-kraus-map (t-phi gate-time)
  "Given a value for T-PHI (dephasing time) and a GATE-TIME, calculates the kraus operators corresponding to the dephasing noise."
  (let*  ((prob (/ gate-time t-phi))
          (prob-k0 (/ prob 2))
          (prob-k1 (- 1 prob-k0))
          (base-matrices '("I" "Z")))
    (generate-kraus-ops base-matrices (list (sqrt prob-k0) (sqrt prob-k1)))))


(defun generate-depolarizing-kraus-map (prob)
  "Given a probability of depolarization PROB, calculate the kraus operation elements for the depolarizing channel."
  (assert (< 0 prob 1) (prob) "DEPOL-PROB must be between 0 and 1")
  (let* ((pk0 (sqrt (- 1 (/ (* 3 prob) 4))))
         (pkn (sqrt (/ prob 4)))
         (base-matrices '("I" "X" "Y" "Z"))
         (probs (list pk0 pkn pkn pkn)))
    (generate-kraus-ops base-matrices probs)))


;; TODO: Need to double check how damping dephasing kraus ops are
;; calculated before just combining it into
;; generate-dephasing-kraus-ops. For now I am pretty sure the
;; calculations are the same.
(defun generate-damping-dephasing-kraus-map (t2 gate-time)
  "Calculates the kraus operators for dephasing and damping noise from T2, which is decoherence time."
  (generate-dephasing-kraus-map t2 gate-time))


(defun kraus-kron (k1s k2s)
  "Calculate the tensor product of two kraus maps K1 and K2 by tensoring their elems. If one of the kraus maps is NIL, tensor the other with the identity matrix."
  (cond ((not k1s) (loop :for k :in k2s
                         :collect (magicl:kron (magicl:make-identity-matrix 2) k )))
        ((not k2s) (loop :for k :in k1s
                         :collect (magicl:kron k (magicl:make-identity-matrix 2))))
        (t  (loop :for k1 :in k1s :append (loop :for k2 :in k2s
                                                :collect (magicl:kron k1 k2))))))


(defun generate-kraus-ops (standard-matrices probs)
  "Builds a list of kraus operators from the base matrices in STANDARD-MATRICES, each scaled by a corresponding probability in PROBS."
  (loop :for mat :in standard-matrices
        :for prob in probs
        :collect (magicl:scale prob (quil:gate-matrix
                                     (quil:gate-definition-to-gate
                                      (quil:lookup-standard-gate mat))))))




