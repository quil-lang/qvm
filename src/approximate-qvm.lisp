(in-package #:qvm)

(defclass approx-qvm (channel-qvm)
  ((t1-ops
    :initarg :t1-ops
    :accessor t1-ops
    :initform (make-hash-table :test 'eql)
    :documentation "T1 value to simulate")
   (t2-ops
    :initarg :t2-ops
    :accessor t2-ops
    :initform (make-hash-table :test 'eql)
    :documentation "T2 value to simulate")
   (avg-gate-time
    :initarg :avg-gate-time
    :accessor avg-gate-time
    :initform (error ":AVG-GATE-TIME is a required initarg to APPROX-QVM")
    :documentation "To calculate the kraus operators for T1, T2, etc.. noise, a gate time value is needed. Ideally, this gate time should be the  duration of the gate preceeding the application of the kraus noise. For now, since I haven't figured out a good way to get a qvm gate's time, I just made the gate time a slot that should represent the average gate time of gates that the qvm will run, and I use this value to calculate T1 and T2 noise. Set the GATE-TIME slot for approx-qvm. This value should represent the average  gate time of the gates that will be run.")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table :test 'eql)))
  (:documentation "APPROX-QVM is a QVM that supports a noise model defined by T1, T2, and readout fidelities on each qubit. At each instruction, T1 noise and T2 noise is applied for the qubits involved in the instruction. Upon MEASURE, the readout noise is also applied for whichever qubit is being measured."))


(defun (setf qubit-t1) (t1 qvm qubit)
  "Save the kraus operators for the T1 provided on the specific qubit."
  (let ((kraus-ops (generate-damping-kraus-ops t1 (avg-gate-time qvm))))
    (check-kraus-ops kraus-ops)
    (setf (gethash qubit (t1-ops qvm)) kraus-ops)))


(defun (setf qubit-t2) (t2 qvm qubit)
  "Save the kraus operators for the T2  provided on the specified qubit."
  (let ((kraus-ops (generate-damping-dephasing-kraus-ops t2 (avg-gate-time qvm))))
    (check-kraus-ops kraus-ops)
    (setf (gethash qubit (t2-ops qvm)) kraus-ops)))


(defmethod (setf qubit-fro) (fro (qvm approx-qvm) qubit)
  ;; Save the readout povms for the fro provided on the specified
  ;; qubit. The fR0 is assumed to be the average readout fidelity (avg
  ;; of p(1|1) and p(0|0)) so as an approximation, we set p(0|0) =
  ;; p(1|1) = fR0 in the readout povm assignment probabilities.
  (setf (gethash qubit (readout-povms qvm)) (list fro (- 1.0d0 fro) (- 1.0d0 fro) fro)))


(defmethod apply-all-kraus-operators ((qvm approx-qvm) (instr quil:gate-application) kraus-ops)
  ;; Apply all the kraus operators for each type of noise
  ;; source. KRAUS-OPS is a list of ( (t1-operators) (t2-operators)),
  ;; where t1-operators is a list of t1 noise maps for each qubit in
  ;; the instruction.  If the instruction is a 2q gate, we need to
  ;; tensor the kraus operators for each qubit's error source.
  (loop :for noise-operators in kraus-ops
        :do (let ((simplified-kraus-ops  (reduce #'kraus-kron noise-operators)))
              (when simplified-kraus-ops
                (apply-kraus-ops qvm instr simplified-kraus-ops)))))


(defmethod transition ((qvm approx-qvm) (instr quil:gate-application))
  ;; For the current instruction in the program, apply the gate
  ;; corresponding to that instruction, and then apply all the kraus
  ;; operators for all the sources of noise following that
  ;; instruction.
  (let* ((gate   (pull-teeth-to-get-a-gate instr))
         (params (mapcar (lambda (p) (force-parameter p qvm))
                         (quil:application-parameters instr)))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (t1-ops (loop :for q in qubits :collect (gethash q (t1-ops qvm))))
         (t2-ops (loop :for q in qubits :collect (gethash q (t2-ops qvm))))
         (kraus-ops (list t1-ops t2-ops)))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
    (apply-all-kraus-operators qvm instr kraus-ops)
    (incf (pc qvm))
    qvm))


(defmethod apply-classical-readout-noise ((qvm approx-qvm) (instr quil:measure))
  ;; Apply the classical readout noise to the state of an APPROX-QVM
  (%corrupt-readout qvm instr (readout-povms qvm)))


(defun tphi (t1 t2)
  "Calculate t_phi from T1 and T2. t_phi = (2*t1*t2) / (2*t1 + t2). If both T1 and T2 are 0, TPHI = 0."
  (if (and (> t1 0) (> t2 0))   
      (/ (* 2 (* t1 t2)) (+ t2 (* 2 t1)))
      0))


(defun generate-damping-kraus-ops (t1 gate-time)
  "Given a value for T1 and a GATE-TIME, generates the kraus operators corresponding to the t1 noise."
  (let* ((prob (/ gate-time t1))
         (k0 (magicl:make-complex-matrix 2 2 (list 0 0 (sqrt prob) 0)))
         (k1 (magicl:make-complex-matrix 2 2 (list 1 0 0  (sqrt (- 1 prob))))))
    (list k0 k1)))


(defun generate-dephasing-kraus-ops (t-phi gate-time)
  "Given a value for T-PHI (dephasing time) and a GATE-TIME, calculates the kraus operators corresponding to the dephasing noise."
  (let*  ((prob (/ gate-time t-phi))
          (prob-k1 (/ prob 2))
          (prob-k0 (- 1 prob-k1 ))
          (kraus-ops (loop :for mat :in '("I" "Z")
                           :for pj :in (list prob-k0 prob-k1)
                           :collect (magicl:scale (sqrt pj)
                                                  (quil:gate-matrix
                                                   (quil:gate-definition-to-gate
                                                    (quil:lookup-standard-gate mat)))))))
    kraus-ops))


;; TODO: Need to double check how damping dephasing kraus ops are
;; calculated before just combining it into
;; generate-dephasing-kraus-ops. For now I am pretty sure the
;; calculations are the same.
(defun generate-damping-dephasing-kraus-ops (t2 gate-time)
  "Calculates the kraus operators for dephasing and damping noise from T2, which is decoherence time."
  (generate-dephasing-kraus-ops t2 gate-time))


(defun kraus-kron (k1s k2s)
  "Calculate the tensor product of two kraus maps K1 and K2 by tensoring their elems. If one of the kraus maps is NIL, tensor the other with the identity matrix."
  (cond ((not k1s) (loop :for k in k2s
                         :collect (magicl:kron (magicl:make-identity-matrix 2) k )))
        ((not k2s) (loop :for k in k1s
                         :collect (magicl:kron k (magicl:make-identity-matrix 2))))
        (t  (loop :for k1 in k1s :append (loop :for k2 in k2s
                                               :collect (magicl:kron k1 k2))))))




