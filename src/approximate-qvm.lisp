(in-package #:qvm)

(defclass approx-qvm (channel-qvm)
  ((t1-ops
    :initarg :t1-ops
    :accessor t1-ops
    :initform (make-hash-table :test 'eql)
    :documentation "t1 value to simulate")
   (t2-ops
    :initarg :t2-ops
    :accessor t2-ops
    :initform (make-hash-table :test 'eql)         
    :documentation "t2 value to simulate")
   (avg-gate-time
    :initarg :avg-gate-time
    :accessor avg-gate-time
    :initform (error ":AVG-GATE-TIME is a required initarg to APPROX-QVM")
    :documentation "The average gate time for a gate in this qvm")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table :test 'eql))))


(defun (setf qubit-t1) (t1 qvm qubit)
  (let ((kraus-ops (generate-damping-kraus-ops t1 (avg-gate-time qvm))))
    (check-kraus-ops kraus-ops)
    (setf (gethash qubit (t1-ops qvm)) kraus-ops)))


(defun (setf qubit-t2) (t2 qvm qubit)
  "Save the kraus operators for the t2 provided on the specified qubit."
  (let ((kraus-ops (generate-damping-dephasing-kraus-ops t2 (avg-gate-time qvm))))
    (check-kraus-ops kraus-ops)
    (setf (gethash qubit (t2-ops qvm)) kraus-ops))))


(defmethod (setf qubit-fro) (fro qvm qubit)
  "Save the readout povms for the fro provided on the specified qubit. The fR0 is assumed to be the average readout fidelity (avg of p(1|1) and p(0|0)) so as an approximation, we set p(0|0) = p(1|1) = fR0 in the readout povm assignment probabilities. "
  (setf (gethash qubit (readout-povms qvm)) (list fro (- 1.0d0 fro) (- 1.0d0 fro) fro)))


; For now just use a single gate time value for all calculations
(defmethod set-gate-time ((qvm approx-qvm) gate-time)
  "To calculate the kraus operators for T1, T2, etc.. noise, a gate time value is needed. Ideally, this gate time should be the duration of the gate preceeding the application of the kraus noise. For now, since I haven't figured out a good way to get a qvm gate's time, I just made the gate time a slot that should represent the average gate time of gates that the qvm will run, and I use this value to calculate T1 and T2 noise. Set the gate time slot for approx-qvm. This value should represent the average gate time of the gates that will be run."
  (setf (avg-gate-time qvm) gate-time))


(defmethod apply-all-kraus-operators ((qvm approx-qvm) (instr quil:gate-application) qubits)
  "Apply all the kraus operators (for all of the noise sources) to the state of the program. If we have just encountered a 2-qubit gate, we need to tensor the kraus operators for each qubit's error source. Currently, this does not support > 2 qubit gates. Note: if the current instr is a 2 qubit gate and there is only kraus ops for one of the qubits, then no error is applied. For example, if q0 has T1 error associated with it but q1 does not, no error channel is applied after a CNOT 0 1. "
  (loop :for source :in (list (t1-ops qvm) (t2-ops qvm))
        :do (case (length qubits)
              (1 (let ((kraus-map (gethash (nth 0 qubits) source))) 
                   (when kraus-map (apply-kraus-ops qvm instr kraus-map))))
              (2 (let ((kraus-map (kraus-kron (gethash (nth 0 qubits) source) (gethash (nth 1 qubits) source))))
                   (when kraus-map (check-kraus-ops kraus-map) (apply-kraus-ops qvm instr kraus-map))))
              (t (error "Unable to APPLY-ALL-KRAUS-OPERATORS: expected two or fewer qubits, but got ~A" (length qubits))))))


(defmethod transition ((qvm approx-qvm) (instr quil:gate-application))
  "For the current instruction in the program, apply the gate corresponding to that
instruction, and then apply all the kraus operators for all the sources of noise following
that instruction. "
  (let ((gate   (pull-teeth-to-get-a-gate instr))
        (params (mapcar (lambda (p) (force-parameter p qvm))
                        (quil:application-parameters instr)))
        (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
    (apply-all-kraus-operators qvm instr qubits)
    (incf (pc qvm))
    qvm))


(defmethod apply-classical-readout-noise ((qvm approx-qvm) (instr quil:measure))
  (%corrupt-readout qvm instr (readout-povms qvm)))


(defun tphi (t1 t2)
  "Calculate t_phi from t1 and t2.
   t_phi = (2*t1*t2) / (2*t1 + t2)"
  (/ (* 2 (* t1 t2)) (+ t2 (* 2 t1))))


(defun generate-damping-kraus-ops (t1 gate-time)
  "Given a value for t1 and a gate time, generates the kraus operators corresponding
to the t1 noise. "
  (let* ((prob (/ gate-time t1)) 
         (k0 (magicl:make-complex-matrix 2 2 (list 0 0 (sqrt prob) 0)))
         (k1 (magicl:make-complex-matrix 2 2 (list 1 0 0  (sqrt (- 1 prob))))))
    (list k0 k1)))


(defun generate-dephasing-kraus-ops (tphi gate-time)
  "Given a value for t_phi (dephasing time) and a gate time, calculates the kraus
operators corresponding to the dephasing noise."
  (let*  ((prob (/ gate-time tphi))
          (prob-k1 (/ prob 2))
          (prob-k0 (- 1 prob-k1 ))
          (kraus-ops (loop :for mat :in '("I" "Z")
                           :for pj :in (list prob-k0 prob-k1)
                           :collect (magicl:scale (sqrt pj)
                                                  (quil:gate-matrix
                                                   (quil:gate-definition-to-gate
                                                    (quil:lookup-standard-gate mat)))))))
    kraus-ops)) 


;; TODO: Need to double check how damping dephasing kraus ops are calculated before just
;; combining it into generate-dephasing-kraus-ops. For now I am pretty sure the calculations
;; are the same. 
(defun generate-damping-dephasing-kraus-ops (t2 gate-time)  
  "Calculates the kraus operators for dephasing and damping noise from t2, which
is decoherence time. "
  (generate-dephasing-kraus-ops t2 gate-time)) 


(defun kraus-kron (k1s k2s)
  "Calculate the tensor product of two kraus maps by tensoring their elems. If one of
the kraus maps is nil, tensor the other with the identity matrix. "

  (cond ((not k1s) (loop :for k in k2s 
                         :collect (magicl:kron (magicl:make-identity-matrix 2) k )))
        ((not k2s) (loop :for k in k1s 
                         :collect (magicl:kron k (magicl:make-identity-matrix 2))))
        (t  (loop :for k1 in k1s :append (loop :for k2 in k2s
                                               :collect (magicl:kron k1 k2))))))

