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


(defmethod set-qubit-t1 (qvm qubit t1)
  "Save the kraus operators for the t1 provided on the specified qubit."
  (setf (gethash qubit (t1-ops qvm)) (generate-damping-kraus-ops t1 (avg-gate-time qvm))))


(defmethod set-qubit-t2 (qvm qubit t2)
  "Save the kraus operators for the t2 provided on the specified qubit."
  (setf (gethash qubit (t2-ops qvm)) (generate-damping-dephasing-kraus-ops t2 (avg-gate-time qvm))))


(defmethod set-qubit-fro (qvm qubit fro)
  "Save the readout povms for the fro provided on the specified qubit. The fR0 is assumed to be the average readout fidelity (avg of p(1|1) and p(0|0)) so as an approximation, we set p(0|0) = p(1|1) = fR0 in the readout povm assignment probabilities. "
  (setf (gethash qubit (readout-povms qvm)) (list fro (- 1.0d0 fro) (- 1.0d0 fro) fro)))


; For now just use a single gate time value for all calculations
(defmethod set-gate-time ((qvm approx-qvm) gate-time)
  "Set the gate time slot for approx-qvm. This value should represent the average gate time of the gates that will be run."
  (setf (avg-gate-time qvm) gate-time))


(defmethod apply-all-kraus-operators ((qvm approx-qvm) (instr quil:gate-application) qubits)
  "Apply all the kraus operators (for all of the noise sources) to the state of the program. If we have just encountered a 2qubit gate, we need to tensor the kraus operators for each qubit's error source. Currently, this does not support >2 qubit gates. Note: if the current instr is a 2 qubit gate and there is only kraus ops for one of the qubits, then no error is applied. For example, if q0 has T1 error associated with it but q1 does not, no error channel is applied after a CNOT 0 1. "
  (loop :for source :in (list (t1-ops qvm) (t2-ops qvm))
        :do (cond ((= (list-length qubits) 1) 
                   (let ((kraus-map (gethash (nth 0 qubits) source)))
                     (when kraus-map (check-kraus-ops kraus-map) (apply-kraus-ops qvm instr kraus-map) )))
                  ((= (list-length qubits) 2) 
                   (let ((kraus-map  (kraus-kron (gethash (nth 0 qubits) source) 
                                                 (gethash (nth 1 qubits) source))))
                     (when kraus-map (check-kraus-ops kraus-map) (apply-kraus-ops qvm instr kraus-map))))              
                  ((> (list-length qubits) 2) 
                   (error "Unable to APPLY-ALL-KRAUS-OPERATORS: expected two or less qubits, but got ~A" (list-length qubits))))))


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

(defun generate-damping-dephasing-kraus-ops (t2 gate-time)  
  "Calculates the kraus operators for dephasing and damping noise from t2, which
is decoherence time. "
  (generate-dephasing-kraus-ops t2 gate-time)) 


(defun kraus-kron (k1s k2s)
  "Calculate the tensor product of two kraus maps by tensoring their elems."
  (loop :for x in k1s
        :append (loop :for y in k2s
                      :collect (magicl:kron x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-test-approx ()
  (let* ((ones 0)
         (qvm (make-instance 'approx-qvm :number-of-qubits 2 :avg-gate-time 1))
         (numshots 100)
         (program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program  (quil:parse-quil program)) )
    (load-program qvm parsed-program :supersede-memory-subsystem t)
    (set-qubit-t1 qvm 0 5)
    (set-qubit-t1 qvm 1 4)
    (set-qubit-t2 qvm 0 3)
    (set-qubit-t2 qvm 1 5)
    (run qvm))
  )

(defun test-approx ()
  "Given a value for t1 and a gate time, generates the kraus operators corresponding
to the t1 noise. "
  (let ((ones 0)
        (qvm (make-instance 'approx-qvm :number-of-qubits 1 :t1 5 :avg-gate-time 1))
        (numshots 100)
        (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
    (loop :repeat numshots
          :do (bring-to-zero-state (amplitudes qvm))
          :do (incf ones (do-noisy-measurement qvm 0 program)))
    (format t "result: ~a" (float (/ ones numshots)))))


(defun test-approx-fro ()
  "Given a value for t1 and a gate time, generates the kraus operators corresponding to the t1 noise. "
  (let* ((ones 0)
         (qubit 0)
         (qvm (make-instance 'approx-qvm :number-of-qubits 1 :avg-gate-time 1))
         (numshots 100)
         (program "DECLARE R0 BIT; X 0;  MEASURE 0 R0"))
    
    (set-qubit-fro qvm qubit .9d0)
    (loop :repeat numshots
          :do (bring-to-zero-state (amplitudes qvm))
          :do (incf ones (do-noisy-measurement qvm 0 program)))
    (format t "result: ~a" (float (/ ones numshots)))))


(defun fro-map (qubit fro)
  (setf m (make-hash-table))
  (setf (gethash qubit m) fro)
  m)


(defun make-noisy-approx-qvm ()
  (let* ((qvm (make-instance 'approx-qvm :number-of-qubits 2 :avg-gate-time 1)))
    (set-qubit-t1 qvm 0 5)
    (set-qubit-t2 qvm 0 4)
    (set-qubit-t1 qvm 1 2)
    (set-qubit-t2 qvm 1 3)))
