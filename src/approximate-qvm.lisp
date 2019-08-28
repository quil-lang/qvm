(in-package #:qvm)

(defclass approx-qvm (kraus-qvm)
  ((t1
    :initarg :t1
    :accessor t1
    :initform nil ; default value
    :documentation "t1 value to simulate")
   (t2
    :initarg :t2
    :accessor t2
    :initform nil ; default value
    :documentation "t2 value to simulate")
   (kraus-ops
    :initarg :kraus-ops
    :accessor kraus-ops
    :initform ()
    :documentation "list of lists of kraus operators : ((t1 kraus ops) (t2 kraus ops) ...)"
    )
   (fros
    :initarg :fros
    :accessor fros
    :initform nil
    :documentation "noisy readout encoded as a hash table of qubit to avg readout fidelity")
   (avg-gate-time
    :initarg :avg-gate-time
    :accessor avg-gate-time
    :initform (error ":AVG-GATE-TIME is a required initarg ~ to APPROX-QVM")
    :documentation "the average gate time for a gate in this qvm")
   ))


(defmethod initialize-instance :after ((qvm approx-qvm) &rest args)
  "This function populates the kraus-ops slot. It checks for any set noise params (T1, T2, etc)
   and adds the kraus operators representing that noise to the kraus-ops slot. "
  (declare (ignore args)) 
  (format t "approx-qvm~%")
  (if (t1 qvm) 
      (setf (kraus-ops qvm) 
            (append (kraus-ops qvm) 
                    (list (generate-damping-kraus-ops (t1 qvm) (avg-gate-time qvm))))))
  (if (t2 qvm) 
      (setf (kraus-ops qvm) 
            (append (kraus-ops qvm) 
                    (list (generate-damping-dephasing-kraus-ops (t2 qvm) (avg-gate-time qvm))))))
  
  ;; readout fidelities are the average of the assignment probabilities p(0|0) and p(1|1). 
  ;; We approximate the readout povm as p(0|0) = p(1|1) = fR0, and 0(0|1) = p(1|0) = 1 - fR0.
  (if (fros qvm)
      (maphash #'(lambda (k v) (setf (gethash k (readout-povms qvm)) 
                                     (list v (- 1.0d0 v) (- 1.0d0 v) v)))
               (fros qvm))))


(defmethod apply-all-kraus-ops ((qvm approx-qvm) (instr quil:gate-application))
  "Apply all the kraus operators (for all of the noise sources) to the state of the program"
   (loop :for kops :in (kraus-ops qvm)
         :do
           (check-kraus-ops kops)
           (apply-kraus-ops qvm instr kops)))


(defmethod transition ((qvm approx-qvm) (instr quil:gate-application))
  "For the current instruction in the program, apply the gate corresponding to that
instruction, and then apply all the kraus operators for all the sources of noise following
that instruction. 
"
  (let ((gate   (pull-teeth-to-get-a-gate instr))
        (params (mapcar #'(lambda (p) (force-parameter p qvm))
                        (quil:application-parameters instr)))
        (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))
    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
    (apply-all-kraus-ops qvm instr)
    (incf (pc qvm))
    qvm))


(defun tphi (t1 t2)
  "Calculate t_phi from t1 and t2.
   t_phi = (2*t1*t2) / (2*t1 + t2)"
  (/ (* 2 (* t1 t2)) (+ t2 (* 2 t1))))


(defun generate-damping-kraus-ops (t1 gate-time)
  "Given a value for t1 and a gate time, generates the kraus operators corresponding
to the t1 noise. 
"
  (let* ((prob (/ gate-time t1)) 
        (k0 (magicl:make-complex-matrix 2 2 (list 0 0 (sqrt prob) 0)))
        (k1 (magicl:make-complex-matrix 2 2 (list 1 0 0  (sqrt (- 1 prob))))))
    (list k0 k1)))


(defun generate-dephasing-kraus-ops (tphi gate-time)
  "Given a value for t_phi (dephasing time) and a gate time, calculates the kraus
operators corresponding to the dephasing noise.
"
   (let*  ((prob (/ gate-time tphi))
           (prob-k1 (/ prob 2))
           (prob-k0 (- 1 prob-k1 ))
           (kraus-ops (loop :for mat :in '("I" "Z")
                 :for pj :in (list prob-k0 prob-k1)
                 :collect (magicl:scale (sqrt pj)
                                        (quil:gate-matrix
                                         (quil:gate-definition-to-gate
                                          (quil:lookup-standard-gate mat)))))))
     kraus-ops
     )) 

(defun generate-damping-dephasing-kraus-ops (t2 gate-time)  
  "Calculates the kraus operators for dephasing and damping noise from t2, which
is decoherence time. 
"
  (generate-dephasing-kraus-ops t2 gate-time)
  ) 
  
  

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-approx ()
  "Given a value for t1 and a gate time, generates the kraus operators corresponding
to the t1 noise. "
  (let ((ones 0)
        (qvm (make-instance 'approx-qvm :number-of-qubits 1 :t1 2 :avg-gate-time 1))
        (numshots 100)
        (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
     (loop :repeat numshots
       :do (incf ones (do-noisy-measurement qvm 0 program)))
    (format t "result: ~a" (float (/ ones numshots)))))
    
    
