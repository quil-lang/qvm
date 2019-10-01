(in-package #:qvm)

(defclass approx-qvm (channel-qvm)
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
   (fros
    :initarg :fros
    :accessor fros
    :initform (make-hash-table :test 'eql)
    :documentation "noisy readout encoded as a hash table of qubit to avg readout fidelity")
   (avg-gate-time
    :initarg :avg-gate-time
    :accessor avg-gate-time
    :initform nil ;(error ":AVG-GATE-TIME is a required initarg ~ to APPROX-QVM")
    :documentation "the average gate time for a gate in this qvm")
   (kraus-ops
    :initarg :kraus-ops
    :accessor kraus-ops
    :initform (make-hash-table :test 'eql)
    :documentation "list of lists of kraus operators : ((t1 kraus ops) (t2 kraus ops) ...)")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table :test 'eql))
   ))


(defmethod initialize-instance :after ((qvm approx-qvm) &rest args)
  "This function populates the kraus-ops slot. It checks for any set noise params (T1, T2, etc)
   and adds the kraus operators representing that noise to the kraus-ops slot. "
  (declare (ignore args)) 
  (if (t1 qvm) 
      (setf (gethash "t1" (kraus-ops qvm))
            (generate-damping-kraus-ops (t1 qvm) (avg-gate-time qvm))))
  (if (t2 qvm) 
      (setf (gethash "t2" (kraus-ops qvm)) 
            (generate-damping-dephasing-kraus-ops (t2 qvm) (avg-gate-time qvm))))
  
  ;; readout fidelities are the average of the assignment probabilities p(0|0) and p(1|1). 
  ;; We approximate the readout povm as p(0|0) = p(1|1) = fR0, and 0(0|1) = p(1|0) = 1 - fR0.
  (if (fros qvm)
      (maphash #'(lambda (k v) (setf (gethash k (readout-povms qvm)) 
                                     (list v (- 1.0d0 v) (- 1.0d0 v) v)))
               (fros qvm))))


(defmethod set-gate-time ((qvm approx-qvm) gate-time)
  (setf (avg-gate-time qvm) gate-time)
  (set-t1 qvm (t1 qvm))
  (set-t2 qvm (t2 qvm))
  )


(defmethod set-t1 ((qvm approx-qvm) t1)
  (setf (t1 qvm) t1)
  (setf (gethash "t1" (kraus-ops qvm)) (generate-damping-kraus-ops (t1 qvm) (avg-gate-time qvm)))
  )

(defmethod set-t2 ((qvm approx-qvm) t2)
    (setf (t2 qvm) t2)
    (setf (gethash "t2" (kraus-ops qvm)) (generate-damping-dephasing-kraus-ops (t2 qvm) (avg-gate-time qvm)))

  )

(defmethod set-fro ((qvm approx-qvm) qubit fro)
    (setf (gethash qubit (fros qvm)) fro)
    (setf (gethash qubit (readout-povms qvm)) (list fro (- 1.0d0 fro) (- 1.0d0 fro) fro))
  )


(defmethod apply-all-kraus-ops ((qvm approx-qvm) (instr quil:gate-application))
  "Apply all the kraus operators (for all of the noise sources) to the state of the program"
  
  (loop for kops being each hash-value of (kraus-ops qvm)
        :do
           (check-kraus-ops kops)
           (apply-kraus-ops qvm instr kops)
        ))


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


(defun %corrupt-approx-qvm (qvm instr povm)
  (check-type instr quil:measure)
  (let* ((q (quil:qubit-index (quil:measurement-qubit instr)))
         (a (quil:measure-address instr))
         (c (dereference-mref qvm a))
         (povm (gethash q (readout-povms qvm))))
    (when povm
      (destructuring-bind (p00 p01 p10 p11) povm
        (setf (dereference-mref qvm a)
              (perturb-measurement c p00 p01 p10 p11))))))


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
    
    
(defun test-approx-fro ()
  "Given a value for t1 and a gate time, generates the kraus operators corresponding
to the t1 noise. "
  (let* ((ones 0)
        (qubit 0)
        (qvm (make-instance 'approx-qvm :number-of-qubits 1 :fros (fro-map qubit .9d0)))
        (numshots 100)
        (program "DECLARE R0 BIT; X 0;  MEASURE 0 R0"))

    (loop :repeat numshots
          :do (bring-to-zero-state (amplitudes qvm))
          :do (incf ones (do-noisy-measurement qvm 0 program)))
    (format t "result: ~a" (float (/ ones numshots)))))


(defun fro-map (qubit fro)
  (setf m (make-hash-table))
  (setf (gethash qubit m) fro)
  m)
