(in-package #:qvm-tests)


(defun simple-test-approx ()
  (let* ((ones 0)
         (qvm (make-instance 'approx-qvm :number-of-qubits 2 :avg-gate-time 1))
         (numshots 100)
         (program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program  (quil:parse-quil program)) )
    (load-program qvm parsed-program :supersede-memory-subsystem t)
    (setf (qubit-t1 qvm 0) 5
          (qubit-t2 qvm 0) 4
          (qubit-t1 qvm 1) 2
          (qubit-t2 qvm 1) 3)
    (run qvm))
  )


(defun test-approx-fro ()
  "Given a value for t1 and a gate time, generates the kraus operators corresponding to the t1 noise. "
  (let* ((ones 0)
         (qubit 0)
         (qvm (make-instance 'approx-qvm :number-of-qubits 1 :avg-gate-time 1))
         (numshots 100)
         (program "DECLARE R0 BIT; X 0;  MEASURE 0 R0"))
    (setf (qubit-fro qvm qubit) .9d0)
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
    (setf (qubit-t1 qvm 0) 5
          (qubit-t2 qvm 0) 4
          (qubit-t1 qvm 1) 2
          (qubit-t2 qvm 1) 3)))
