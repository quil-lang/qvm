(in-package #:qvm-tests)


(defun make-test-channel-qvm ()
  (let* ((p1 (make-noise-pred (match-any-n-qubits 1 0 1) 1 "after"))
         (r1 (make-noise-rule p1 (list (generate-damping-kraus-ops 2 1))))
         (nm (make-noise-model (list r1)))
         (qvm (make-instance 'channel-qvm :number-of-qubits 2 :noise-model nm)))
    qvm))


(defun test-channel-noise-model ()
  (let* ((pred1 (make-noise-pred (match-strict-qubits 0 1) 1 "after"))
          (pred2 (make-noise-pred (match-any-gates "X" "Y") 1 "after"))
          (kraus-elems1 (list (generate-damping-kraus-ops 2 1)))
          (kraus-elems2 (list (generate-damping-dephasing-kraus-ops 3 1)))
          (rule1 (make-noise-rule pred1 kraus-elems1))
          (rule2 (make-noise-rule pred2 kraus-elems2)))
    (make-noise-model (list rule1 rule2))))


(defun simple-channel-qvm-test ()
  (let* ((qvm (make-test-channel-qvm))
         (qubit 0)
         (program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program)))
    (set-readout-povm qvm qubit (list .9d0 .1d0 .1d0 .9d0))
    (load-program qvm parsed-program :supersede-memory-subsystem t)
    (run qvm)))

