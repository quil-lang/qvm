(in-package #:qvm-tests)


(deftest test-approx-qvm ()
  ;; Test that the correct kraus operators are set for the correct
  ;; qubits when building an APPROX-QVM
  (let* ((approx-qvm (make-instance 'qvm::approx-qvm :number-of-qubits 2 :avg-gate-time 1))
         (program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program)))
    (load-program approx-qvm parsed-program :supersede-memory-subsystem t)
    (setf (qvm::qubit-t1 approx-qvm 0) 5
          (qvm::qubit-t2 approx-qvm 0) 4
          (qvm::qubit-t1 approx-qvm 1) 2
          (qvm::qubit-t2 approx-qvm 1) 3)
    (let ((t1-q0 (qvm::generate-damping-kraus-ops 5 1))
          (t1-q1 (qvm::generate-damping-kraus-ops 2 1))
          (t2-q0 (qvm::generate-damping-dephasing-kraus-ops 4 1))
          (t2-q1 (qvm::generate-damping-dephasing-kraus-ops 3 1)))
      (is (= 1 (qvm::avg-gate-time approx-qvm)))
      (is (mapcar #'cl-quil::matrix-equality t1-q0 (gethash 0 (qvm::t1-ops approx-qvm))))
      (is (mapcar #'cl-quil::matrix-equality t1-q1 (gethash 1 (qvm::t1-ops approx-qvm))))
      (is (mapcar #'cl-quil::matrix-equality t2-q0 (gethash 0 (qvm::t2-ops approx-qvm))))
      (is (mapcar #'cl-quil::matrix-equality t2-q1 (gethash 1 (qvm::t2-ops approx-qvm)))))))


(deftest test-prog ()
  (let* ((approx-qvm (make-instance 'qvm::approx-qvm :number-of-qubits 1 :avg-gate-time 1))
         (program "DECLARE R0 BIT; X 0; Z 0; MEASURE 0 R0")
         (parsed-program (quil:parse-quil program)))
    (setf (qvm::qubit-fro approx-qvm 0) .9d0)
    (qvm:load-program approx-qvm parsed-program :supersede-memory-subsystem t)
    (run approx-qvm)))


(deftest test-tphi-calc ()
  ;; Test that T-PHI is properly calculated.
  (let* ((t1 12)
         (t2 40)
         (tphi (qvm::tphi t1 t2)))
    (= tphi 15)
    (is (= (qvm::tphi 0 0) 0))
    (is (= (qvm::tphi 1 0) 0))
    (is (= (qvm::tphi 0 1)) 0)))


(deftest test-kraus-kron ()
  ;; Test that the tensor product of kraus operators is properly calculated.
  (let* ((k1 (qvm::generate-damping-kraus-ops 2 5))
         (k2 (qvm::generate-damping-kraus-ops 3 7))
         (kron (qvm::kraus-kron k1 k2)))
    (loop :for k in kron
          :do (is (magicl::matrix-rows (nth 0 kron)) 4)
          :do (is (magicl::matrix-cols (nth 0 kron)) 4))
    (is (= (length kron) 4)))
  (let* ((k (qvm::generate-damping-kraus-ops 2 5))
         (kron-first (qvm::kraus-kron k nil))
         (kron-second (qvm::kraus-kron nil k)))
    (loop :for elem in kron-first
          :do (is (magicl::matrix-rows elem) 4)
          :do (is (magicl::matrix-cols elem) 4))
    (loop :for elem in kron-second
          :do (is (magicl::matrix-rows elem) 4)
          :do (is (magicl::matrix-cols elem) 4))
    (is (= (length kron-first) 2))
    (is (= (length kron-second) 2))
    (is (not (qvm::kraus-kron nil nil)))))


(deftest test-approx-qvm-readout-noise ()
  ;; Test that the readout noise is correctly applied to an
  ;; APPROX-QVM. Test by applying a program 100 times and evaluating
  ;; the resulting excited state population
  (with-execution-modes (:interpret)
    (let* ((ones 0)
           (qubit 0)
           (approx-qvm (make-instance 'qvm::approx-qvm :number-of-qubits 1 :avg-gate-time 1))
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0")
           (parsed-program (quil:parse-quil program)))
      (setf (qvm::qubit-fro approx-qvm qubit) .9d0)
      (load-program approx-qvm parsed-program :supersede-memory-subsystem t)
      (loop :repeat numshots
            :do (qvm::bring-to-zero-state (qvm::amplitudes approx-qvm))
            :do (run approx-qvm)
            :do (incf ones (qvm::dereference-mref approx-qvm (quil:mref "R0" qubit))))
      (is (< 50 ones numshots)))))
