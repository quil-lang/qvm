(in-package #:qvm-tests)


(deftest test-approx-qvm-make-instance ()
  (let* ((avg-gate-time 1)
         (t1 (qvm::generate-damping-kraus-map 5 avg-gate-time))
         (t2 (qvm::generate-dephasing-kraus-map 6 avg-gate-time))
         (depol (qvm::generate-depolarizing-kraus-map .2))
         (good-povm '(.5d0 .5d0 .5d0 .5d0))
         (bad-povm '(.9d0 .2d0 .4d0 .9d0))
         (t1-ops (make-hash-table))
         (t2-ops (make-hash-table))
         (depol-ops (make-hash-table))
         (povms (make-hash-table)))
    (setf (gethash 0 t1-ops) t1)
    (setf (gethash 1 t2-ops) t2)
    (setf (gethash 0 depol-ops) depol)
    (setf (gethash 0 povms) good-povm)
    (make-instance 'qvm::approx-qvm :number-of-qubits 2 
                                    :t1-ops t1-ops 
                                    :t2-ops t2-ops
                                    :depol-ops depol-ops
                                    :readout-povms povms
                                    :avg-gate-time avg-gate-time)
    (setf (gethash 0 t1-ops) '(4 5 3 6))
    (signals error  
      (make-instance 'qvm::approx-qvm :number-of-qubits 2
                                      :t1-ops t1-ops
                                      :t2-ops t2-ops
                                      :readout-povms povms
                                      :avg-gate-time avg-gate-time))
    (setf (gethash 1 povms) bad-povm)
    (signals error  
      (make-instance 'qvm::approx-qvm :number-of-qubits 2
                                      :t1-ops t1-ops
                                      :t2-ops t2-ops
                                      :readout-povms povms
                                      :avg-gate-time avg-gate-time))))


(deftest test-approx-qvm ()
  ;; Test that the correct kraus operators are set for the correct
  ;; qubits when building an APPROX-QVM.
  (let* ((approx-qvm (make-instance 'qvm::approx-qvm :number-of-qubits 2 :avg-gate-time 1))
         (program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program)))
    (load-program approx-qvm parsed-program :supersede-memory-subsystem t)
    (setf (qvm::qubit-t1 approx-qvm 0) 5
          (qvm::qubit-t2 approx-qvm 0) 4
          (qvm::qubit-t1 approx-qvm 1) 2
          (qvm::qubit-t2 approx-qvm 1) 3)
    (let ((t1-q0 (qvm::generate-damping-kraus-map 5 1))
          (t1-q1 (qvm::generate-damping-kraus-map 2 1))
          (t2-q0 (qvm::generate-damping-dephasing-kraus-map 4 1))
          (t2-q1 (qvm::generate-damping-dephasing-kraus-map 3 1)))
      (is (= 1 (qvm::avg-gate-time approx-qvm)))
      (is (every #'cl-quil::matrix-equality t1-q0 (gethash 0 (qvm::t1-ops approx-qvm))))
      (is (every #'cl-quil::matrix-equality t1-q1 (gethash 1 (qvm::t1-ops approx-qvm))))
      (is (every #'cl-quil::matrix-equality t2-q0 (gethash 0 (qvm::t2-ops approx-qvm))))
      (is (every #'cl-quil::matrix-equality t2-q1 (gethash 1 (qvm::t2-ops approx-qvm)))))))


(deftest test-run-approx-qvm ()
  ;; Make sure the approx-qvm can run a simple program without errors.
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
  ;; Test that the tensor product of kraus operators is properly
  ;; calculated.
  (let* ((k1 (qvm::generate-damping-kraus-map 2 5))
         (k2 (qvm::generate-damping-kraus-map 3 7))
         (kron (qvm::kraus-kron k1 k2)))
    (loop :for k in kron
          :do (is (magicl::matrix-rows (nth 0 kron)) 4)
          :do (is (magicl::matrix-cols (nth 0 kron)) 4))
    (is (= (length kron) 4)))
  (let* ((k (qvm::generate-damping-kraus-map 2 5))
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
    (is (null (qvm::kraus-kron nil nil))))) 


(deftest test-approx-qvm-readout-noise ()
  ;; Test that the readout noise is correctly applied to an
  ;; APPROX-QVM. Test by applying a program 100 times and evaluating
  ;; the resulting excited state population.
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


(deftest test-approx-qvm-t1-noise ()
  ;; Test that the t1 noise is correctly applied to an
  ;; APPROX-QVM. Test by applying a program 100 times and evaluating
  ;; the resulting excited state population.
  (with-execution-modes (:interpret)
    (let* ((ones 0)
           (qubit 0)
           (approx-qvm (make-instance 'qvm::approx-qvm :number-of-qubits 1 :avg-gate-time .2))
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0")
           (parsed-program (quil:parse-quil program)))
      (setf (qvm::qubit-t1 approx-qvm 0) 4)
      (load-program approx-qvm parsed-program :supersede-memory-subsystem t)
      (loop :repeat numshots
            :do (qvm::bring-to-zero-state (qvm::amplitudes approx-qvm))
            :do (run approx-qvm)
            :do (incf ones (qvm::dereference-mref approx-qvm (quil:mref "R0" qubit))))
      (is (< 85 ones numshots)))))


(deftest test-approx-qvm-t2-noise ()
  ;; Test that the T2 noise is correctly applied to an APPROX-QVM.
  ;; Test by applying a program 100 times and evaluating the resulting
  ;; excited state population.
  (with-execution-modes (:interpret)
    (let* ((ones 0)
           (qubit 0)
           (approx-qvm (make-instance 'qvm::approx-qvm :number-of-qubits 1 :avg-gate-time 1))
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0")
           (parsed-program (quil:parse-quil program)))
      (setf (qvm::qubit-t2 approx-qvm 0) 2)
      (load-program approx-qvm parsed-program :supersede-memory-subsystem t)
      (loop :repeat numshots
            :do (qvm::bring-to-zero-state (qvm::amplitudes approx-qvm))
            :do (run approx-qvm)
            :do (incf ones (qvm::dereference-mref approx-qvm (quil:mref "R0" qubit))))
      (is (= 100 ones)))))



(deftest test-approx-qvm-depol-noise ()
  ;; Test that the depolarizing noise is correctly applied to an
  ;; APPROX-QVM Test by applying a program 100 times and evaluating
  ;; the resulting excited state population.
  (with-execution-modes (:interpret)
    (let* ((ones 0)
           (qubit 0)
           (approx-qvm (make-instance 'qvm::approx-qvm :number-of-qubits 1 :avg-gate-time 1))
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0")
           (parsed-program (quil:parse-quil program)))
      (setf (qvm::qubit-depolarization approx-qvm 0) .2)
      (load-program approx-qvm parsed-program :supersede-memory-subsystem t)
      (loop :repeat numshots
            :do (qvm::bring-to-zero-state (qvm::amplitudes approx-qvm))
            :do (run approx-qvm)
            :do (incf ones (qvm::dereference-mref approx-qvm (quil:mref "R0" qubit))))
      (is (< 0 ones numshots)))))


(deftest test-generate-damping-kraus-map ()
  (let* ((gate-time (+ 1 (random 3))) ; gate-time between 1 and 4
         (t1 (+ gate-time (random 6))) ; random t1 > gate-time 
         (kraus (qvm::generate-damping-kraus-map t1 gate-time))
         (k0 (magicl:make-complex-matrix 2 2 (list 0 0 (sqrt (/ gate-time t1)) 0)))
         (k1 (magicl:make-complex-matrix 2 2 (list 1 0 0 (sqrt (- 1 (/ gate-time t1))))))
         )
    (is (cl-quil::matrix-equality (nth 0 kraus) k0))
    (is (cl-quil::matrix-equality (nth 1 kraus) k1))))


(deftest test-generate-dephasing-kraus-map ()
  (let* ((gate-time (+ 1 (random 3)))    ; gate-time between 1 and 4
         (tphi (+ gate-time (random 5))) ; tphi > gate-time
         (kraus (qvm::generate-dephasing-kraus-map tphi gate-time))
         (k0 (magicl:scale (sqrt (/ (/ gate-time tphi) 2)) (quil:gate-matrix
                                                            (quil:gate-definition-to-gate
                                                             (quil:lookup-standard-gate "I")))))
         (k1 (magicl:scale (sqrt (- 1 (/ (/ gate-time tphi) 2))) (quil:gate-matrix
                                                                  (quil:gate-definition-to-gate
                                                                   (quil:lookup-standard-gate "Z"))))))
    (is (cl-quil::matrix-equality (nth 0 kraus) k0))
    (is (cl-quil::matrix-equality (nth 1 kraus) k1))))


(deftest test-generate-depolarizing-kraus-map ()
  (let* ((prob (+ .001 (random .9))) ; random probabilitiy between .001 and .9001
         (kraus (qvm::generate-depolarizing-kraus-map prob))
         (k0 (magicl:scale (sqrt (- 1 (/ (* 3 prob) 4))) (quil:gate-matrix
                                                          (quil:gate-definition-to-gate
                                                           (quil:lookup-standard-gate "I")))))
         (k1 (magicl:scale (sqrt ( / prob 4)) (quil:gate-matrix
                                               (quil:gate-definition-to-gate
                                                (quil:lookup-standard-gate "X")))))
         (k2 (magicl:scale (sqrt ( / prob 4)) (quil:gate-matrix
                                               (quil:gate-definition-to-gate
                                                (quil:lookup-standard-gate "Y")))))
         (k3 (magicl:scale (sqrt ( / prob 4)) (quil:gate-matrix
                                               (quil:gate-definition-to-gate
                                                (quil:lookup-standard-gate "Z"))))))
    (is (cl-quil::matrix-equality (nth 0 kraus) k0))
    (is (cl-quil::matrix-equality (nth 1 kraus) k1))
    (is (cl-quil::matrix-equality (nth 2 kraus) k2))
    (is (cl-quil::matrix-equality (nth 3 kraus) k3))))
