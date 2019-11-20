;;;; basic-noise-qvm-tests.lisp
;;;;
;;;; Author: Sophia Ponte

(in-package #:qvm-tests)

(deftest test-basic-noise-qvm-make-instance ()
  (let* ((avg-gate-time 1)
         (good-povm '(.5d0 .5d0 .5d0 .5d0))
         (bad-povm '(.9d0 .2d0 .4d0 .9d0))
         (t1-vals (make-hash-table))
         (t2-vals (make-hash-table))
         (tphi-vals (make-hash-table))
         (depolarization-ops (make-hash-table))
         (povms (make-hash-table)))
    (setf (gethash 1 t1-vals) 5.6d0)
    (setf (gethash 1 t2-vals) 6.4d0)
    (setf (gethash 0 depolarization-ops) (qvm::depolarizing-kraus-map .9d0))
    (setf (gethash 0 t1-vals) 5.3d0)
    (setf (gethash 0 tphi-vals) 2.2d0)
    (setf (gethash 0 povms) good-povm)
    (make-instance 'qvm::basic-noise-qvm :number-of-qubits 2
                                         :t1-vals t1-vals
                                         :t2-vals t2-vals
                                         :tphi-vals tphi-vals
                                         :depolarization-ops depolarization-ops
                                         :readout-povms povms
                                         :avg-gate-time avg-gate-time)
    (setf (gethash 0 t1-vals) '(4 5 3 6))
    (signals error
      (make-instance 'qvm::basic-noise-qvm :number-of-qubits 2
                                           :t1-vals t1-vals
                                           :t2-vals t2-vals
                                           :readout-povms povms
                                           :avg-gate-time avg-gate-time))
    (setf (gethash 1 povms) bad-povm)
    (signals error
      (make-instance 'qvm::basic-noise-qvm :number-of-qubits 2
                                           :t1-vals t1-vals
                                           :t2-vals t2-vals
                                           :readout-povms povms
                                           :avg-gate-time avg-gate-time))
    (setf (gethash 1 depolarization-ops) .9d0)
    (signals error
      (make-instance 'qvm::basic-noise-qvm :number-of-qubits 2
                                           :depolarization-ops depolarization-ops
                                           :avg-gate-time avg-gate-time))))

(deftest test-basic-noise-qvm ()
  ;; Test that the correct kraus operators are set for the correct
  ;; qubits when building an BASIC-NOISE-QVM.
  (let* ((basic-noise-qvm (make-instance 'qvm::basic-noise-qvm :number-of-qubits 2 :avg-gate-time 1))
         (program "DECLARE R0 BIT; X 0; CNOT 0 1; MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (depol-ops (qvm::depolarizing-kraus-map .9d0)))
    (load-program basic-noise-qvm parsed-program :supersede-memory-subsystem t)
    (setf (qvm::qubit-t1 basic-noise-qvm 0) 5
          (qvm::qubit-t2 basic-noise-qvm 0) 4
          (qvm::qubit-t1 basic-noise-qvm 1) 2
          (qvm::qubit-t2 basic-noise-qvm 1) 3
          (qvm::qubit-depolarization basic-noise-qvm 0) .9d0)
    (is (= 1 (qvm::avg-gate-time basic-noise-qvm)))
    (is (= 5 (gethash 0 (qvm::t1-vals basic-noise-qvm))))
    (is (= 4 (gethash 0 (qvm::t2-vals basic-noise-qvm))))
    (is (= 2 (gethash 1 (qvm::t1-vals basic-noise-qvm))))
    (is (= 3 (gethash 1 (qvm::t2-vals basic-noise-qvm))))
    (is (every #'cl-quil::matrix-equality (gethash 0 (qvm::depolarization-ops basic-noise-qvm)) depol-ops))))

(deftest test-run-basic-noise-qvm ()
  ;; Make sure the basic-noise-qvm can run a simple program without
  ;; errors, and that the time is properly elapsed.
  (let* ((basic-noise-qvm (make-instance 'qvm::basic-noise-qvm :number-of-qubits 1 :avg-gate-time 1))
         (program "DECLARE R0 BIT; X 0; Z 0; MEASURE 0 R0")
         (parsed-program (quil:parse-quil program)))
    (setf (qvm::qubit-fro basic-noise-qvm 0) .9d0)
    (qvm:load-program basic-noise-qvm parsed-program :supersede-memory-subsystem t)
    (run basic-noise-qvm)
    (is (= 2 (qvm::elapsed-time basic-noise-qvm)))
    (run basic-noise-qvm)
    (is (= 2 (qvm::elapsed-time basic-noise-qvm)))))

(deftest test-tphi-calc ()
  ;; Test that T-PHI is properly calculated.
  (let* ((t1 12)
         (t2 16)
         (tphi (qvm::tphi t1 t2)))
    (is (= 9.6d0 tphi)))
  (signals error (qvm::tphi 0 1))
  (signals error (qvm::tphi 0 0))
  (signals error (qvm::tphi 10 30))     ; T2 has to be < 2 * T1
  (signals error (qvm::tphi 10 20))
  (signals error (qvm::tphi -3 -4)))

(deftest test-kraus-kron ()
  ;; Test that the tensor product of kraus operators is properly
  ;; calculated.
  (let* ((k1 (qvm::damping-kraus-map 2 5))
         (k2 (qvm::damping-kraus-map 3 7))
         (kron (qvm::kraus-kron k1 k2)))
    (loop :for k in kron
          :do (is (= 4 (magicl::matrix-rows (nth 0 kron))))
          :do (is (= 4 (magicl::matrix-cols (nth 0 kron)))))
    (is (= 4 (length kron))))
  (let* ((k (qvm::damping-kraus-map 2 5))
         (kron-first (qvm::kraus-kron k nil))
         (kron-second (qvm::kraus-kron nil k)))
    (loop :for elem in kron-first
          :do (is (= 4  (magicl::matrix-rows elem)))
          :do (is (= 4 (magicl::matrix-cols elem))))
    (loop :for elem in kron-second
          :do (is (= 4 (magicl::matrix-rows elem)))
          :do (is (= 4 (magicl::matrix-cols elem))))
    (is (= 2 (length kron-first)))
    (is (= 2 (length kron-second)))
    (is (null (qvm::kraus-kron nil nil)))))

(deftest test-basic-noise-qvm-readout-noise ()
  ;; Test that the readout noise is correctly applied to an
  ;; BASIC-NOISE-QVM. Test by applying a program 100 times and evaluating
  ;; the resulting excited state population.
  (with-execution-modes (:interpret)
    (let* ((q (make-instance 'qvm::basic-noise-qvm :number-of-qubits 1 :avg-gate-time 1))
           (numshots 100)
           (qubit 0)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
      (setf (qvm::qubit-fro q qubit) .9d0)
      (let ((ones-measured (qvm-tests::run-n-shot-program numshots q program)))
        (is (< 50 ones-measured numshots))))))

(deftest test-basic-noise-qvm-t1-noise ()
  ;; Test that the t1 noise is correctly applied to an
  ;; BASIC-NOISE-QVM. Test by applying a program 100 times and evaluating
  ;; the resulting excited state population.
  (with-execution-modes (:interpret)
    (let* ((qubit 0)
           (q (make-instance 'qvm::basic-noise-qvm :number-of-qubits 1 :avg-gate-time .2))
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
      (setf (qvm::qubit-t1 q qubit) 1)
      (let ((ones-measured (qvm-tests::run-n-shot-program numshots q program)))
        (is (< ones-measured numshots))))))

(deftest test-basic-noise-qvm-t1-t2-noise ()
  ;; Test that the T1 and T2 noise is correctly applied to an
  ;; BASIC-NOISE-QVM.  Test by applying a program 100 times and
  ;; evaluating the resulting excited state population.
  (with-execution-modes (:interpret)
    (let* ((qubit 0)
           (q (make-instance 'qvm::basic-noise-qvm :number-of-qubits 1 :avg-gate-time 1))
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
      (setf (qvm::qubit-t1 q qubit) 2)
      (setf (qvm::qubit-t2 q qubit) 1.5)
      (let ((ones-measured (qvm-tests::run-n-shot-program numshots q program)))
        (is (< ones-measured numshots))))))

(deftest test-basic-noise-qvm-depol-noise ()
  ;; Test that the depolarizing noise is correctly applied to an
  ;; BASIC-NOISE-QVM Test by applying a program 100 times and evaluating
  ;; the resulting excited state population.
  (with-execution-modes (:interpret)
    (let* ((qubit 0)
           (q (make-instance 'qvm::basic-noise-qvm :number-of-qubits 1 :avg-gate-time 1))
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
      (setf (qvm::qubit-depolarization q qubit) .2d0)
      (let ((ones-measured (qvm-tests::run-n-shot-program numshots q program)))
        (is (< 0 ones-measured numshots))))))

(deftest test-basic-noise-qvm-with-density-matrix ()
  ;; Test that a depolarizing BASIC-NOISE-QVM
  ;; with a DENSITY-MATRIX-STATE correctly depolarizes the state.
  (let* ((num-qubits 1)
         (qubit 0)
         (depolarization-prob .5d0)
         (numshots 100)
         (density-matrix-state (qvm::make-density-matrix-state num-qubits))
         (dms-basic-noise-qvm (make-instance 'basic-noise-qvm :number-of-qubits num-qubits
                                                              :state density-matrix-state
                                                              :avg-gate-time 1))
         (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
    (setf (qvm::qubit-depolarization dms-basic-noise-qvm qubit) depolarization-prob)
    (let ((ones-measured (qvm-tests::run-n-shot-program numshots dms-basic-noise-qvm program)))
      (is (< ones-measured numshots)))))

(deftest test-damping-kraus-map ()
  ;; Test that the damping kraus map is properly constructed
  (let* ((gate-time (+ 1 (random 3))) ; gate-time between 1 and 4
         (t1 (+ gate-time (random 6))) ; random t1 > gate-time
         (kraus (qvm::damping-kraus-map t1 gate-time))
         (k0 (magicl:make-complex-matrix 2 2 (list 0 0 (sqrt (- 1 (exp (/ (- gate-time) t1)))) 0)))
         (k1 (magicl:make-complex-matrix 2 2 (list 1 0 0 (sqrt (- 1 (- 1 (exp (/ (- gate-time) t1)))))))))
    (is (cl-quil::matrix-equality (nth 0 kraus) k0))
    (is (cl-quil::matrix-equality (nth 1 kraus) k1))))

(deftest test-calculate-dephasing-noise ()
  ;; Test that dephasing noise is properly calculated from either t1
  ;; or t2.
  (let* ((elapsed-time 1)
         (tphi 5)
         (t1 6)
         (t2 8)
         (dephasing-map-from-tphi (qvm::dephasing-kraus-map tphi elapsed-time))
         (dephasing-map-from-t1-t2 (qvm::dephasing-kraus-map (qvm::tphi t1 t2) 
                                                             elapsed-time)))
    (is (every #'cl-quil::matrix-equality dephasing-map-from-tphi (qvm::calculate-dephasing-map tphi t1 t2 elapsed-time)))
    (is (every #'cl-quil::matrix-equality dephasing-map-from-t1-t2 (qvm::calculate-dephasing-map nil t1 t2 elapsed-time))))
  (let ((elapsed-time 1)
        (t1 10)
        (t2 30))
    (signals error (qvm::calculate-dephasing-map nil t1 t2 elapsed-time))))

(deftest test-dephasing-kraus-map ()
  ;; Test that the dephasing map is properly constructed.
  (let* ((gate-time (+ 1 (random 3)))    ; gate-time between 1 and 4
         (tphi (+ gate-time (random 5))) ; tphi > gate-time
         (kraus (qvm::dephasing-kraus-map tphi gate-time))
         (prob (- 1 (exp (/ (- gate-time)
                            tphi))))
         (k0 (magicl:scale (sqrt (/ prob 2)) (quil:gate-matrix
                                              (quil:gate-definition-to-gate
                                               (quil:lookup-standard-gate "I")))))
         (k1 (magicl:scale (sqrt (- 1 (/ prob  2))) (quil:gate-matrix
                                                     (quil:gate-definition-to-gate
                                                      (quil:lookup-standard-gate "Z"))))))
    (is (cl-quil::matrix-equality (nth 0 kraus) k0))
    (is (cl-quil::matrix-equality (nth 1 kraus) k1))))

(deftest test-depolarizing-kraus-map ()
  ;; Test that the depolarizing map is properly constructed.
  (let* ((prob (+ .001 (random .9))) ; random probabilitiy between .001 and .9001
         (kraus (qvm::depolarizing-kraus-map prob))
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
