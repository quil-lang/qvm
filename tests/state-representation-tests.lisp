(in-package #:qvm-tests)

(deftest test-make-pure-state ()
  (let* ((num-qubits (+ 1 (random 8)))
        (ps (qvm::make-pure-state num-qubits)))
    (is (= (length (qvm::amplitudes ps)) (expt 2 num-qubits)))
    (is (= (length (qvm::%trial-amplitudes ps)) (expt 2 num-qubits))))
  (signals error
    (qvm::make-pure-state 1.5)))

(deftest test-make-density-matrix-state ()
  (let* ((num-qubits (+ 1 (random 8)))
         (dms (qvm::make-density-matrix-state num-qubits)))
    (is (= (length (qvm::amplitudes dms)) (expt 2 (* 2 num-qubits))))
    (print num-qubits)
    (is (equal (list (expt 2 num-qubits) 
                     (expt 2 num-qubits)) 
               (array-dimensions (qvm::matrix-view dms)))))
  (signals error
    (qvm::make-density-matrix-state 1.5)))

(deftest test-num-qubits ()
  (let* ((num-qubits (+ 1 (random 8)))
         (ps (qvm::make-pure-state num-qubits))
         (dms (qvm::make-density-matrix-state num-qubits)))
    (is (= num-qubits (qvm::num-qubits ps)))
    (is (= num-qubits (qvm::num-qubits dms)))))

;;; State operations on PURE-STATE and DENSITY-MATRIX-STATE

(deftest test-pure-state-apply-gate ()
  ;; produce the bell state with quantum gates and check that the
  ;; amplitudes of the state are correct
  (let* ((state (qvm::make-pure-state 2))
         (h (qvm::pull-teeth-to-get-a-gate (quil::build-gate "H" () 0)))
         (cnot (qvm::pull-teeth-to-get-a-gate  (quil::build-gate "CNOT" () 0 1))))
    (qvm::apply-gate-state h state (list 0))
    (qvm::apply-gate-state cnot state (list 0 1))
    (is (= #C(0.7071067811865475d0 0.0d0) (aref (qvm::amplitudes state) 0)))
    (is (= #C(0.7071067811865475d0 0.0d0) (aref (qvm::amplitudes state) 3)))))

(deftest test-density-matrix-state-apply-gate ()
  ;; produce the bell state with quantum gates and check that the
  ;; amplitudes of the state are correct.
  (let* ((state (qvm::make-density-matrix-state 2))
         (h (qvm::pull-teeth-to-get-a-gate (quil::build-gate "H" () 0)))
         (cnot (qvm::pull-teeth-to-get-a-gate  (quil::build-gate "CNOT" () 0 1)))
         (expected-amplitude-entry #C(0.4999999999999999d0 0.0d0)))
    (qvm::apply-gate-state h state (list 0))
    (qvm::apply-gate-state cnot state (list 0 1))
    (is (= expected-amplitude-entry (aref (qvm::amplitudes state) 0)))
    (is (= expected-amplitude-entry (aref (qvm::amplitudes state) 3)))
    (is (= expected-amplitude-entry (aref (qvm::amplitudes state) 12)))
    (is (= expected-amplitude-entry (aref (qvm::amplitudes state) 15)))))

(deftest test-qvm-with-superoperator ()
  ;; Replace the I 0 instruction with a depolarizing
  ;; superoperator. This superoperator will cause the PURE-STATE of
  ;; the qvm to depolarize, which is noticeable when repeating a
  ;; measurement multiple times.
  (let* ((qvm (qvm::make-qvm 2))
         (program "DECLARE R0 BIT; I 0; MEASURE 0 R0")
         (numshots 100))
    (qvm::set-superoperator qvm "I" '(0) (qvm::depolarizing-kraus-map .5))
    (let ((ones-measured (qvm-tests::run-n-shot-program numshots qvm program)))
      (is (< ones-measured numshots)))))

(deftest test-mixed-state-qvm-with-superoperator ()
  ;; Replace the I 0 instruction with a depolarizing
  ;; superoperator. This superoperator will cause the PURE-STATE of
  ;; the qvm to depolarize, which is noticeable when repeating a
  ;; measurement multiple times.
  (let* ((qvm (qvm::make-mixed-state-qvm 2))
         (program "DECLARE R0 BIT; I 0; MEASURE 0 R0")
         (numshots 100))
    (qvm::set-superoperator qvm "I" '(0) (qvm::depolarizing-kraus-map .5))
    (let ((ones-measured (qvm-tests::run-n-shot-program numshots qvm program)))
      (is (< ones-measured numshots)))))

(deftest test-noise-qvms-with-superoperator ()
  ;; Check that superoperators get applied to CHANNEL-QVM and
  ;; BASIC-NOISE-QVM.  
  ;; CHANNEL-QVM:
  (let* ((num-qubits 2)
         (pure-state (qvm::make-pure-state num-qubits))
         (density-matrix-state (qvm::make-density-matrix-state num-qubits))
         (ps-channel-qvm (make-instance 'channel-qvm :number-of-qubits num-qubits
                                                     :state pure-state))
         (dms-channel-qvm (make-instance 'channel-qvm :number-of-qubits num-qubits
                                                      :state density-matrix-state))
         (program "DECLARE R0 BIT; I 0; MEASURE 0 R0")
         (numshots 100))
    (qvm::set-superoperator ps-channel-qvm "I" '(0) (qvm::depolarizing-kraus-map .5))
    (qvm::set-superoperator dms-channel-qvm "I" '(0) (qvm::depolarizing-kraus-map .5))

    (let ((ones-measured-on-pure-state (qvm-tests::run-n-shot-program 
                                        numshots 
                                        ps-channel-qvm 
                                        program))
          (ones-measured-on-dms (qvm-tests::run-n-shot-program
                                 numshots 
                                 dms-channel-qvm 
                                 program)))
      (is (< ones-measured-on-pure-state numshots))
      (is (< ones-measured-on-dms numshots))))
  
  ;; BASIC-NOISE-QVM
  (let* ((num-qubits 2)
         (pure-state (qvm::make-pure-state num-qubits))
         (density-matrix-state (qvm::make-density-matrix-state num-qubits))
         (ps-basic-noise-qvm (make-instance 'basic-noise-qvm :number-of-qubits num-qubits
                                                             :state pure-state
                                                             :avg-gate-time 1))
         (dms-basic-noise-qvm (make-instance 'basic-noise-qvm :number-of-qubits num-qubits
                                                              :state density-matrix-state
                                                              :avg-gate-time 1))
         (program "DECLARE R0 BIT; I 0; MEASURE 0 R0")
         (numshots 100))
    (qvm::set-superoperator ps-basic-noise-qvm "I" '(0) (qvm::depolarizing-kraus-map .5))
    (qvm::set-superoperator dms-basic-noise-qvm "I" '(0) (qvm::depolarizing-kraus-map .5))

    (let ((ones-measured-on-pure-state (qvm-tests::run-n-shot-program 
                                        numshots 
                                        ps-basic-noise-qvm 
                                        program))
          (ones-measured-on-dms (qvm-tests::run-n-shot-program
                                 numshots 
                                 dms-basic-noise-qvm 
                                 program)))
      (is (< ones-measured-on-pure-state numshots))
      (is (< ones-measured-on-dms numshots)))))
   
;; Mixed State Tests

(defun density-matrix-trace (qvm)
  "Compute the trace of the density matrix associated with density qvm."
  (let ((sum (flonum 0))
        (density-matrix (qvm::density-matrix-view qvm)))
    ;; This is a sum along the diagonal of the density-matrix
    (dotimes (i (expt 2 (qvm::number-of-qubits qvm)) sum)
      (incf sum (realpart (aref density-matrix i i))))))


(defun density-matrix-purity (qvm)
  "Compute the purity aka tr(ρ^2)."
  (let* ((n (expt 2 (number-of-qubits qvm)))
         (density-mat (magicl:make-matrix :rows n :cols n :data (qvm::amplitudes qvm)))
         (squared     (magicl:multiply-complex-matrices density-mat density-mat)))
    (realpart (reduce #'+ (magicl:matrix-diagonal squared)))))

(deftest test-mixed-state-qvm-parametric-gate ()
  "Density qvm can apply parametric gates."
    (let* ((qvm (qvm::make-mixed-state-qvm 1)))
      (load-program qvm (with-output-to-quil
                          "DEFGATE G(%a):"
                          "    cos(%a), sin(%a)"
                          "    -sin(%a), cos(%a)"
                          "G(0.0) 0"))
      (run qvm)
      (is (double-float= 1 (realpart (aref (qvm::density-matrix-view qvm) 0 0)) 1/10000))))

(deftest test-mixed-state-qvm-force-measurement-1q ()
  "Measurement on 1q density matrix qvm behaves as expected."
  (let ((qvm (qvm::make-mixed-state-qvm 1)))
    (qvm::load-program qvm (with-output-to-quil "H 0"))
    (run qvm)
    (qvm::force-measurement 1 0 (qvm::state qvm) 0.5)
    (is (double-float= 1 (density-matrix-trace qvm)))
    (is (double-float= 1 (realpart
                          (aref (qvm::density-matrix-view qvm) 1 1))))))

(deftest test-mixed-state-qvm-force-measurement-4q ()
  "Measurement on 4q density matrix qvm is trace preserving."
  (let ((qvm (qvm::make-mixed-state-qvm 4))
        (pp (with-output-to-quil
              "H 0"
              "CNOT 0 1"
              "CNOT 1 3"
              "H 3")))
    (load-program qvm pp)
    (run qvm)
    (is (qvm-tests::double-float= 1 (density-matrix-trace qvm)))
    (let ((p (qvm::get-excited-state-probability (qvm::state qvm) 3)))
      (qvm::force-measurement 1 3 (qvm::state qvm) p)
      (is (qvm-tests::double-float= 1 (density-matrix-trace qvm))))))

(defun load-density-from-matrix (qvm mat)
  "Overwrites the density matrix of the density-qvm QVM with values from the magicl matrix MAT."
  (check-type mat magicl:matrix)
  (assert (equal (array-dimensions (qvm::density-matrix-view qvm))
                 (list (magicl:matrix-rows mat) (magicl:matrix-cols mat)))
          (mat)
          "Density matrix is of wrong size")
  (let ((density-matrix (qvm::density-matrix-view qvm)))
    (destructuring-bind (rows cols) (array-dimensions density-matrix)
      (dotimes (i rows qvm)
        (dotimes (j cols)
          (setf (aref density-matrix i j)
                (cflonum (magicl:ref mat i j))))))))

(defun make-1q-mixed-state-qvm (p)
  (check-type p (real 0 1))
  (let ((qvm (qvm::make-mixed-state-qvm 1))
        (mat (magicl:diag 2 2
                          (list (- 1 p) p))))
    (load-density-from-matrix qvm mat)))

(defun make-2q-with-1q-mixed-state-qvm (p)
  (check-type p (real 0 1))
  (let* ((qvm (qvm::make-mixed-state-qvm 2))
         (q   (- 1 p))
         (mat1 (magicl:diag 2 2 (list q p)))
         (mat2 (magicl:make-complex-matrix 2 2 '(0.5 0.5 0.5 0.5)))
         (mat (magicl:kron mat1 mat2)))
    (load-density-from-matrix qvm mat)))

(deftest test-mixed-state-qvm-1q-purity ()
  "Tests that unitary evolution preserves purity."
  (dolist (p '(0.1 0.4 0.5 0.6 0.9))
    (let ((expected-purity (+ (expt (- 1 p) 2) (expt p 2)))
          (qvm (make-1q-mixed-state-qvm p)))
      (is (double-float= (density-matrix-purity qvm) expected-purity))
      (load-program qvm (with-output-to-quil "H 0"))
      (run qvm)
      (is (double-float= (density-matrix-purity qvm) expected-purity)))))

(deftest test-mixed-state-qvm-2q-purity ()
  "Tests that unitary evolution preserves purity."
  (dolist (p '(0.1 0.4 0.5 0.6 0.9))
    (let ((expected-purity (+ (expt (- 1 p) 2) (expt p 2)))
          (qvm (make-2q-with-1q-mixed-state-qvm p)))
      (is (double-float= (density-matrix-purity qvm) expected-purity))
      (load-program qvm (with-output-to-quil "CNOT 0 1"))
      (run qvm)
      (is (double-float= (density-matrix-purity qvm) expected-purity)))))

(deftest test-mixed-state-qvm-1q-measure-discard ()
  "Test that measure discard behaves as expected."
  ;; We prepare the pure state (1/sqrt(2))(|0> - |1>). If we were to
  ;; measure and record the outcome, we would know with certainty
  ;; which of |0> or |1> the state had collapsed into. If we discard
  ;; that outcome (in some sense) then we should find ourselves in the
  ;; mixed state |ψ> = 1/2 (|0><0| + |1><1|)
  (let ((p (quil:parse-quil "H 0
MEASURE 0"))
        (qvm (qvm::make-mixed-state-qvm 1)))
    (load-program qvm p)
    (run qvm)
    (let ((mat (qvm::density-matrix-view qvm)))
      (is (double-float= (realpart (aref mat 1 1)) 0.5))
      (is (double-float= (realpart (aref mat 0 0)) 0.5)))))
