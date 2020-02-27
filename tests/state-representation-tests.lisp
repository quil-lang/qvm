(in-package #:qvm-tests)

(deftest test-make-pure-state ()
  (let* ((num-qubits (+ 1 (random 8)))
         (ps (qvm::make-pure-state num-qubits)))
    (is (= (length (qvm::state-elements ps)) (expt 2 num-qubits)))
    (is (not (qvm::%trial-amplitudes ps))))
  (signals error
    (qvm::make-pure-state 1.5)))

(deftest test-make-density-matrix-state ()
  (let* ((num-qubits (+ 1 (random 8)))
         (dms (qvm::make-density-matrix-state num-qubits)))
    (is (= (length (qvm::state-elements dms)) (expt 2 (* 2 num-qubits))))
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
    (qvm::apply-gate-to-state h state '(0))
    (qvm::apply-gate-to-state cnot state '(0 1))
    (is (cflonum= (/ (sqrt 2) 2) (aref (qvm::state-elements state) 0)))
    (is (cflonum= (/ (sqrt 2) 2) (aref (qvm::state-elements state) 3)))))

(deftest test-density-matrix-state-apply-gate ()
  ;; produce the bell state with quantum gates and check that the
  ;; amplitudes of the state are correct.
  (let* ((state (qvm::make-density-matrix-state 2))
         (h (qvm::pull-teeth-to-get-a-gate (quil::build-gate "H" () 0)))
         (cnot (qvm::pull-teeth-to-get-a-gate  (quil::build-gate "CNOT" () 0 1)))
         (expected-amplitude-entry .5))
    (qvm::apply-gate-to-state h state '(0))
    (qvm::apply-gate-to-state cnot state '(0 1))
    (is (cflonum= expected-amplitude-entry (aref (qvm::state-elements state) 0)))
    (is (cflonum= expected-amplitude-entry (aref (qvm::state-elements state) 3)))
    (is (cflonum= expected-amplitude-entry (aref (qvm::state-elements state) 12)))
    (is (cflonum= expected-amplitude-entry (aref (qvm::state-elements state) 15)))))

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
         (ps (qvm::make-pure-state num-qubits))
         (dms (qvm::make-density-matrix-state num-qubits))
         (ps-channel-qvm (make-instance 'channel-qvm :number-of-qubits num-qubits
                                                     :state ps))
         (dms-channel-qvm (make-instance 'channel-qvm :number-of-qubits num-qubits
                                                      :state dms))
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
         (ps (qvm::make-pure-state num-qubits))
         (dms (qvm::make-density-matrix-state num-qubits))
         (ps-basic-noise-qvm (make-instance 'basic-noise-qvm :number-of-qubits num-qubits
                                                             :state ps
                                                             :avg-gate-time 1))
         (dms-basic-noise-qvm (make-instance 'basic-noise-qvm :number-of-qubits num-qubits
                                                              :state dms
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

(deftest test-mixed-state-qvm-parametric-gate ()
  "Density qvm can apply parametric gates."
  (let* ((qvm (qvm::make-mixed-state-qvm 1)))
    (load-program qvm (with-output-to-quil
                        "DEFGATE G(%a):"
                        "    cos(%a), sin(%a)"
                        "    -sin(%a), cos(%a)"
                        "G(0.0) 0"))
    (run qvm)
    (is (double-float= 1 (realpart (aref (qvm::matrix-view (qvm::state qvm)) 0 0)) 1/10000))))

(deftest test-mixed-state-qvm-force-measurement-1q ()
  "Measurement on 1q density matrix qvm behaves as expected."
  (let ((qvm (qvm::make-mixed-state-qvm 1)))
    (qvm::load-program qvm (with-output-to-quil "H 0"))
    (run qvm)
    (qvm::force-measurement 1 0 (qvm::state qvm) 0.5)
    (is (double-float= 1 (density-matrix-trace qvm)))
    (is (double-float= 1 (realpart
                          (aref (qvm::matrix-view (qvm::state qvm)) 1 1))))))

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
  (assert (equal (array-dimensions (qvm::matrix-view (qvm::state qvm)))
                 (list (magicl:nrows mat) (magicl:ncols mat)))
          (mat)
          "Density matrix is of wrong size")
  (let ((density-matrix (qvm::matrix-view (qvm::state qvm))))
    (destructuring-bind (rows cols) (array-dimensions density-matrix)
      (dotimes (i rows qvm)
        (dotimes (j cols)
          (setf (aref density-matrix i j)
                (cflonum (magicl:tref mat i j))))))))

(defun make-1q-mixed-state-qvm (p)
  (check-type p (real 0 1))
  (let ((qvm (qvm::make-mixed-state-qvm 1))
        (mat (magicl:from-diag (list (- 1 p) p))))
    (load-density-from-matrix qvm mat)))

(defun make-2q-with-1q-mixed-state-qvm (p)
  (check-type p (real 0 1))
  (let* ((qvm (qvm::make-mixed-state-qvm 2))
         (q   (- 1 p))
         (mat1 (magicl:from-diag (list q p)))
         (mat2 (magicl:const 0.5 '(2 2) :type '(complex double-float)))
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
  (let ((p (quil:parse-quil "H 0; MEASURE 0"))
        (qvm (qvm::make-mixed-state-qvm 1)))
    (load-program qvm p)
    (run qvm)
    (let ((mat (qvm::matrix-view (qvm::state qvm))))
      (is (double-float= (realpart (aref mat 1 1)) 0.5))
      (is (double-float= (realpart (aref mat 0 0)) 0.5)))))
