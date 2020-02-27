;;;; density-qvm-tests.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:qvm-tests)

(defun density-matrix-trace (qvm)
  "Compute the trace of the density matrix associated with density qvm."
  (let ((sum (flonum 0))
        (density-matrix (qvm::matrix-view (qvm::state qvm))))
    ;; This is a sum along the diagonal of the density-matrix
    (dotimes (i (expt 2 (qvm::number-of-qubits qvm)) sum)
      (incf sum (realpart (aref density-matrix i i))))))


(defun density-matrix-purity (qvm)
  "Compute the purity aka tr(ρ^2)."
  (let* ((n (expt 2 (number-of-qubits qvm)))
         (density-mat (magicl:from-array (qvm::amplitudes qvm)
                                        (list n n)
                                        :type '(complex double-float)))
         (squared     (magicl:@ density-mat density-mat)))
    (realpart (magicl:trace squared))))

(deftest test-density-qvm-parametric-gate ()
  "Density qvm can apply parametric gates."
    (let* ((qvm (qvm::make-density-qvm 1)))
      (load-program qvm (with-output-to-quil
                          "DEFGATE G(%a):"
                          "    cos(%a), sin(%a)"
                          "    -sin(%a), cos(%a)"
                          "G(0.0) 0"))
      (run qvm)
      (is (double-float= 1 (realpart (aref (qvm::matrix-view (qvm::state qvm)) 0 0)) 1/10000))))

(deftest test-density-qvm-force-measurement-1q ()
  "Measurement on 1q density matrix qvm behaves as expected."
  (let ((qvm (qvm::make-density-qvm 1)))
    (qvm::load-program qvm (with-output-to-quil "H 0"))
    (run qvm)
    (qvm::force-measurement 1 0 (qvm::state qvm) 0.5)
    (is (double-float= 1 (density-matrix-trace qvm)))
    (is (double-float= 1 (realpart
                          (aref (qvm::matrix-view (qvm::state qvm)) 1 1))))))

(deftest test-density-qvm-force-measurement-4q ()
  "Measurement on 4q density matrix qvm is trace preserving."
  (let ((qvm (qvm::make-density-qvm 4))
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

(defun make-1q-mixture (p)
  (check-type p (real 0 1))
  (let ((qvm (qvm::make-density-qvm 1))
        (mat (magicl:from-diag (list (- 1 p) p))))
    (load-density-from-matrix qvm mat)))

(defun make-2q-with-1q-mixed (p)
  (check-type p (real 0 1))
  (let* ((qvm (qvm::make-density-qvm 2))
         (q   (- 1 p))
         (mat1 (magicl:from-diag (list q p)))
         (mat2 (magicl:const 0.5 '(2 2) :type '(complex double-float)))
         (mat (magicl:kron mat1 mat2)))
    (load-density-from-matrix qvm mat)))


(deftest test-density-qvm-1q-purity ()
  "Tests that unitary evolution preserves purity."
  (dolist (p '(0.1 0.4 0.5 0.6 0.9))
    (let ((expected-purity (+ (expt (- 1 p) 2) (expt p 2)))
          (qvm (make-1q-mixture p)))
      (is (double-float= (density-matrix-purity qvm) expected-purity))
      (load-program qvm (with-output-to-quil "H 0"))
      (run qvm)
      (is (double-float= (density-matrix-purity qvm) expected-purity)))))

(deftest test-density-qvm-2q-purity ()
  "Tests that unitary evolution preserves purity."
  (dolist (p '(0.1 0.4 0.5 0.6 0.9))
    (let ((expected-purity (+ (expt (- 1 p) 2) (expt p 2)))
          (qvm (make-2q-with-1q-mixed p)))
      (is (double-float= (density-matrix-purity qvm) expected-purity))
      (load-program qvm (with-output-to-quil "CNOT 0 1"))
      (run qvm)
      (is (double-float= (density-matrix-purity qvm) expected-purity)))))

(deftest test-density-qvm-noisy-readout ()
  "Test that the noisy readout behaves as expected."
  (test-noisy-readout-2q-qvm (make-density-qvm 2 :classical-memory-subsystem nil)))

;;; Shamelessly stolen from noisy-qvm-tests.lisp
;;; This shouldn't work anymore because I decoupled readout povms from density-qvm AND there isn't a measure-all defined for density-qvm. I commented out the end test
(deftest test-density-qvm-noisy-measure-all ()
  "Test that MEASURE-ALL works correctly for noisy readout"
  (let ((p (with-output-to-quil
             (write-line "X 0")
             (write-line "X 1")))
        (tries 500)
        (results-desired '((1 1)
                           (1 0)))
        (qvm (make-density-qvm 2 :classical-memory-subsystem nil)))
    (qvm:load-program qvm p :supersede-memory-subsystem t)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let* ((qvm-final (progn
                                  (qvm::reset-quantum-state qvm)
                                  (qvm::set-readout-povm qvm 1 '(0.8d0 0.1d0
                                                                 0.2d0 0.9d0))
                                  (qvm:run qvm))))
                (multiple-value-bind (qvm-final measured-bits)
                    (measure-all qvm-final)
                  (declare (ignore qvm-final))
                  (setf results-desired
                        (remove measured-bits results-desired :test #'equalp)))))
    (is (plusp tries))))

(deftest test-density-qvm-1q-measure-discard ()
  "Test that measure discard behaves as expected."
  ;; We prepare the pure state (1/sqrt(2))(|0> - |1>). If we were to
  ;; measure and record the outcome, we would know with certainty
  ;; which of |0> or |1> the state had collapsed into. If we discard
  ;; that outcome (in some sense) then we should find ourselves in the
  ;; mixed state |ψ> = 1/2 (|0><0| + |1><1|)
  (let ((p (quil:parse-quil "H 0
MEASURE 0"))
        (qvm (make-density-qvm 1)))
    (load-program qvm p)
    (run qvm)
    (let ((mat (qvm::matrix-view (qvm::state qvm))))
      (is (double-float= (realpart (aref mat 1 1)) 0.5))
      (is (double-float= (realpart (aref mat 0 0)) 0.5)))))

(deftest test-density-qvm-noisy-x-gate ()
  "Test a noisy X gate with bit flip probability px = 1.0 on the DENSITY-QVM."
  (let ((p (with-output-to-quil
             "DECLARE ro BIT"
             "X 0"
             "MEASURE 0 ro"))
        (qvm (make-instance 'qvm:density-qvm :classical-memory-subsystem nil
                                             :number-of-qubits 2)))
    (set-noisy-gate qvm "X" '(0) (qvm::make-pauli-perturbed-1q-gate "X" 1.0 0.0 0.0))
    (qvm:load-program qvm p :supersede-memory-subsystem t)
    (run qvm)
    (is (= 0 (memory-ref qvm "ro" 0)))))
