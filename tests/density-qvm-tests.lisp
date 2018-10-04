;;;; density-qvm-tests.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:qvm-tests)

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

(deftest test-density-qvm-parametric-gate ()
  "Density qvm can apply parametric gates."
    (let* ((qvm (qvm::make-density-qvm 1)))
      (load-program qvm (with-output-to-quil
                          "DEFGATE G(%a):"
                          "    cos(%a), sin(%a)"
                          "    -sin(%a), cos(%a)"
                          "G(0.0) 0"))
      (run qvm)
      (is (double-float= 1 (realpart (aref (qvm::density-matrix-view qvm) 0 0)) 1/10000))))

(deftest test-density-qvm-force-measurement-1q ()
  "Measurement on 1q density matrix qvm behaves as expected."
  (let ((qvm (qvm::make-density-qvm 1)))
    (qvm::load-program qvm (with-output-to-quil "H 0"))
    (run qvm)
    (qvm::density-qvm-force-measurement 1 0 qvm 0.5)
    (is (double-float= 1 (density-matrix-trace qvm)))
    (is (double-float= 1 (realpart
                          (aref (qvm::density-matrix-view qvm) 1 1))))))

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
    (let ((p (qvm::density-qvm-qubit-probability qvm 3)))
      (qvm::density-qvm-force-measurement 1 3 qvm p)
      (is (qvm-tests::double-float= 1 (density-matrix-trace qvm))))))

(deftest test-density-qvm-1q-with-kraus ()
  "Simple 1q gate noise works as expected."
  (let ((qvm (qvm::make-density-qvm 1)))
    ;; an equal mix of I and X
    (set-noisy-gate qvm "I" '(0) (qvm::make-pauli-perturbed-1q-gate "I" 0.5 0.0 0.0))
    (load-program qvm (with-output-to-quil "I 0"))
    (run qvm)

    (let ((expected-density #(0.5 0.0
                              0.0 0.5)))
      (is (every (lambda (a b)
                 (and (double-float= (realpart a) (realpart b) 1/10000)
                      (double-float= (imagpart a) (imagpart b) 1/10000)))
               (qvm::amplitudes qvm)
               expected-density)))))

(deftest test-density-qvm-2q-with-kraus ()
  "Simple gate noise on 2q density matrix works as expected."
  (let ((qvm-classic (qvm::make-density-qvm 2))
        (qvm-kraus (qvm::make-density-qvm 2)))
    (load-program qvm-classic (with-output-to-quil "X 1"))
    (run qvm-classic)

    ;; set up a noisy version of I 1 that should be equivalent to X 1
    (set-noisy-gate qvm-kraus "I" '(1) (qvm::make-pauli-perturbed-1q-gate "I" 1.0 0.0 0.0))
    (load-program qvm-kraus (with-output-to-quil "I 1"))
    (run qvm-kraus)

    (is (every (lambda (a b)
                 (and (double-float= (realpart a) (realpart b) 1/10000)
                      (double-float= (imagpart a) (imagpart b) 1/10000)))
               (qvm::amplitudes qvm-classic)
               (qvm::amplitudes qvm-kraus)))))


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

(defun make-1q-mixture (p)
  (check-type p (real 0 1))
  (let ((qvm (qvm::make-density-qvm 1))
        (mat (magicl:diag 2 2
                          (list (- 1 p) p))))
    (load-density-from-matrix qvm mat)))

(defun make-2q-with-1q-mixed (p)
  (check-type p (real 0 1))
  (let* ((qvm (qvm::make-density-qvm 2))
         (q   (- 1 p))
         (mat1 (magicl:diag 2 2 (list q p)))
         (mat2 (magicl:make-complex-matrix 2 2 '(0.5 0.5 0.5 0.5)))
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
