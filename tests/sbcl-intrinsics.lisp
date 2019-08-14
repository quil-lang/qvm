;;;; tests/sbcl-intrinsics.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:qvm-tests)

(deftest test-simple-matrix-mult ()
  "Test basic matrix multiplication using AVX2."
  (let* ((mat (make-array '(2 2) :initial-contents '((#C(1d0 2d0) #C(3d0 4d0)) (#C(5d0 6d0) #C(7d0 8d0)))))     
         (a #C(1d0 2d0))
         (b #C(3d0 4d0))
         (expected-p #C(-10d0 28d0))
         (expected-q #C(-18d0 68d0))
         (v (aref mat 0 0))
         (x (aref mat 0 1))
         (y (aref mat 1 0))
         (z (aref mat 1 1)))
    (multiple-value-bind (vyr vyi xzr xzi)
        (qvm-intrinsics::2x2matrix-to-simd v x y z)
      (multiple-value-bind (p q)
          (qvm-intrinsics::matmul2-simd vyr vyi xzr xzi a b)
        (is (cflonum= p expected-p))
        (is (cflonum= q expected-q))))))

(deftest test-matmul2-vector-simd ()
  "Test that matmul2-vector-simd multiplies vectors correctly"
  (let* ((mat (make-array '(2 2) :element-type '(complex double-float) :initial-contents '((#C(1d0 2d0) #C(3d0 4d0)) (#C(5d0 6d0) #C(7d0 8d0)))))     
         (vec (make-array '(2) :element-type '(complex double-float) :initial-contents '(#C(1d0 2d0) #C(3d0 4d0))))
         (res (make-array '(2) :element-type '(complex double-float) :initial-contents '(#C(-10d0 28d0) #C(-18d0 68d0)))))
    (qvm-intrinsics::matmul2-vector-simd mat vec)
    (is (every #'cflonum= vec res))))
