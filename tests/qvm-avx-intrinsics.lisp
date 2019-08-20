;;;; tests/sbcl-intrinsics.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:qvm-tests)

(deftest test-avx2-simple-matrix-mult ()
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

(deftest test-avx2-matmul2-vector-simd ()
  "Test that matmul2-vector-simd multiplies vectors correctly"
  (let ((mat (make-array '(2 2) :element-type '(complex double-float) :initial-contents '((#C(1d0 2d0) #C(3d0 4d0)) (#C(5d0 6d0) #C(7d0 8d0)))))     
         (vec (make-array '(2) :element-type '(complex double-float) :initial-contents '(#C(1d0 2d0) #C(3d0 4d0))))
         (res (make-array '(2) :element-type '(complex double-float) :initial-contents '(#C(-10d0 28d0) #C(-18d0 68d0)))))
    (qvm-intrinsics::matmul2-vector-simd mat vec)
    (is (every #'cflonum= vec res))))

(deftest test-avx2-matmul2-random ()
  "Test avx version of matmul4 by comparing to normal one 100 times"
  (let ((mat (make-array '(2 2) :element-type '(complex double-float)))
        (vec1 (make-array '(2) :element-type '(complex double-float)))
        (vec2 (make-array '(2) :element-type '(complex double-float))))
    (loop :repeat 100 :do
      (dotimes (i 2)
        (let ((num (complex (random 1d0) (random 1d0))))
          (setf (aref vec1 i) num
                (aref vec2 i) num)))
      (dotimes (i 2)
        (dotimes (j 2)
          (setf (aref mat i j) (complex (random 1d0) (random 1d0)))))
      (qvm::matmul2 mat vec1 vec2)
      (qvm-intrinsics::matmul2-vector-simd mat vec1)
      (is (every #'cflonum= vec1 vec2))))
  nil)

(deftest test-avx2-matmul4-random ()
  "Test avx version of matmul4 by comparing to normal one 100 times"
  (let ((mat (make-array '(4 4) :element-type '(complex double-float)))
        (vec1 (make-array '(4) :element-type '(complex double-float)))
        (vec2 (make-array '(4) :element-type '(complex double-float))))
    (loop :repeat 100 :do
      (dotimes (i 4)
        (let ((num (complex (random 1d0) (random 1d0))))
          (setf (aref vec1 i) num
                (aref vec2 i) num)))
      (dotimes (i 4)
        (dotimes (j 4)
          (setf (aref mat i j) (complex (random 1d0) (random 1d0)))))
      (qvm::matmul4 mat vec1 vec2)
      (qvm-intrinsics::matmul4-vector-simd mat vec1)
      (is (every #'cflonum= vec1 vec2))))
  nil)
