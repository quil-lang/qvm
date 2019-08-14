;;;; sbcl-linear-algebra-intrinsics.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:qvm-intrinsics)
(declaim (optimize speed))

(declaim (inline matmul2-simd))
(defun matmul2-simd (vyr vyi xzr xzi a b)
  "Multiply the matrix [[v x] [y z]] by vector [a b] using vyr, vyi, xzr, xzi from the output of 2x2matrix-to-simd and return the complex double-floats p and q.

Uses AVX2 features."
  (%matmul2-simd vyr vyi xzr xzi a b))
(declaim (notinline matmul2-simd))

(declaim (inline 2x2matrix-to-simd))
(defun 2x2matrix-to-simd (x y z w)
  "Convert a 2x2 matrix of complex numbers into real/complex part combined registers. Takes in (v, x, y, z) and returns (vyr, vyi, xzr, xzi) for use in matmul2-simd.

Uses AVX2 features."
  (%2x2matrix-to-simd x y z w))
(declaim (notinline 2x2matrix-to-simd))

;;; Test Functions & Examples

(defun matmul2-vector-simd (mat vec)
  "Multiply a 2x2 matrix by a vector containing adjacent pairs of complex numbers, storing the result back into the vector.

Uses AVX2 features."
  (declare (type (simple-array (complex double-float) (2 2)) mat)
           (type (simple-array (complex double-float) (*)) vec)
           (optimize (speed 3) (safety 0))
           (inline matmul2-simd)
           (inline 2x2matrix-to-simd))
  (let* ((v (aref mat 0 0))
         (x (aref mat 0 1))
         (y (aref mat 1 0))
         (z (aref mat 1 1)))
    (multiple-value-bind (vyr vyi xzr xzi)
        (2x2matrix-to-simd v x y z)
      (loop :for i :below (length vec) :by 2
            :do (let* ((a (aref vec i))
                       (b (aref vec (1+ i))))
                  (multiple-value-bind (p q)
                      (matmul2-simd vyr vyi xzr xzi a b)
                    (setf (aref vec i) p)
                    (setf (aref vec (1+ i)) q)
                    nil))))))

(defun matmul2-bench (n)
  "Compare time taken to do the same operation without AVX2, then with AVX2 and print output to stdout."
  (sb-ext:gc :full t)
  (let ((vec (make-array n :element-type '(complex double-float)))
        (mat (make-array '(2 2) :element-type '(complex double-float))))
    (dotimes (i n)
      (setf (aref vec i) (complex (random 1d0) (random 1d0))))
    (dotimes (i 2)
      (dotimes (j 2)
        (setf (aref mat i j) (complex (random 1d0) (random 1d0)))))
    (format t "Without AVX2:~%")
    (time (loop :repeat 4 :do (matmul2-vector mat vec)))
    (format t "With AVX2:~%")
    (time (loop :repeat 4 :do (matmul2-vector-simd mat vec)))
    nil))

(defun test-matmul2-simd (v x y z a b)
  "Multiply the matrix [[v x] [y z]] by vector [a b] using AVX2."
  (declare (inline matmul2-simd)
           (optimize (speed 1)))
  (check-type v (complex double-float))
  (check-type y (complex double-float))
  (check-type x (complex double-float))
  (check-type z (complex double-float))
  (check-type a (complex double-float))
  (check-type b (complex double-float))
  (multiple-value-bind (vyr vyi xzr xzi)
      (2x2matrix-to-simd v x y z)
    (multiple-value-bind (p q)
        (matmul2-simd vyr vyi xzr xzi a b)
      (list p q))))

;;; Non-avx2 functions for benchmarking and testing

(declaim (inline matmul2*))
(defun matmul2* (v x y z a b)
  "Sample matmul2 function. Used for benchmarking AVX2 versions."
  (declare (type (complex double-float) v x y z a b)
           (values (values (complex double-float) (complex double-float))))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (values (+ (* a v)
             (* b x))
          (+ (* a y)
             (* b z))))
(declaim (notinline matmul2*))

(defun matmul2-vector (mat vec)
  "Multiply a 2x2 matrix by a vector containing adjacent pairs of complex numbers, storing the result back into the vector. Used for benchmarking AVX2 version."
  (declare (type (simple-array (complex double-float) (2 2)) mat)
           (type (simple-array (complex double-float) (*)) vec)
           (inline matmul2*)
           (optimize (speed 3) (safety 0)))
  (let* ((v (aref mat 0 0))
         (x (aref mat 0 1))
         (y (aref mat 1 0))
         (z (aref mat 1 1)))
    (loop :for i :below (length vec) :by 2
          :do (let* ((a (aref vec i))
                     (b (aref vec (1+ i))))
                (multiple-value-bind (p q)
                    (matmul2* v x y z a b)
                  (setf (aref vec i) p)
                  (setf (aref vec (1+ i)) q)
                  nil)))))
