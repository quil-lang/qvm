;;;; sbcl-linear-algebra-intrinsics.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:qvm-intrinsics)
(declaim (optimize speed))

(declaim (inline 2x2matrix-to-simd))
(defun 2x2matrix-to-simd (x y z w)
  "Convert a 2x2 matrix of complex numbers into real/complex part combined registers. Takes in (v, x, y, z) and returns (vyr, vyi, xzr, xzi) for use in matmul2-simd.

Uses AVX2 features."
  (%2x2matrix-to-simd x y z w))
(declaim (notinline 2x2matrix-to-simd))

(declaim (inline 2x4matrix-to-simd))
(defun 2x4matrix-to-simd (m00 m01 m02 m03 m10 m11 m12 m13)
  "Convert a 2x2 matrix of complex numbers into real/complex part combined registers. Takes in (v, x, y, z) and returns (vyr, vyi, xzr, xzi) for use in matmul2-simd.

Uses AVX2 features."
  (%2x4matrix-to-simd m00 m01 m02 m03 m10 m11 m12 m13))
(declaim (notinline 2x4matrix-to-simd))

(declaim (inline matmul2-simd))
(defun matmul2-simd (vyr vyi xzr xzi a b)
  "Multiply the matrix [[v x] [y z]] by vector [a b] using vyr, vyi, xzr, xzi from the output of 2x2matrix-to-simd and return the complex double-floats p and q.

Uses AVX2 features."
  (%matmul2-simd vyr vyi xzr xzi a b))
(declaim (notinline matmul2-simd))

(declaim (inline matmul2-simd-real))
(defun matmul2-simd-real (vyr xzr a b)
  "Multiply the real-only matrix [[v x] [y z]] by vector [a b] using vyr, vyi, xzr, xzi from the output of 2x2matrix-to-simd and return the complex double-floats p and q.

Uses AVX2 features."
  (%matmul2-simd-real vyr xzr a b))
(declaim (notinline matmul2-simd-real))

(declaim (inline matmul4-simd-half))
(defun matmul4-simd-half (m0r m1r m2r m3r m0i m1i m2i m3i a0 a1 a2 a3)
  "4x4 half matrix mult

Uses AVX2 features."
  (%matmul4-simd-half m0r m1r m2r m3r m0i m1i m2i m3i a0 a1 a2 a3))
(declaim (notinline matmul4-simd-half))

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

(defun matmul4-vector-simd (mat vec)
  "Multiply a 4x4 matrix by a vector containing adjacent quads of complex numbers, storing the result back into the vector.

Uses AVX2 features."
  (declare (type (simple-array (complex double-float) (4 4)) mat)
           (type (simple-array (complex double-float) (*)) vec)
           (optimize (speed 3) (safety 0) (debug 0) (space 3) (compilation-speed 0))
           (inline matmul4-simd-half)
           (inline 2x4matrix-to-simd)
           (dynamic-extent mat))
  (let ((mat (sb-ext:array-storage-vector mat))
        (a (make-array 4 :element-type '(complex double-float))))
    (declare (type (simple-array (complex double-float) (4)) a)
             (type (simple-array (complex double-float) (16)))
             (dynamic-extent a))
    (symbol-macrolet ((a0 (aref a 0))
                      (a1 (aref a 1))
                      (a2 (aref a 2))
                      (a3 (aref a 3)))
      (loop :for i :below (length vec) :by 4 :do
           (setf a0 (aref vec (+ i 0))
                 a1 (aref vec (+ i 1))
                 a2 (aref vec (+ i 2))
                 a3 (aref vec (+ i 3)))
           (let ((m00 (aref mat 0))
                 (m01 (aref mat 1))
                 (m02 (aref mat 2))
                 (m03 (aref mat 3))
                 (m10 (aref mat 4))
                 (m11 (aref mat 5))
                 (m12 (aref mat 6))
                 (m13 (aref mat 7)))
             (multiple-value-bind (m0r m0i m1r m1i m2r m2i m3r m3i)
                 (2x4matrix-to-simd m00 m01 m02 m03 m10 m11 m12 m13)
               (multiple-value-bind (r0 r1)
                   (matmul4-simd-half m0r m0i m1r m1i m2r m2i m3r m3i a0 a1 a2 a3)
                 (setf (aref vec (+ i 0)) r0
                       (aref vec (+ i 1)) r1))))
           (let ((m20 (aref mat 8))
                 (m21 (aref mat 9))
                 (m22 (aref mat 10))
                 (m23 (aref mat 11))
                 (m30 (aref mat 12))
                 (m31 (aref mat 13))
                 (m32 (aref mat 14))
                 (m33 (aref mat 15)))
             (multiple-value-bind (m4r m4i m5r m5i m6r m6i m7r m7i)
                 (2x4matrix-to-simd m20 m21 m22 m23 m30 m31 m32 m33)
               (multiple-value-bind (r2 r3)
                   (matmul4-simd-half m4r m4i m5r m5i m6r m6i m7r m7i a0 a1 a2 a3)
                 (setf (aref vec (+ i 2)) r2
                       (aref vec (+ i 3)) r3)))))))
  vec)

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

(defun matmul4-bench (n)
  "Compare time taken to do the same operation without AVX2, then with AVX2 and print output to stdout."
  (sb-ext:gc :full t)
  (let ((vec (make-array n :element-type '(complex double-float)))
        (mat (make-array '(4 4) :element-type '(complex double-float))))
    (dotimes (i n)
      (setf (aref vec i) (complex (random 1d0) (random 1d0))))
    (dotimes (i 4)
      (dotimes (j 4)
        (setf (aref mat i j) (complex (random 1d0) (random 1d0)))))
    ;; (format t "Without AVX2:~%")
    ;; (time (loop :repeat 4 :do (matmul4-vector mat vec)))
    (format t "With AVX2:~%")
    (time (loop :repeat 4 :do (matmul4-vector-simd mat vec)))
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
