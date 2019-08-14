;;;; linear-algebra-intrinsics.lisp
;;;;

;;;; Author: Cole Scott

(defpackage #:qvm-intrinsics
  (:use :cl :sb-ext :sb-c))

(in-package #:qvm-intrinsics)

(deftype d4 ()
  '(simd-pack-256 double-float))
(deftype cdf ()
  '(complex double-float))

(defknown (%2x2matrix-to-simd) (cdf cdf cdf cdf)
    (values d4 d4 d4 d4)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown (%matmul2-simd) (d4 d4 d4 d4 cdf cdf)
    (values cdf cdf)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(in-package #:sb-vm)

(define-vop (qvm-intrinsics::matmul2-simd)
  (:translate qvm-intrinsics::%matmul2-simd)
  (:policy :fast-safe)
  (:args (vyr :scs (double-avx2-reg))
	 (vyi :scs (double-avx2-reg))
	 (xzr :scs (double-avx2-reg))
	 (xzi :scs (double-avx2-reg))
	 (a :scs (complex-double-reg))
	 (b :scs (complex-double-reg)))
  (:arg-types simd-pack-256-double
	      simd-pack-256-double
	      simd-pack-256-double
	      simd-pack-256-double
	      complex-double-float
	      complex-double-float)
  (:results (a :scs (complex-double-reg))
	    (b :scs (complex-double-reg)))
  (:result-types complex-double-float complex-double-float)
  (:temporary (:sc double-avx2-reg) aa bb aa-swzld bb-swzld res)
  (:generator 4
		(inst vinsertf128 aa a a #xFF) ; Store a in aa twice [Ar Ai Ar Ai]
		(inst vinsertf128 bb b b #xFF) ; Store b in bb twice [Br Bi Br Bi]
		(inst vpermpd aa-swzld aa #4r2301) ; Store a in aa-swzld reversed twice [Ai Ar Ai Ar]
		(inst vpermpd bb-swzld bb #4r2301) ; Store b in bb-swzld reversed twice [Bi Br Bi Br]
		(inst vxorpd res res res)
		(inst vfmadd231pd res vyi aa-swzld)
		(inst vfmadd231pd res xzi bb-swzld)
		(inst vfmaddsub231pd res vyr aa)
		(inst vfmadd231pd res xzr bb)
		(inst vextractf128 a res #xFF)
		(inst vextractf128 b res #x00)))

(define-vop (qvm-intrinsics::2x2matrix-to-simd)
  (:translate qvm-intrinsics::%2x2matrix-to-simd)
  (:policy :fast-safe)
  (:args (v :scs (complex-double-reg))
	 (x :scs (complex-double-reg))
	 (y :scs (complex-double-reg))
	 (z :scs (complex-double-reg)))
  (:arg-types complex-double-float
	      complex-double-float
	      complex-double-float
	      complex-double-float)
  (:results (vyr :scs (double-avx2-reg))
	    (vyi :scs (double-avx2-reg))
	    (xzr :scs (double-avx2-reg))
	    (xzi :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double
		 simd-pack-256-double
		 simd-pack-256-double
		 simd-pack-256-double)
  (:temporary (:sc double-avx2-reg) vy xz) ; NOTE: Could probably be optimized
  (:generator 4
	      (inst vinsertf128 vy y v #xFF) ; vy = [Vr Vi Yr Yi]
	      (inst vinsertf128 xz z x #xFF) ; xz = [Xr Xi Zr Zi]
	      (inst vpermpd vyr vy #4r2200)  ; vyr = [Vr Vr Yr Yr]
	      (inst vpermpd vyi vy #4r3311)  ; vyi = [Vi Vi Yi Yi]
	      (inst vpermpd xzr xz #4r2200)  ; xzr = [Xr Xr Zr Zr]
	      (inst vpermpd xzi xz #4r3311)  ; xzr = [Xi Xi Zi Zi]
	      ))

(in-package :qvm-intrinsics)
(declaim (optimize speed))

(declaim (inline matmul2-simd))
(defun matmul2-simd (a b c d e f) (%matmul2-simd a b c d e f))
(declaim (notinline matmul2-simd))

(declaim (inline 2x2matrix-to-simd))
(defun 2x2matrix-to-simd (x y z w) (%2x2matrix-to-simd x y z w))
(declaim (notinline 2x2matrix-to-simd))

(defun matmul2-vector-simd (mat vec)
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
		 (setf (aref vec (1+ i)) q)))))
    )
  nil)

;;;

(declaim (inline matmul2*))
(defun matmul2* (v x y z a b)
  (declare (type (complex double-float) v x y z a b)
           (values (values (complex double-float) (complex double-float))))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (values (+ (* a v)
	     (* b x))
	  (+ (* a y)
	     (* b z)))
  )
(declaim (notinline matmul2*))

(defun matmul2-vector (mat vec)
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
	       (setf (aref vec (1+ i)) q))))
    nil))

(defun matmul2-bench (n)
  (sb-ext:gc :full t)
  (let ((vec (make-array n :element-type '(complex double-float)))
	(mat (make-array '(2 2) :element-type '(complex double-float))))
    (dotimes (i n)
      (setf (aref vec i) (complex (random 1d0) (random 1d0))))
    (dotimes (i 2)
      (dotimes (j 2)
	(setf (aref mat i j) (complex (random 1d0) (random 1d0)))))
    (time (loop :repeat 4 :do (matmul2-vector mat vec)))
    (time (loop :repeat 4 :do (matmul2-vector-simd mat vec)))
    nil))

(defun test-matmul2-simd (v x y z a b)
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
