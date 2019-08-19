;;;; sbcl-vops.lisp
;;;;
;;;; Author: Cole Scott
;;;;         
;;;; Collaborators: Jussi Kukkonen
;;;;                Robert Smith

(defpackage #:qvm-intrinsics
  (:use #:cl #:sb-ext #:sb-c))

(in-package #:qvm-intrinsics)

(deftype d4 ()
  '(simd-pack-256 double-float))
(deftype cdf ()
  '(complex double-float))

;;; Function stub definitions
;;; This tells the compiler about the existance and properties of the VOPs as functions

(defknown %prefetch ((member :nta :t0 :t1 :t2)
                     sb-sys:system-area-pointer
                     sb-vm:signed-word
                     (member 2 4 8 16)
                     fixnum)
    (values &optional)
    (any always-translatable)
  :overwrite-fndb-silently t)

(defknown (%2x2matrix-to-simd) (cdf cdf cdf cdf)
    (values d4 d4 d4 d4)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown (%2x4matrix-to-simd) (cdf cdf cdf cdf cdf cdf cdf cdf)
    (values d4 d4 d4 d4 d4 d4 d4 d4)
    (flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown (%matmul2-simd) (d4 d4 d4 d4 cdf cdf)
    (values cdf cdf)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown (%matmul2-simd-real) (d4 d4 cdf cdf)
    (values cdf cdf)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown (%matmul4-simd-half) (d4 d4 d4 d4 d4 d4 d4 d4 cdf cdf cdf cdf)
    (values cdf cdf)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

;;; VOP definitions

(in-package #:sb-vm)

(define-vop (qvm-intrinsics::%prefetch)
  (:translate qvm-intrinsics::%prefetch)
  (:policy :fast-safe)
  (:args (base :scs (sap-reg))
         (index :scs (any-reg)))
  (:arg-types (:constant (member :nta :t0 :t1 :t2))
              system-area-pointer
              (:constant signed-word)
              (:constant (member . #.(loop :for i :below 4
                                           :collect (ash 1 (+ i n-fixnum-tag-bits)))))
              fixnum)
  (:results)
  (:info type disp stride)
  (:generator 1
              (inst prefetch type
                    (make-ea :byte :base base
                             :index index
                             :scale (ash stride
                                         (- n-fixnum-tag-bits))
                             :disp disp))))

(define-vop (qvm-intrinsics::2x2matrix-to-simd)
  (:translate qvm-intrinsics::%2x2matrix-to-simd)
  (:policy :fast-safe)
  (:args (m00 :scs (complex-double-reg))
         (m01 :scs (complex-double-reg))
         (m10 :scs (complex-double-reg))
         (m11 :scs (complex-double-reg)))
  (:arg-types complex-double-float
              complex-double-float
              complex-double-float
              complex-double-float)
  (:results (m0r :scs (double-avx2-reg))
            (m0i :scs (double-avx2-reg))
            (m1r :scs (double-avx2-reg))
            (m1i :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double
                 simd-pack-256-double
                 simd-pack-256-double
                 simd-pack-256-double)
  (:temporary (:sc double-avx2-reg) m0 m1)
  (:generator 4
              (inst vinsertf128 m0 m10 m00 #xFF)
              (inst vinsertf128 m1 m11 m01 #xFF)
              (inst vpermpd m0r m0 #4r2200)
              (inst vpermpd m0i m0 #4r3311)
              (inst vpermpd m1r m1 #4r2200)
              (inst vpermpd m1i m1 #4r3311)))

(define-vop (qvm-intrinsics::2x4matrix-to-simd)
  (:translate qvm-intrinsics::%2x4matrix-to-simd)
  (:policy :fast-safe)
  (:args (m00 :scs (complex-double-reg))
         (m01 :scs (complex-double-reg))
         (m02 :scs (complex-double-reg))
         (m03 :scs (complex-double-reg))
         (m10 :scs (complex-double-reg))
         (m11 :scs (complex-double-reg))
         (m12 :scs (complex-double-reg))
         (m13 :scs (complex-double-reg)))
  (:arg-types complex-double-float
              complex-double-float
              complex-double-float
              complex-double-float
              complex-double-float
              complex-double-float
              complex-double-float
              complex-double-float)
  (:results (m0r :scs (double-avx2-reg))
            (m0i :scs (double-avx2-reg))
            (m1r :scs (double-avx2-reg))
            (m1i :scs (double-avx2-reg))
            (m2r :scs (double-avx2-reg))
            (m2i :scs (double-avx2-reg))
            (m3r :scs (double-avx2-reg))
            (m3i :scs (double-avx2-reg)))
  (:result-types simd-pack-256-double
                 simd-pack-256-double
                 simd-pack-256-double
                 simd-pack-256-double
                 simd-pack-256-double
                 simd-pack-256-double
                 simd-pack-256-double
                 simd-pack-256-double)
  (:temporary (:sc double-avx2-reg) m0 m1 m2 m3)
  (:generator 4
              (inst vinsertf128 m0 m10 m00 #xFF)
              (inst vinsertf128 m1 m11 m01 #xFF)
              (inst vinsertf128 m2 m12 m02 #xFF)
              (inst vinsertf128 m3 m13 m03 #xFF)
              (inst vpermpd m0r m0 #4r2200)
              (inst vpermpd m0i m0 #4r3311)
              (inst vpermpd m1r m1 #4r2200)
              (inst vpermpd m1i m1 #4r3311)
              (inst vpermpd m2r m2 #4r2200)
              (inst vpermpd m2i m2 #4r3311)
              (inst vpermpd m3r m3 #4r2200)
              (inst vpermpd m3i m3 #4r3311)))

(defun qvm-intrinsics::repeat-complex-registers (&rest args)
  "Store a XMM register to upper and lower half of YMM register"
  (loop :for arg :in args
     :do (let ((dest (first arg))
               (src (second arg)))
           (inst vinsertf128 dest src src #xFF))))

(defun qvm-intrinsics::swizzle-complex-registers (&rest args)
  "Store a XMM register to upper and lower half of YMM register"
  (loop :for arg :in args
     :do (let ((dest (first arg))
               (src (second arg)))
           (inst vpermpd dest src #4r0101))))

(define-vop (qvm-intrinsics::matmul2-simd)
  (:translate qvm-intrinsics::%matmul2-simd)
  (:policy :fast-safe)
  (:args (vyr :scs (double-avx2-reg))
         (vyi :scs (double-avx2-reg))
         (xzr :scs (double-avx2-reg))
         (xzi :scs (double-avx2-reg))
         (a :scs (complex-double-reg) :target p)
         (b :scs (complex-double-reg) :target q))
  (:arg-types simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              complex-double-float
              complex-double-float)
  (:results (p :scs (complex-double-reg))
            (q :scs (complex-double-reg)))
  (:result-types complex-double-float complex-double-float)
  (:temporary (:sc double-avx2-reg) aa bb acc)
  (:generator 4
              (let ((aa-swzld aa)
                    (bb-swzld bb)) ; Save a register by using the first used temp to also store accumulator
                (qvm-intrinsics::swizzle-complex-registers (list aa-swzld a) (list bb-swzld b))
                (inst vmulpd acc vyi aa-swzld)      ; Multiply complex parts of a and store in acc
                (inst vfmadd231pd acc xzi bb-swzld) ; Multiply complex parts of b and add to acc
                (qvm-intrinsics::repeat-complex-registers (list aa a) (list bb b))
                (inst vfmaddsub231pd acc vyr aa)    ; Multiply real parts of a and add to acc, negating complex parts
                (inst vfmadd231pd acc xzr bb)       ; Multiply real parts of b and add to acc
                (inst vextractf128 p acc #xFF)      ; Copy the upper 2 doubles from acc to p
                (inst vextractf128 q acc #x00))))   ; Copy the lower 2 doubles from acc to q

(define-vop (qvm-intrinsics::matmul2-simd-real)
  (:translate qvm-intrinsics::%matmul2-simd-real)
  (:policy :fast-safe)
  (:args (vyr :scs (double-avx2-reg))
         (xzr :scs (double-avx2-reg))
         (a :scs (complex-double-reg) :target p)
         (b :scs (complex-double-reg) :target q))
  (:arg-types simd-pack-256-double
              simd-pack-256-double
              complex-double-float
              complex-double-float)
  (:results (p :scs (complex-double-reg))
            (q :scs (complex-double-reg)))
  (:result-types complex-double-float complex-double-float)
  (:temporary (:sc double-avx2-reg) aa bb)
  (:generator 4
              (let ((acc aa)) ; Save a register by using the first used temp to also store accumulator
                (qvm-intrinsics::repeat-complex-registers (list aa a) (list bb b))
                (inst vmulpd acc vyr aa)          ; Multiply real parts of a and store in acc
                (inst vfmadd231pd acc xzr bb)     ; Multiply real parts of b and add to acc
                (inst vextractf128 p acc #xFF)    ; Copy the upper 2 doubles from acc to p
                (inst vextractf128 q acc #x00)))) ; Copy the lower 2 doubles from acc to q

(define-vop (qvm-intrinsics::matmul4-simd-half)
  (:translate qvm-intrinsics::%matmul4-simd-half)
  (:policy :fast-safe)
  (:args (m0r :scs (double-avx2-reg))
         (m0i :scs (double-avx2-reg))
         (m1r :scs (double-avx2-reg))
         (m1i :scs (double-avx2-reg))
         (m2r :scs (double-avx2-reg))
         (m2i :scs (double-avx2-reg))
         (m3r :scs (double-avx2-reg))
         (m3i :scs (double-avx2-reg))
         (a0 :scs (complex-double-reg) :target p)
         (a1 :scs (complex-double-reg) :target q)
         (a2 :scs (complex-double-reg))
         (a3 :scs (complex-double-reg)))
  (:arg-types simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              complex-double-float
              complex-double-float
              complex-double-float
              complex-double-float)
  (:results (p :scs (complex-double-reg))
            (q :scs (complex-double-reg)))
  (:result-types complex-double-float complex-double-float)
  (:temporary (:sc double-avx2-reg) aa0 aa1 acc)
  (:generator 4
              (let* ((aa2 aa0)          ; Save registers by reusing same old ones
                     (aa3 aa1)
                     (aa0-swzld aa0)
                     (aa1-swzld aa1)
                     (aa2-swzld aa2)
                     (aa3-swzld aa3))
                (qvm-intrinsics::swizzle-complex-registers (list aa0-swzld a0) (list aa1-swzld a1))
                (inst vmulpd acc m0i aa0-swzld)
                (inst vfmadd231pd acc m1i aa1-swzld)
                (qvm-intrinsics::swizzle-complex-registers (list aa2-swzld a2) (list aa3-swzld a3))
                (inst vfmadd231pd acc m2i aa2-swzld)
                (inst vfmadd231pd acc m3i aa3-swzld)
                (qvm-intrinsics::repeat-complex-registers (list aa0 a0) (list aa1 a1))
                (inst vfmaddsub231pd acc m0r aa0)
                (inst vfmadd231pd acc m1r aa1)
                (qvm-intrinsics::repeat-complex-registers (list aa2 a2) (list aa3 a3))
                (inst vfmadd231pd acc m2r aa2)
                (inst vfmadd231pd acc m3r aa3)
                (inst vextractf128 p acc #xFF)
                (inst vextractf128 q acc #x00))))


