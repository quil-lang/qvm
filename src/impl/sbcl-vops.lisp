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

(defknown (%2x2matrix-to-simd) (cdf cdf cdf cdf)
    (values d4 d4 d4 d4)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

(defknown (%matmul2-simd) (d4 d4 d4 d4 cdf cdf)
    (values cdf cdf)
    (movable flushable always-translatable)
  :overwrite-fndb-silently t)

;;; VOP definitions

(in-package #:sb-vm)

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
  (:temporary (:sc double-avx2-reg) aa bb aa-swzld bb-swzld res)
  (:generator 4
              (inst vinsertf128 aa a a #xFF)      ; Store a in aa twice [Ar Ai Ar Ai]
              (inst vinsertf128 bb b b #xFF)      ; Store b in bb twice [Br Bi Br Bi]
              (inst vpermpd aa-swzld aa #4r2301)  ; Store a in aa-swzld reversed twice [Ai Ar Ai Ar]
              (inst vpermpd bb-swzld bb #4r2301)  ; Store b in bb-swzld reversed twice [Bi Br Bi Br]
              (inst vxorpd res res res)           ; Set res to zero
              (inst vfmadd231pd res vyi aa-swzld) ; Multiply complex parts of a and add to res
              (inst vfmadd231pd res xzi bb-swzld) ; Multiply complex parts of b and add to res
              (inst vfmaddsub231pd res vyr aa)    ; Multiply real parts of a and add to res, negating complex parts
              (inst vfmadd231pd res xzr bb)       ; Multiply real parts of b and add to res
              (inst vextractf128 p res #xFF)      ; Copy the upper 2 doubles from res to p
              (inst vextractf128 q res #x00)))    ; Copy the lower 2 doubles from res to q

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
  (:temporary (:sc double-avx2-reg) vy xz) ; NOTE: Could probably be changed to not use temp registers
  (:generator 4
              (inst vinsertf128 vy y v #xFF) ; vy = [Vr Vi Yr Yi]
              (inst vinsertf128 xz z x #xFF) ; xz = [Xr Xi Zr Zi]
              (inst vpermpd vyr vy #4r2200)  ; vyr = [Vr Vr Yr Yr]
              (inst vpermpd vyi vy #4r3311)  ; vyi = [Vi Vi Yi Yi]
              (inst vpermpd xzr xz #4r2200)  ; xzr = [Xr Xr Zr Zr]
              (inst vpermpd xzi xz #4r3311)  ; xzr = [Xi Xi Zi Zi]
              ))
