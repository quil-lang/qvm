;;;; sbcl-avx-vops.lisp
;;;;
;;;; Author: Cole Scott
;;;;         
;;;; Collaborators: Jussi Kukkonen
;;;;                Robert Smith

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
;;
;; NOTE: All registers are expressed highest to lowest place values (indices [3 2 1 0])
;;
;; NOTE: Common Lisp stores complex doubles with the imaginary part in the higher place value:
;;   c = [ ci cr ]
;;

(in-package #:sb-vm)

;; 2x2matrix-to-simd
;;
;; Creates packed real and imaginary vectors of columns in 2x2 matrix
;;
;; ARGS: 2x2 complex matrix
;;   +-----+-----+
;;   | m00 | m01 |
;;   +-----+-----+
;;   | m10 | m11 |
;;   +-----+-----+
;;      ^     ^
;;      m0    m1
;;
;; where mXX = [ mXXi mXXr ]
;;
;; RESULTS:
;;   m0r = [ m10r m10r m00r m00r ]
;;   m0i = [ m10i m10i m00i m00i ]
;;   m1r = [ m11r m11r m01r m01r ]
;;   m1i = [ m11i m11i m01i m01i ]
;;
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
              (inst vinsertf128 m0 m10 m00 #xFF) ; m0 = [ m10i m10r m00i m00r ]
              (inst vinsertf128 m1 m11 m01 #xFF) ; m1 = [ m11i m11r m01i m01r ]
              (inst vpermpd m0r m0 #4r2200)      ; m0r = [ m0[2] m0[2] m0[0] m0[0] ] = [ m10r m10r m00r m00r ]
              (inst vpermpd m0i m0 #4r3311)      ; m0i = [ m0[3] m0[3] m0[1] m0[1] ] = [ m10i m10i m00i m00i ]
              (inst vpermpd m1r m1 #4r2200)      ; m1r = [ m1[2] m1[2] m1[0] m1[0] ] = [ m11r m11r m01r m01r ]
              (inst vpermpd m1i m1 #4r3311)))    ; m1i = [ m1[3] m1[3] m1[1] m1[1] ] = [ m11i m11i m01i m01i ]

;; 2x4matrix-to-simd
;;
;; Creates packed real and imaginary vectors of columns in 2x4 matrix
;;
;; ARGS: 2x4 complex matrix
;;   +-----+-----+-----+-----+
;;   | m00 | m01 | m02 | m03 |
;;   +-----+-----+-----+-----+
;;   | m10 | m11 | m12 | m13 |
;;   +-----+-----+-----+-----+
;;      ^     ^     ^     ^
;;      m0    m1    m2    m3
;;
;; where mXX = [ mXXi mXXr ]
;;
;; RESULTS:
;;   m0r = [ m10r m10r m00r m00r ]
;;   m0i = [ m10i m10i m00i m00i ]
;;   m1r = [ m11r m11r m01r m01r ]
;;   m1i = [ m11i m11i m01i m01i ]
;;   m2r = [ m12r m12r m02r m02r ]
;;   m2i = [ m12i m12i m02i m02i ]
;;   m3r = [ m13r m13r m03r m03r ]
;;   m3i = [ m13i m13i m03i m03i ]
;;
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
              (inst vinsertf128 m0 m10 m00 #xFF) ; m0 = [ m10i m10r m00i m00r ]
              (inst vinsertf128 m1 m11 m01 #xFF) ; m1 = [ m11i m11r m01i m01r ]
              (inst vinsertf128 m2 m12 m02 #xFF) ; m2 = [ m12i m12r m02i m02r ]
              (inst vinsertf128 m3 m13 m03 #xFF) ; m3 = [ m13i m13r m03i m03r ]
              (inst vpermpd m0r m0 #4r2200)      ; m0r = [ m0[2] m0[2] m0[0] m0[0] ] = [ m10r m10r m00r m00r ]
              (inst vpermpd m0i m0 #4r3311)      ; m0i = [ m0[3] m0[3] m0[1] m0[1] ] = [ m10i m10i m00i m00i ]
              (inst vpermpd m1r m1 #4r2200)      ; m1r = [ m1[2] m1[2] m1[0] m1[0] ] = [ m11r m11r m01r m01r ]
              (inst vpermpd m1i m1 #4r3311)      ; m1i = [ m1[3] m1[3] m1[1] m1[1] ] = [ m11i m11i m01i m01i ]
              (inst vpermpd m2r m2 #4r2200)      ; m2r = [ m2[2] m2[2] m2[0] m2[0] ] = [ m12r m12r m02r m02r ]
              (inst vpermpd m2i m2 #4r3311)      ; m2i = [ m2[3] m2[3] m2[1] m2[1] ] = [ m12i m12i m02i m02i ]
              (inst vpermpd m3r m3 #4r2200)      ; m3r = [ m3[2] m3[2] m3[0] m3[0] ] = [ m13r m13r m03r m03r ]
              (inst vpermpd m3i m3 #4r3311)))    ; m3i = [ m3[3] m3[3] m3[1] m3[1] ] = [ m13i m13i m03i m03i ]

(defun qvm-intrinsics::repeat-complex-registers (&rest args)
  "Store a XMM register to upper and lower half of YMM register

ARGS:
Lists of the form (dest src)
dest and src are of type complex double

dest = [ src[1] src[0] src[1] src[0] ]"
  (loop :for (dest src) :in args
        :do (inst vinsertf128 dest src src #xFF)))

(defun qvm-intrinsics::swizzle-complex-registers (&rest args)
  "Store a XMM register to upper and lower half of YMM register, interchanging real and imaginary parts

ARGS:
Lists of the form (dest src)
dest and src are of type complex double

dest = [ src[0] src[1] src[0] src[1] ]"
  (loop :for (dest src) :in args
        :do (inst vpermpd dest src #4r0101)))

;; matmul2-simd
;;
;; Multiplies a 2x2 matrix by a 2x1 vector
;;
;; ARGS:
;;   m0r, m0i, m1r, m1i: Packed real/imag vectors from 2x2matrix-to-simd
;;   a0, a1: Column vector
;;
;; RESULT:
;;   p, q: Components of 2x1 complex double matrix result
;;
(define-vop (qvm-intrinsics::matmul2-simd)
  (:translate qvm-intrinsics::%matmul2-simd)
  (:policy :fast-safe)
  (:args (m0r :scs (double-avx2-reg))
         (m0i :scs (double-avx2-reg))
         (m1r :scs (double-avx2-reg))
         (m1i :scs (double-avx2-reg))
         (a0 :scs (complex-double-reg) :target p)
         (a1 :scs (complex-double-reg) :target q))
  (:arg-types simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              simd-pack-256-double
              complex-double-float
              complex-double-float)
  (:results (p :scs (complex-double-reg))
            (q :scs (complex-double-reg)))
  (:result-types complex-double-float complex-double-float)
  (:temporary (:sc double-avx2-reg) aa0 aa1 acc)
  (:generator 4
              (let ((aa0-swzld aa0) ; Save 2 registers by storing swizzled values in same register
                    (aa1-swzld aa1))
                (qvm-intrinsics::swizzle-complex-registers
                 (list aa0-swzld a0)                 ; aa0-swzld = [ a0r a0i a0r a0i ]
                 (list aa1-swzld a1))                ; aa1-swzld = [ a1r a1i a1r a1i ]
                (inst vmulpd acc m0i aa0-swzld)      ; acc  = [ (m10i * a0r) (m10i * a0i) (m00i * a0r) (m00i * a0i) ]
                (inst vfmadd231pd acc m1i aa1-swzld) ; acc += [ (m11i * a1r) (m11i * a1i) (m01i * a1r) (m01i * a1i) ]
                (qvm-intrinsics::repeat-complex-registers
                 (list aa0 a0)                       ; aa0 = [ a0i a0r a0i a0r ]
                 (list aa1 a1))                      ; aa1 = [ a1i a1r a1i a1r ]
                (inst vfmaddsub231pd acc m0r aa0)    ; acc = [ (m10r * a0r) (m10r * a0i) (m00r * a0r) (m00r * a0i) ]
                                                     ;     + [ acc[3] -acc[2] acc[1] -acc[0] ]
                                                     ; (This negates the i^2 components)
                (inst vfmadd231pd acc m1r aa1)       ; acc += [ (m11r * a1r) (m11r * a1i) (m01r * a1r) (m01r * a1i) ]
                (inst vextractf128 p acc #xFF)       ; p = [ acc[3] acc[2] ]
                (inst vextractf128 q acc #x00))))    ; q = [ acc[1] acc[0] ]

;; matmul2-simd-real
;;
;; Multiplies a real-only 2x2 matrix by a 2x1 vector
;;
;; ARGS:
;;   m0r, m1r: Packed real vectors from 2x2matrix-to-simd
;;   a0, a1: Column vector
;;
;; RESULT:
;;   p, q: Components of 2x1 complex double matrix result
;;
(define-vop (qvm-intrinsics::matmul2-simd-real)
  (:translate qvm-intrinsics::%matmul2-simd-real)
  (:policy :fast-safe)
  (:args (m0r :scs (double-avx2-reg))
         (m1r :scs (double-avx2-reg))
         (a0 :scs (complex-double-reg) :target p)
         (a1 :scs (complex-double-reg) :target q))
  (:arg-types simd-pack-256-double
              simd-pack-256-double
              complex-double-float
              complex-double-float)
  (:results (p :scs (complex-double-reg))
            (q :scs (complex-double-reg)))
  (:result-types complex-double-float complex-double-float)
  (:temporary (:sc double-avx2-reg) aa0 aa1)
  (:generator 4
              (let ((acc aa0)) ; Save a register by using the first used temp to also store accumulator
                (qvm-intrinsics::repeat-complex-registers
                 (list aa0 a0)                    ; aa0 = [ a0i a0r a0i a0r ]
                 (list aa1 a1))                   ; aa1 = [ a1i a1r a1i a1r ]
                (inst vmulpd acc m0r aa0)         ; acc  = [ (m10r * a1r) (m10r * a1i) (m00r * a1r) (m00r * a1i)
                (inst vfmadd231pd acc m1r aa1)    ; acc += [ (m11r * a1r) (m11r * a1i) (m01r * a1r) (m01r * a1i)
                (inst vextractf128 p acc #xFF)    ; p = [ acc[3] acc[2] ]
                (inst vextractf128 q acc #x00)))) ; q = [ acc[1] acc[0] ]

;; matmul4-simd-half
;;
;; Multiplies a 2x4 matrix (half of a 4x4 matrix) by a 4x1 vector
;;
;; ARGS:
;;   m0r, m0i, m1r, m1i, m2r, m2i, m3r, m3i: Packed real/imag vectors from 2x4matrix-to-simd
;;   a0, a1, a2, a3: Column vector
;;
;; RESULT:
;;   p, q: Components of 2x1 complex double matrix result
;;
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
              (let* ((aa2 aa0)          ; Save registers by reusing some old ones
                     (aa3 aa1)
                     (aa0-swzld aa0)
                     (aa1-swzld aa1)
                     (aa2-swzld aa2)
                     (aa3-swzld aa3))
                (qvm-intrinsics::swizzle-complex-registers
                 (list aa0-swzld a0)                 ; aa0-swzld = [ a0r a0i a0r a0i ]
                 (list aa1-swzld a1))                ; aa1-swzld = [ a1r a1i a1r a1i ]
                (inst vmulpd acc m0i aa0-swzld)      ; acc  = [ (m10i * a0r) (m10i * a0i) (m00i * a0r) (m00i * a0i) ]
                (inst vfmadd231pd acc m1i aa1-swzld) ; acc += [ (m11i * a1r) (m11i * a1i) (m01i * a1r) (m01i * a1i) ]
                (qvm-intrinsics::swizzle-complex-registers
                 (list aa2-swzld a2)                 ; aa2-swzld = [ a2r a2i a2r a2i ]
                 (list aa3-swzld a3))                ; aa3-swzld = [ a3r a3i a3r a3i ]
                (inst vfmadd231pd acc m2i aa2-swzld) ; acc += [ (m12i * a2r) (m12i * a2i) (m02i * a2r) (m02i * a2i) ]
                (inst vfmadd231pd acc m3i aa3-swzld) ; acc += [ (m13i * a3r) (m13i * a3i) (m03i * a1r) (m03i * a3i) ]
                (qvm-intrinsics::repeat-complex-registers
                 (list aa0 a0)                       ; aa0 = [ a0i a0r a0i a0r ]
                 (list aa1 a1))                      ; aa1 = [ a1i a1r a1i a1r ]
                (inst vfmaddsub231pd acc m0r aa0)    ; acc = [ (m10r * a0r) (m10r * a0i) (m00r * a0r) (m00r * a0i) ]
                                                     ;     + [ acc[3] -acc[2] acc[1] -acc[0] ]
                                                     ; (This negates the i^2 components)
                (inst vfmadd231pd acc m1r aa1)       ; acc += [ (m11r * a1r) (m11r * a1i) (m01r * a1r) (m01r * a1i) ]
                (qvm-intrinsics::repeat-complex-registers
                 (list aa2 a2)                       ; aa2 = [ a2i a2r a2i a2r ]
                 (list aa3 a3))                      ; aa3 = [ a3i a3r a3i a3r ]
                (inst vfmadd231pd acc m2r aa2)       ; acc += [ (m12r * a2r) (m12r * a2i) (m02r * a2r) (m01r * a2i) ]
                (inst vfmadd231pd acc m3r aa3)       ; acc += [ (m13r * a3r) (m13r * a3i) (m03r * a3r) (m01r * a3i) ]
                (inst vextractf128 p acc #xFF)       ; p = [ acc[3] acc[2] ]
                (inst vextractf128 q acc #x00))))    ; q = [ acc[1] acc[0] ]

