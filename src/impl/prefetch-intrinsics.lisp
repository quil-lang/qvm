;;;; sbcl-prefetch-intrinsics.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:qvm-intrinsics)
(declaim (optimize speed))

(defun prefetch (type base displacement stride index)
  (declare (ignore type)  ;; always do t0
           (type sb-sys:system-area-pointer base)
           ;(type (signed-byte #.sb-vm:n-word-bits) displacement)
           (type (integer 0 0) displacement)
           (ignore displacement)
           (type (member 2 4 8 16) stride)
           (type fixnum index))
  (case stride
    (2
     (%prefetch :t0 base 0 2 index))
    (4
     (%prefetch :t0 base 0 4 index))
    (8
     (%prefetch :t0 base 0 8 index))
    (16
     (%prefetch :t0 base 0 16 index))))

(defmacro define-prefetch (name num)
  (check-type name symbol)
  (check-type num unsigned-byte)
  `(progn (declaim (inline ,name))
          (defun ,name (base index)
            (declare (type sb-sys:system-area-pointer base)
                     (type fixnum index))
            (%prefetch :t0 base 0 ,num index))
          (declaim (notinline ,name))))

(define-prefetch prefetch2 2)
(define-prefetch prefetch4 4)
(define-prefetch prefetch8 8)
(define-prefetch prefetch16 16)

(declaim (inline prefetch2 prefetch4 prefetch8 prefetch16))
(defun prefetch2 (base index)
  (declare (type sb-sys:system-area-pointer base)
           (type fixnum index))
  (%prefetch :t0 base 0 2 index))
(defun prefetch4 (base index)
  (declare (type sb-sys:system-area-pointer base)
           (type fixnum index))
  (%prefetch :t0 base 0 4 index))
(defun prefetch8 (base index)
  (declare (type sb-sys:system-area-pointer base)
           (type fixnum index))
  (%prefetch :t0 base 0 8 index))
(defun prefetch16 (base index)
  (declare (type sb-sys:system-area-pointer base)
           (type fixnum index))
  (%prefetch :t0 base 0 16 index))
(declaim (notinline prefetch2 prefetch4 prefetch8 prefetch16))
