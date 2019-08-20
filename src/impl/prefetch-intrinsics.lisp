;;;; sbcl-prefetch-intrinsics.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:qvm-intrinsics)

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
