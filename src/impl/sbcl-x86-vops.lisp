;;;; sbcl-x86-vops.lisp
;;;;
;;;; Author: Cole Scott
;;;;         
;;;; Collaborators: Robert Smith

(in-package #:qvm-intrinsics)

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

