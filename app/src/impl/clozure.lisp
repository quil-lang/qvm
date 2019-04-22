;;;; clozure.lisp

(in-package #:qvm-app)

(defun zap-info ()
  ;; No-op on Clozure
  )

(defun disable-debugger ()
  (setf ccl::*batch-flag* t))

(defun enable-debugger ()
  (setf ccl::*batch-flag* nil))
