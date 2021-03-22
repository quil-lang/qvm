;;;; sbcl.lisp

(in-package #:qvm-app-ng)

(defun disable-debugger ()
  (sb-ext:disable-debugger))

(defun enable-debugger ()
  (sb-ext:enable-debugger))
