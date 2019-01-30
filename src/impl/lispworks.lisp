;;;; lispworks.lisp

(in-package #:qvm)

(defun call-at-exit (fun)
  (lw:define-action "When quitting image" "Deallocate shared memories"
    fun))
