;;;; api/trivial-handlers.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defun handle-version ()
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (*standard-output*)
     (show-version))))
