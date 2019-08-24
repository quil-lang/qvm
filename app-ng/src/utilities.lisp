;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defun safely-parse-quil-string (string)
  "Safely parse a Quil string STRING."
  (flet ((no-includes (path)
           (error "INCLUDE is disabled. Refusing to include ~A" path)))
    (let ((quil:*resolve-include-pathname* #'no-includes)
          (quil::*allow-unresolved-applications* t))
      (quil:parse-quil string))))
