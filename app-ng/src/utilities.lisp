;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defun get-random-state (arg)
  (etypecase arg
    (null (qvm:seeded-random-state nil))
    (unsigned-byte (qvm:seeded-random-state arg))))
