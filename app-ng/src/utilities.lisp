;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defun get-random-state (arg)
  (etypecase arg
    (null (qvm:seeded-random-state nil))
    (unsigned-byte (qvm:seeded-random-state arg))))

(defun hex-char-p (char)
  (case (char-downcase char)
    ((#\a #\b #\c #\d #\e #\f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     t)))
