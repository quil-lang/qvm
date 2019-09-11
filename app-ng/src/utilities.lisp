;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defun get-random-state (arg)
  (etypecase arg
    (null (qvm:seeded-random-state nil))
    (unsigned-byte (qvm:seeded-random-state arg))))

(defun hex-char-p (char &key (case-sensitive-p t))
  (case (if case-sensitive-p char (char-downcase char))
    ((#\a #\b #\c #\d #\e #\f #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     t)))

;; Stolen from HUNCHENTOOT::ISO-TIME
(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun run-program-on-qvm (qvm parsed-program)
  "Load and run PARSED-PROGRAM on the given QVM."
  (qvm:load-program qvm parsed-program :supersede-memory-subsystem t)
  (qvm:run qvm))


