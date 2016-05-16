;;;; qil.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; QIL parsing. For now, this is just S-expressions.

(defun slurp-forms (stream)
  "Slurp all of the forms of the stream STREAM into a list."
  (loop :with eof := (gensym "EOF")
        :for form := (read stream nil eof nil) :then (read stream nil eof nil)
        :until (eq eof form)
        :collect form))

(defun read-qil-file (filespec)
  (let ((*read-eval* nil))
    (with-open-file (s filespec :direction ':input
                                :if-does-not-exist ':error)
      (let ((*package* (find-package :qvm)))
        (slurp-forms s)))))
