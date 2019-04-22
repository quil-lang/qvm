;;;; obliviate.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun |POST-obliviate| (request)
  (declare (ignore request))
  ;; Blast away all bookkept QVMs.
  (let ((n (hash-table-count **persistent-qvms**)))
    (clrhash **persistent-qvms**)
    ;; Return the number of entries deleted.
    (yason-encode-to-string n)))
