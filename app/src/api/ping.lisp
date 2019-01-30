;;;; api/ping.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun handle-ping ()
  (format nil "pong ~D" (get-universal-time)))
