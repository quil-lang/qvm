;;;; build-configuration.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file contains configuration of the QVM package for use with
;;; the executable.
;;;
;;; In general, one would see changes to 'config.lisp' constants here.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "~&Warming the operator cache for ~D qubit~:P..."
          #1=*executable-time-operator-cache-limit*)
  (finish-output)
  (warm-apply-matrix-operator-cache :max-qubits #1#)
  (format t "done~%")
  (finish-output))
