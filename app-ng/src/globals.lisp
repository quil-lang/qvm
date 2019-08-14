;;;; app-ng/src/globals.lisp
;;;;
;;;; Author: appleby

(in-package #:qvm-app-ng)

(defvar *qubit-limit* nil)
(defvar *num-workers* nil)
(defvar *debug* nil)
(defvar *program-name* "qvm-ng")
(defvar *safe-include-directory* nil)
(defvar swank:*use-dedicated-output-stream*)

(deftype simulation-method ()
  "Available QVM simulation methods."
  `(member pure-state full-density-matrix))

(defvar *simulation-method*  nil
  "The active QVM simulation method.

This is set once upon initialization of the QVM and is controlled by the --similation-method option")

(defparameter *available-simulation-methods* '("pure-state" "full-density-matrix")
  "List of available simulation methods.")

(defparameter *available-allocation-kinds* '("native" "foreign")
  "Kinds of allocations that are possible.")

(global-vars:define-global-var **default-allocation**
    (lambda (n) (make-instance 'qvm:lisp-allocation :length n)))

