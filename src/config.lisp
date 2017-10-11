;;;; src/config.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Configuration for QVM compile-time and run-time behavior.

(declaim (type (integer 0 (128)) *qubits-required-for-parallelization*))
(defparameter *qubits-required-for-parallelization* 19
  "The number of qubits required of a quantum state before it gets operated on in parallel.")

(defvar *transition-verbose* nil
  "Controls whether each transition is printed with a timing.")

(defvar *compile-before-running* nil
  "Compile programs loaded into the QVM before running them.")

(defvar *optimize-dangerously-fast*
  '(optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0))
  "Optimization qualities for when the code should go as fast as possible.")

(defvar *optimize-briskly*
  '(optimize speed (safety 1) (debug 1) (space 0))
  "Optimization qualities for when the code should go fast, but have safety.")

(defvar *optimize-safely*
  '(optimize (speed 0) (safety 3) (debug 3) (space 3))
  "Optimization qualities for when the code should emphasize safety and debugability.")
