;;;; src/config.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Configuration for QVM compile-time and run-time behavior.

(deftype parallelization-limit ()
  "A limit on the number of qubits that can be parallelized across."
  `(integer 0 50))

(declaim (type parallelization-limit *qubits-required-for-parallelization*))
(defparameter *qubits-required-for-parallelization* 19
  "The number of qubits required of a quantum state before it gets operated on in parallel.")

(defvar *transition-verbose* nil
  "Controls whether each transition is printed with a timing.")

(defvar *compile-before-running* nil
  "Compile programs loaded into the QVM before running them.")

(defvar *inline-static-gates-during-compilation* nil
  "Inline the actual gate matrices into the compiled instructions.")

(defvar *optimize-dangerously-fast*
  '(optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0))
  "Optimization qualities for when the code should go as fast as possible.")

(defvar *optimize-briskly*
  '(optimize speed (safety 1) (debug 1) (space 0))
  "Optimization qualities for when the code should go fast, but have safety.")

(defvar *optimize-safely*
  '(optimize (speed 0) (safety 3) (debug 3) (space 3))
  "Optimization qualities for when the code should emphasize safety and debugability.")

;;;
;;; ""64K ought to be enough for anybody." -Bill Gates" -Michael Scott
;;;
(global-vars:define-global-parameter **classical-memory-size-limit** (* 64 1024)
  "The limit of the number of octets that can be allocated for classical memory. Default is 64KiB.")
