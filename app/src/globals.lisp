;;;; src/globals.lisp
;;;;
;;;; Author: Nikolas Tezak
;;;;         Robert Smith

(in-package #:qvm-app)

(defvar *qubit-limit* nil)              ; Maximum no. of qubits.
(defvar *num-workers* nil)
(defvar *time-limit* nil)
(defvar *safe-include-directory* nil)
(defvar *app* nil)
(defvar *debug* nil)

(global-vars:define-global-var **persistent-wavefunction** nil)
(global-vars:define-global-var **persistent-wavefunction-finalizer** (constantly nil))

(deftype simulation-method ()
  "Available QVM simulation methods."
  `(member pure-state full-density-matrix))

(defvar *simulation-method*  nil
  "The active QVM simulation method.

This is set once upon initialization of the QVM and is controlled by the --similation-method option")

(defvar *shared-memory-object-name* nil
  "The name of the POSIX shared memory object, or nil if none is present.")
