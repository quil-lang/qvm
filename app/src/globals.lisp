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

(global-vars:define-global-var **default-allocation**
    (lambda (n) (make-instance 'qvm:lisp-allocation :length n)))

(deftype simulation-method ()
  "Available QVM simulation methods."
  `(member pure-state full-density-matrix))

(defvar *simulation-method*  nil
  "The active QVM simulation method.

This is set once upon initialization of the QVM and is controlled by the --similation-method option")

(defvar *shared-memory-object-name* nil
  "The name of the POSIX shared memory object, or nil if none is present.")

(defvar *logger* (make-instance 'cl-syslog:rfc5424-logger
                                :app-name "qvm"
                                :facility ':local0
                                :log-writer (cl-syslog:null-log-writer))
  "The CL-SYSLOG logger instance.")
