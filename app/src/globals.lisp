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

(defvar *logger* (make-instance 'cl-syslog:rfc5424-logger
                                :app-name "qvm"
                                :facility ':local0
                                :log-writer (cl-syslog:null-log-writer))
  "The CL-SYSLOG logger instance.")

(defparameter *available-simulation-methods* '("pure-state" "density")
  "List of available simulation methods.")

(defparameter *available-allocation-kinds* '("native" "foreign")
  "Kinds of allocations that are possible.")

(global-vars:define-global-var **default-allocation**
    (lambda (n) (make-instance 'qvm:lisp-allocation :length n)))

(deftype simulation-method ()
  "Available QVM simulation methods."
  `(member pure-state                   ; Exact pure state evolution
           pauli-pure-state             ; Pauli channel
           stochastic-pure-state        ; Stochastic pure state evolution
           density                      ; Density matrix evolution
           ))

(defun string-to-simulation-method (string)
  ;; We do this so we don't dynamically intern uncollectable symbols.
  (alexandria:eswitch (string :test #'string=)
    ("pure-state"            'pure-state)
    ("pauli-pure-state"      'pauli-pure-state)
    ("stochastic-pure-state" 'stochastic-pure-state)
    ("density"               'density)))
