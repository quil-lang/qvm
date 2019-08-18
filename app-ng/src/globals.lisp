;;;; app-ng/src/globals.lisp
;;;;
;;;; Author: Nikolas Tezak
;;;;         Robert Smith

(in-package #:qvm-app-ng)

(defvar *app* nil)
(defvar *num-workers* nil)
(defvar *qubit-limit* nil)
(defvar *debug* nil)
(defvar *program-name* "qvm-ng")
(defvar swank:*use-dedicated-output-stream*)

(defvar *logger* (make-instance 'cl-syslog:rfc5424-logger
                                :app-name *program-name*
                                :facility ':local0
                                :log-writer (cl-syslog:null-log-writer))
  "The CL-SYSLOG logger instance.")
