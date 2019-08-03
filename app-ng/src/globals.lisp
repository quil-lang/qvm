;;;; app-ng/src/globals.lisp
;;;;
;;;; Author: appleby

(in-package #:qvm-app-ng)

(defvar *num-workers* 1)
(defvar *debug* nil)
(defvar *program-name* "qvm-ng")
(defvar swank:*use-dedicated-output-stream*)

(defvar *logger* (make-instance 'cl-syslog:rfc5424-logger
                                :app-name *program-name*
                                :facility ':local0
                                :log-writer (cl-syslog:null-log-writer))
  "The CL-SYSLOG logger instance.")
