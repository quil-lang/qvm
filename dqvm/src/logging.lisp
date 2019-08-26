;;;; src/logging.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(defvar *logger* nil "Global logging facility (per MPI rank).")
(defparameter *log-level* :debug "Default log level")

(defun make-logger (name &key (maximum-priority *log-level*))
  "Create a logger called NAME with priority specified by MAXIMUM-PRIORITY."
  (make-instance 'cl-syslog:rfc5424-logger
                 :app-name name
                 :facility ':local0
                 :maximum-priority maximum-priority
                 :log-writer (cl-syslog:stream-log-writer uiop/stream:*stderr*)))

(defun format-log (level &rest rest)
  "Send a message to syslog."
  (apply #'cl-syslog:format-log *logger* level rest))

(defun setup-logger (&optional message)
  "Easy set up of logger dynamic variable."
  (setf *logger* (make-logger (format nil "dqvm-~D" (mpi-comm-rank))))
  (when (and message (zerop (mpi-comm-rank)))
    (format-log :info message))

  ;; The barrier below is there for aesthetic reasons only. It enforces that the welcome message is the first line in the log file.
  (barrier))
