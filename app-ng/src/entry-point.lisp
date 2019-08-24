;;;; app-ng/src/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defparameter *option-spec* '())

(defun show-help ()
  (format t "Usage:~%")
  (format t "    qvm-ng [<options>...]~%~%")
  (format t "Options:~%")
  (command-line-arguments:show-option-help *option-spec* :sort-names t))

(defun show-version ()
  (format t "~A [~A]~%" +QVM-VERSION+ +GIT-HASH+))

(defun show-welcome ()
  (format t "~&~
*****************************************~%~
* Welcome to the Rigetti QVM (Next Gen) *~%~
*****************************************~%~
Copyright (c) 2016-2019 Rigetti Computing.~2%")
  (format t "(Configured with ~A MiB of workspace and ~D core~:P.)~2%"
          #+sbcl
          (floor (sb-ext:dynamic-space-size) (expt 1024 2))
          #-sbcl
          "many many"
          (max 1 (qvm:count-logical-cores)))
  nil)

(defun generalized-boolean-to-exit-code (successp)
  (cond ((integerp successp) successp)
        ((null successp) 1)
        (t 0)))

(defun quit-nicely (&optional (successp t)
                    &aux (code (generalized-boolean-to-exit-code successp)))
  #+sbcl
  (sb-ext:exit :code code :abort nil)
  #-sbcl
  (uiop:quit code t))

(defun %main (argv)
  (handler-case
      (progn
        (show-welcome)
        (setf *logger* (make-logger "qvm-ng" ':debug))
        (start-server-mode :host +default-server-address+ :port +default-server-port+)
        (quit-nicely 0))
    #+sbcl
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (format-log "Caught Control-C. Quitting.")
      (quit-nicely 0))
    (error (c)
      (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
      (format-log :err "Error encountered, quitting.")
      (quit-nicely 1))))

(defun asdf-entry-point ()
  (%main (uiop:command-line-arguments)))

