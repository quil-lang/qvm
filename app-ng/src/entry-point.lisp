;;;; app-ng/src/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defparameter *option-spec* '())

(defun show-help ()
  (format t "Usage:~%")
  (format t "    ~A [<options>...]~%~%" *program-name*)
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
  #+forest-sdk
  (format t "This is a part of the Forest SDK. By using this program~%~
             you agree to the End User License Agreement (EULA) supplied~%~
             with this program. If you did not receive the EULA, please~%~
             contact <support@rigetti.com>.~2%")
  (format t "(Configured with ~A MiB of workspace and ~D worker~:P.)~2%"
          #+sbcl
          (floor (sb-ext:dynamic-space-size) (expt 1024 2))
          #-sbcl
          "many many"
          (or *num-workers* (max 1 (qvm:count-logical-cores))))
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
  (let ((*program-name* (pop argv)))
    (handler-case
        (progn
          (show-welcome)
          (quit-nicely 0))
      #+sbcl
      (sb-sys:interactive-interrupt (c)
        (declare (ignore c))
        (format-log "Caught Control-C. Quitting.")
        (quit-nicely 0))
      (error (c)
        (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
        (format-log :err "Error encountered, quitting.")
        (quit-nicely 1)))))

(defun asdf-entry-point ()
  (%main (list* *program-name* (uiop:command-line-arguments))))

