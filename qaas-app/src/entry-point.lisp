;;;; qaas/src/entry-point.lisp
;;;;
;;;; Author: appleby

(in-package #:qvm-qaas)

(defparameter *option-spec*
  `((("help" #\h)
     :type boolean
     :optional t
     :documentation "display help")

    (("version" #\v)
     :type boolean
     :optional t
     :documentation "display the versions of the app and underlying QVM")

    (("verbose")
     :type boolean
     :optional t
     :documentation "display verbose output")

    (("num-workers" #\w)
     :type integer
     :initial-value 0
     :documentation "workers to use in parallel (0 => maximum number)")

    #-forest-sdk
    (("swank-port")
     :type integer
     :optional t
     :documentation "port to start a Swank server on")

    #-forest-sdk
    (("debug")
     :type boolean
     :optional t
     :documentation "debug mode, specifically this causes the QVM to not automatically catch execution errors allowing interactive debugging via SWANK.")

    (("check-sdk-version")
     :type boolean
     :initial-value nil
     :documentation "Check for a new SDK version at launch.")

    (("proxy")
     :type string
     :initial-value nil
     :documentation "Proxy to use when checking for an SDK update.")

    (("quiet")
     :type boolean
     :optional t
     :documentation "Disable all non-essential printed output to stdout (banner, etc.).")

    (("log-level")
     :type string
     :optional t
     :initial-value "debug"
     :documentation "maximum logging level (\"debug\", \"info\", \"notice\", \"warning\", \"err\", \"crit\", \"alert\", or \"emerg\")")))

(defun show-help ()
  (format t "Usage:~%")
  (format t "    ~A [<options>...]~%~%" *program-name*)
  (format t "Options:~%")
  (command-line-arguments:show-option-help *option-spec* :sort-names t))

(defun show-version ()
  (format t "~A [~A]~%" +QVM-VERSION+ +GIT-HASH+))

(defun show-welcome ()
  (format t "~&~
*************************************
* Welcome to the Rigetti QVM (QAAS) *~%~
************************************~%~
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

(defun check-libraries ()
  "Check that the foreign libraries are adequate. Exits with status
  0 if so, 1 if not."
  #+sbcl
  (format t "Loaded libraries:~%~{  ~A~%~}~%"
          (mapcar 'sb-alien::shared-object-pathname sb-sys:*shared-objects*))
  (unless (magicl.foreign-libraries:foreign-symbol-available-p "zuncsd_"
                                                               'magicl.foreign-libraries:liblapack)
    (format t "The loaded version of LAPACK is missing necessary functionality.~%")
    (quit-nicely 1))
  (format t "Library check passed.~%")
  (quit-nicely 0))

(defun quit-nicely (&optional (code 0))
  #+sbcl
  (sb-ext:exit :code code :abort nil)
  #-sbcl
  (uiop:quit code t))

(defun log-level-string-to-symbol (log-level)
  (let ((log-level-kw (assoc (intern (string-upcase log-level) 'keyword)
                             cl-syslog::*priorities*)))
    (unless log-level-kw
      (error "Invalid logging level: ~a" log-level))

    (car log-level-kw)))

(defun process-options (&key
                          version
                          check-libraries
                          verbose
                          help
                          #-forest-sdk swank-port
                          num-workers
                          #-forest-sdk debug
                          check-sdk-version
                          proxy
                          quiet
                          log-level)

  (setf *logger* (make-instance 'cl-syslog:rfc5424-logger
                                :app-name *program-name*
                                :facility ':local0
                                :maximum-priority (log-level-string-to-symbol log-level)
                                :log-writer
                                #+windows
                                (cl-syslog:stream-log-writer)
                                #-windows
                                (cl-syslog:tee-to-stream
                                 (cl-syslog:syslog-log-writer *program-name* :local0)
                                 *error-output*)))
  (when help
    (show-help)
    (quit-nicely))

  (when version
    (show-version)
    (quit-nicely))

  (when check-libraries
    (check-libraries))

  (when check-sdk-version
    (multiple-value-bind (available-p version)
        (sdk-update-available-p +QVM-VERSION+ :proxy proxy)
      (when available-p
        (format t "An update is available to the SDK. You have version ~A. ~
Version ~A is available from https://www.rigetti.com/forest~%"
                +QVM-VERSION+ version))
      (uiop:quit (if (and available-p version) 0 1))))

  (when verbose
    (setf qvm:*transition-verbose* t))

  #-forest-sdk
  (when debug
    (setf *debug* t))

  (cond
    ((zerop num-workers)
     (qvm:prepare-for-parallelization))
    (t
     (qvm:prepare-for-parallelization num-workers)
     (setf *num-workers* num-workers)))

  ;; Show the welcome message.
  (unless quiet (show-welcome))

  ;; Start Swank if we were asked. Re-enable the debugger.
  #-forest-sdk
  (when swank-port
    (enable-debugger)
    (format-log "Starting Swank on port ~D" swank-port)
    (setf swank:*use-dedicated-output-stream* nil)
    (swank:create-server :port swank-port
                         :dont-close t))

  (quit-nicely))

(defun command-line-debugger (condition previous-hook)
  (declare (ignore previous-hook))
  (format *error-output* "~&Fatal ~A: ~%  ~A~%"
          (type-of condition)
          condition)
  (force-output *error-output*)
  (quit-nicely 1))

(defun setup-debugger ()
  #+forest-sdk
  (setf *debugger-hook* 'command-line-debugger)
  #-forest-sdk
  (disable-debugger))

(defun %main (argv)
  (setup-debugger)

  (let ((*program-name* (pop argv)))
    (handler-case
        (handler-bind ((style-warning #'muffle-warning))
          (command-line-arguments:handle-command-line
           *option-spec*
           'process-options
           :command-line argv
           :name "qvm"
           :rest-arity nil))
      #+sbcl
      (sb-sys:interactive-interrupt (c)
        (declare (ignore c))
        (format-log "Caught Control-C. Quitting.")
        (quit-nicely))
      (error (c)
        (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
        (format-log :err "Error encountered, quitting.")
        (quit-nicely 1)))))

(defun asdf-entry-point ()
  (%main (list* "qaas" (uiop:command-line-arguments))))

