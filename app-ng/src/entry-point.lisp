;;;; app-ng/src/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defparameter *available-simulation-methods* '("pure-state" "full-density-matrix")
  "List of available simulation methods.")

(defparameter *available-allocation-kinds* '("native" "foreign")
  "Kinds of allocations that are possible.")

(defparameter *option-spec*
  `((("default-allocation")
     :type string
     :initial-value "native"
     :documentation "select where wavefunctions get allocated: \"native\" (default) for native allocation, \"foreign\" for C-compatible allocation")

    (("help" #\h)
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

    (("server" #\S)
     :type boolean
     :optional t
     :initial-value nil
     :documentation "start a QVM server")

    (("host")
     :type string
     :optional t
     :initial-value ,+default-server-address+
     :documentation "host on which to start the QVM server")

    (("port" #\p)
     :type integer
     :optional t
     :initial-value ,+default-server-port+
     :documentation "port to start the QVM server on")

    (("qubits" #\q)
     :type integer
     :optional t
     :documentation "Number of qubits to force to use.")

    (("num-workers" #\w)
     :type integer
     :initial-value 0
     :documentation "workers to use in parallel (0 => maximum number)")

    (("time-limit")
     :type integer
     :initial-value 0
     :documentation "time limit for computations (0 => unlimited, ms)")

    (("qubit-limit")
     :type integer
     :initial-value 0
     :documentation "maximum number of qubits allowed to be used in the server (0 => unlimited)")

    (("safe-include-directory")
     :type string
     :optional t
     :documentation "allow INCLUDE only files in this directory")

    (("simulation-method")
     :type string
     :initial-value "pure-state"
     :documentation ,(format nil "the method of qvm simulation; includes {~{~S~^, ~}}. Note that FULL-DENSITY-MATRIX simulation requires that --qubits be specified."
                             *available-simulation-methods*))

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

(defun allocation-description-maker (kind)
  "Return a function INTEGER -> ALLOCATION that takes a number of elements and returns a proper descriptor for the allocation."
  (cond
    ((string-equal kind "native")
     (lambda (length)
       (make-instance 'qvm:lisp-allocation :length length)))
    ((string-equal kind "foreign")
     (lambda (length)
       (make-instance 'qvm:c-allocation :length length)))
    (t
     (error "Invalid kind of allocation ~S, wanted any of {~{~S~^, ~}"
            kind
            *available-allocation-kinds*))))

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
                          default-allocation
                          help
                          server
                          host
                          port
                          #-forest-sdk swank-port
                          num-workers
                          time-limit
                          qubit-limit
                          safe-include-directory
                          qubits
                          simulation-method
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

  (when default-allocation
    (setq **default-allocation** (allocation-description-maker default-allocation)))

  (when (plusp time-limit)
    (setf *time-limit* (/ time-limit 1000.0d0)))

  (when (plusp qubit-limit)
    (setf *qubit-limit* qubit-limit))

  (unless (or (null *safe-include-directory*)
              (uiop:directory-pathname-p *safe-include-directory*))
    (error "--safe-include-directory must point to a directory. Got ~S. Did you ~
            forget a trailing slash?"
           *safe-include-directory*))

  (setf *safe-include-directory* safe-include-directory)

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

  (unless (or (null qubits) (not (minusp qubits)))
    (error "--qubits must be a non-negative integer. Got ~A." qubits))

  (unless (member simulation-method *available-simulation-methods* :test #'string-equal)
    (error "Invalid simulation method: ~S" simulation-method))

  ;; Determine the simulation method, and set *SIMULATION-METHOD* appropriately
  (setf *simulation-method* (intern (string-upcase simulation-method) :qvm-app-ng))
  (when (and (eq *simulation-method* 'full-density-matrix)
             (null qubits))
    (format-log :err "Full density matrix simulation requires --qubits to be specified.")
    (quit-nicely 1))

  (format-log "Selected simulation method: ~A" simulation-method)

  (when server
    (start-server-mode :host host :port port))

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
           :name *program-name*
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
  (%main (list* *program-name* (uiop:command-line-arguments))))

