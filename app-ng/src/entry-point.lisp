;;;; app-ng/src/entry-point.lisp
;;;;
;;;; Author: appleby

(in-package #:qvm-app-ng)

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
                          log-level
                          (batch t)
                          server
                          simulation-method
                          allocation-method
                          memory-limit
                          safe-include-directory
                          qubits)
  "Process all options, setting global variables, and process input or start server."
  ;; Initialize global configuration first by loading the config file.
  ;; TODO Specify the file on command line.
  (load-config-file :reset t)

  (initialize-logger *program-name* log-level)

  (when help
    (show-help)
    (quit-nicely))

  (when version
    (show-version)
    (quit-nicely))

  (when check-libraries
    (quit-nicely (check-libraries)))

  (when check-sdk-version
    (quit-nicely (check-sdk-version :proxy proxy)))

  (when verbose
    (setf qvm:*transition-verbose* t))

  #-forest-sdk
  (when debug
    (setf *debug* t))

  (cond
    ((minusp num-workers)
     (error "Cannot create a negative number (~D) of workers." num-workers))
    ((zerop num-workers)
     (qvm:prepare-for-parallelization)
     (setf (gethash "num-workers" **config**) 0))
    (t
     (qvm:prepare-for-parallelization num-workers)
     (setf (gethash "num-workers" **config**) num-workers)))

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

  (when safe-include-directory
    (setf (gethash "safe-include-directory" **config**) safe-include-directory))
  
  (cond
    (server
     (start-server-mode :host host :port port :qubits qubits :memory-limit memory-limit))
    ;; BATCH is by default T (see above). This is to facilitate testing
    ;; PROCESS-OPTIONS by passing :BATCH nil and :SERVER nil. Maybe
    ;; needs a better design, to separate "processing of options" from
    ;; "acting on options".
    (batch
     (start-batch-mode :qubits qubits
                       :simulation-method simulation-method
                       :allocation-method allocation-method))))

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
        (quit-nicely 0))
      (error (c)
        (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
        (format-log :err "Error encountered, quitting.")
        (quit-nicely 1)))))

(defun asdf-entry-point ()
  (%main (list* *program-name* (uiop:command-line-arguments))))

