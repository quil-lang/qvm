;;;; app-ng/src/entry-point.lisp
;;;;
;;;; Author: appleby

(in-package #:qvm-app-ng)

(defun show-help ()
  (format t "Usage:~%")
  (format t "    ~A [<options>...]~%~%" *program-name*)
  (format t "Options:~%")
  (qvm-app-ng.config:show-option-help))

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

(defun process-options (&key
                          help
                          version
                          verbose
                          server
                          host
                          port
                          qubits
                          num-workers
                          #-forest-sdk swank-port
                          #-forest-sdk debug
                          check-sdk-version
                          check-libraries
                          proxy
                          quiet
                          log-level
                          qubit-limit
                          memory-limit
                          safe-include-directory
                          allocation-method
                          simulation-method)
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

  (when allocation-method
    (setq **default-allocation** (allocation-description-maker allocation-method)))

  (when qubit-limit
    (setf *qubit-limit* qubit-limit))

  (when memory-limit
    (setf qvm::**classical-memory-size-limit** memory-limit))

  #-forest-sdk
  (when debug
    (setf *debug* t))

  (when safe-include-directory
    (setf *safe-include-directory* safe-include-directory))

  (cond
    ((zerop num-workers)
     (qvm:prepare-for-parallelization))
    (t
     (qvm:prepare-for-parallelization num-workers)
     (setf *num-workers* num-workers)))

  (unless quiet
    (show-welcome))

  ;; Start Swank if we were asked. Re-enable the debugger.
  #-forest-sdk
  (when swank-port
    (enable-debugger)
    (format-log "Starting Swank on port ~D" swank-port)
    (setf swank:*use-dedicated-output-stream* nil)
    (swank:create-server :port swank-port
                         :dont-close t))

  (when (not (null simulation-method))
    ;; Determine the simulation method, and set *SIMULATION-METHOD* appropriately
    (setf *simulation-method* (intern (string-upcase simulation-method) :qvm-app-ng))
    (when (and (eq *simulation-method* 'full-density-matrix)
               (null qubits))
      (format-log :err "Full density matrix simulation requires --qubits to be specified.")
      (quit-nicely 1)))

  (cond
    (server
     (start-server-mode :host host
                        :port port
                        :qubits qubits
                        :memory-limit memory-limit))
    (t
     (start-batch-mode :qubits qubits
                       :simulation-method simulation-method
                       :allocation-method allocation-method))))

(alexandria:define-constant +default-config-file-path+
    (merge-pathnames ".qvm_config" (user-homedir-pathname))
  :test #'equal
  :documentation "Default path to the QVM config file.")

(defun %main (argv)
  (setup-debugger)

  (let ((*program-name* (pop argv)))
    (handler-case
        (handler-bind ((style-warning #'muffle-warning))
          (qvm-app-ng.config:handle-config argv
                                           (and (uiop:file-exists-p +default-config-file-path+)
                                                +default-config-file-path+)
                                           #'process-options))
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

