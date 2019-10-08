;;;; app-ng/src/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defparameter *option-spec*
  `((("help" #\h)
     :type boolean
     :optional t
     :documentation "Display help")
    (("host")
     :type string
     :initial-value ,+default-server-address+
     :documentation "Host on which to start the QVM server")
    (("log-level")
     :type string
     :initial-value "debug"
     :documentation ,(format nil "Maximum logging level. Must be one of ~{~(~A~)~^, ~}"
                             +available-log-levels+))
    (("port" #\p)
     :type integer
     :initial-value ,+default-server-port+
     :documentation "Port to start the QVM server on")
    (("rpc-request")
     :type string
     :initial-value nil
     :documentation "An initial RPC request to execute on application startup. The value should be a JSON object that corresponds to the request body you would send to invoke the RPC method via the HTTP server. For example, to invoke the \"version\" method, pass '{\"type\":\"version\"}'. The result will printed on *STANDARD-OUTPUT*.")
    (("server" #\S)
     :type boolean
     :optional t
     :initial-value nil
     :documentation "Start a QVM server")
    (("verbose")
     :type integer
     :initial-value 1
     :documentation "Desired verbosity level. The following levels are understood.

> 1: verbose output
= 1: normal logging and output (default)
< 1: no logging and only essential output")
    (("version" #\v)
     :type boolean
     :optional t
     :documentation "Display the versions of the app and underlying QVM")))

(defun process-options (&key
                          help
                          host
                          log-level
                          port
                          rpc-request
                          server
                          verbose
                          version)

  (when help
    (show-help)
    (return-from process-options))

  (when version
    (show-version)
    (return-from process-options))

  (when (> verbose 0)
    (setf *logger* (make-logger "qvm-ng" (parse-log-level log-level)))
    (show-welcome))

  (when rpc-request
    (run-initial-rpc-request rpc-request))

  (when server
    (start-server-mode :host host :port port)
    #|not reached|#))

(defun show-help ()
  (format *error-output* "Usage:~%")
  (format *error-output* "    qvm-ng [<options>...]~%~%")
  (format *error-output* "Options:~%")
  (command-line-arguments:show-option-help *option-spec* :stream *error-output* :sort-names t))

(defun show-version ()
  (format *error-output* "~A [~A]~%" +QVM-VERSION+ +GIT-HASH+))

(defun show-welcome ()
  (format *error-output*
          "~&*****************************************~%~
             * Welcome to the Rigetti QVM (Next Gen) *~%~
             *****************************************~%~
             Copyright (c) 2016-2019 Rigetti Computing.~2%~
             (Configured with ~A MiB of workspace and ~D core~:P.)~2%"
          #+sbcl
          (floor (sb-ext:dynamic-space-size) (expt 1024 2))
          #-sbcl
          "many many"
          (max 1 (qvm:count-logical-cores)))
  nil)

(defun run-initial-rpc-request (rpc-request)
  (let ((*request-json* (parse-json-or-lose rpc-request)))
    (alexandria:if-let ((handler (lookup-rpc-handler-for-request *request-json*)))
      (format t "~A~%" (funcall handler))
      (error "No handler found for initial rpc request: ~S" rpc-request))))

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

(defun %entry-point (argv)
  ;; A (somewhat) testable version of ENTRY-POINT that doesn't QUIT-NICELY or trap errors. Note that
  ;; certain flags in ARGV (e.g. "-S") will cause PROCESS-OPTIONS to start a server and never
  ;; return.
  (qvm:prepare-for-parallelization)

  (command-line-arguments:handle-command-line
   *option-spec*
   'process-options
   :command-line argv
   :name "qvm-ng"
   :rest-arity nil))

(defun entry-point (argv)
  "The entry point for running the application from the command line."
  (handler-case
      (progn
        (%entry-point argv)
        (quit-nicely 0))
    #+sbcl
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (format-log "Caught Control-C. Quitting.")
      (quit-nicely 0))
    ;; TODO(appleby): re-enable this before shipping (maybe).
    #+(or)
    (error (c)
      (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
      (format-log :err "Error encountered, quitting.")
      (quit-nicely 1))))

(defun asdf-entry-point ()
  (entry-point (uiop:command-line-arguments)))

