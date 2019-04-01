;;;; src/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

;;;; Entry-point into binary executable.

(defvar swank:*use-dedicated-output-stream*)
(defvar *entered-from-main* nil)
(defvar *program-name* "qvm")

(defparameter *benchmark-types* '("bell" "qft" "hadamard" "qualcs" #-forest-sdk "suite")
  "List of allowed benchmark names.")

(defparameter *available-simulation-methods* '("pure-state" "full-density-matrix")
  "List of available simulation methods.")

(defparameter *available-allocation-kinds* '("native" "foreign")
  "Kinds of allocations that are possible.")

(defparameter *option-spec*
  `((("default-allocation")
     :type string
     :initial-value "native"
     :documentation "select where wavefunctions get allocated: \"native\" (default) for native allocation, \"foreign\" for C-compatible allocation")
    (("execute" #\e)
     :type boolean
     :optional t
     :documentation "read a Quil program from stdin and execute it (DEPRECATED: simply elide this option)")

    (("server" #\S)
     :type boolean
     :optional t
     :documentation "start a QVM server")

    (("port" #\p)
     :type integer
     :optional t
     :documentation "port to start the QVM server on")

    (("qubits" #\q)
     :type integer
     :optional t
     :documentation "Number of qubits to force to use.")

    (("memory-limit")
     :type integer
     :initial-value #.qvm::**classical-memory-size-limit**
     :documentation "limit to the number of octets of usable classical memory per program (this does *not* limit the amount of memory this program consumes)")

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

    (("parallelization-limit")
     :type integer
     :optional t
     :documentation "The number of qubits to start parallelizing at")

    (("benchmark")
     :type integer
     :initial-value 0
     :documentation "run a benchmark entangling N qubits (default: 26)")

    (("benchmark-type")
     :type string
     :initial-value "bell"
     :documentation ,(format nil "the type of benchmark to run; includes {~{~S~^, ~}}" *benchmark-types*))

    (("help" #\h)
     :type boolean
     :optional t
     :documentation "display help")

    (("version" #\v)
     :type boolean
     :optional t
     :documentation "display the versions of the app and underlying QVM")

    (("check-libraries")
     :type boolean
     :optional t
     :documentation "check that foreign libraries are adequate")

    (("verbose")
     :type boolean
     :optional t
     :documentation "display verbose output")

    #-forest-sdk
    (("swank-port")
     :type integer
     :optional t
     :documentation "port to start a Swank server on")

    (("compile" #\c)
     :type boolean
     :optional t
     :documentation "pre-compile Quil programs before execution.")

    (("safe-include-directory")
     :type string
     :optional t
     :documentation "allow INCLUDE only files in this directory")

    (("shared")
     :type string
     :optional t
     :documentation "make the QVM use POSIX shared memory. If an empty string is provided, a name will be generated and printed out at initialization. Otherwise the name provided will be used. Must specify --qubits. Only relevant to --server option.")

    (("simulation-method")
     :type string
     :initial-value "pure-state"
     :documentation ,(format nil "the method of qvm simulation; includes {~{~S~^, ~}}. Note that FULL-DENSITY-MATRIX simulation requiresthat --qubits be specified."
                             *available-simulation-methods*))

    #-forest-sdk
    (("debug")
     :type boolean
     :optional t
     :documentation "debug mode, specifically this causes the QVM to not automatically catch execution errors allowing interactive debugging via SWANK.")

    #+forest-sdk
    (("skip-version-check"
      :type boolean
      :initial-value nil
      :documentation "Do not check for a new QVM version at launch."))

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

(defun check-libraries ()
  "Check that the foreign libraries are adequate. Exits with status
  0 if so, 1 if not."
  #+sbcl
  (format t "Loaded libraries:~%~{  ~A~%~}~%"
          (mapcar 'sb-alien::shared-object-pathname sb-sys:*shared-objects*))
  (unless (magicl.foreign-libraries:foreign-symbol-available-p "zuncsd_"
                                                               'magicl.foreign-libraries:liblapack)
    (format t "The loaded version of LAPACK is missing necessary functionality.~%")
    (uiop:quit 1))
  (format t "Library check passed.~%")
  (uiop:quit 0))

(defun show-welcome ()
  (format t "~&~
******************************
* Welcome to the Rigetti QVM *~%~
******************************~%~
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

(defparameter *host-address* "0.0.0.0")
(defparameter *default-host-port* 5000)

(defun start-server-app (port)
  (check-type port (or null (integer 0 65535))
              "The port must be between 0 and 65535.")
  (when (null port)
    (setf port *default-host-port*))
  (format-log "Starting server on port ~D." port)
  (unless (null *qubit-limit*)
    (format-log "Server is limited to ~D qubit~:P." *qubit-limit*))
  (start-server port)
  ;; TODO? Make this join the thread, instead of spinning in a loop.
  (loop (sleep 1)))

(defun pprint-complex (stream c &optional colonp atp)
  "A formatter for complex numbers that can be used with ~/.../."
  (declare (ignore colonp atp))
  (let ((re (realpart c))
        (im (imagpart c)))
    (cond
      ((zerop im)
       (format stream "~F" re))
      ((zerop re)
       (format stream "~Fi" im))
      ((plusp im)
       (format stream "~F+~Fi" re im))
      (t                                ; equiv: (minusp im)
       (format stream "~F-~Fi" re (- im))))))

(defun resolve-safely (filename)
  (flet ((contains-up (filename)
           (member-if (lambda (obj)
                        (or (eql ':UP obj)
                            (eql ':BACK obj)))
                      (pathname-directory filename))))
    (cond
      ((uiop:absolute-pathname-p filename)
       (error "Not allowed to specify absolute paths to INCLUDE."))

      ((uiop:relative-pathname-p filename)
       ;; Only files allowed.
       (unless (uiop:file-pathname-p filename)
         (error "INCLUDE requires a pathname to a file."))
       (when (contains-up filename)
         (error "INCLUDE can't refer to files above."))
       (if (null *safe-include-directory*)
           filename
           (merge-pathnames filename *safe-include-directory*)))

      (t
       (error "Invalid pathname: ~S" filename)))))

(defun safely-parse-quil-string (string)
  "Safely parse a Quil string STRING."
  (flet ((parse-it (string)
           (let* ((quil::*allow-unresolved-applications* t)
                  (parse-results
                    (quil:parse-quil-string string)))
             parse-results)))
    (if (null *safe-include-directory*)
        (parse-it string)
        (let ((quil:*resolve-include-pathname* #'resolve-safely))
          (parse-it string)))))

(defun safely-read-quil (&optional (stream *standard-input*))
  "Safely read the Quil from the stream STREAM, defaulting to *STANDARD-INPUT*."
  (safely-parse-quil-string (slurp-lines stream)))

(defgeneric make-shared-wavefunction (simulation-method num-qubits name))

(defmethod make-shared-wavefunction ((simulation-method (eql 'pure-state)) num-qubits name)
  (multiple-value-bind (vec finalizer)
      (qvm:allocate-vector (make-instance 'qvm:posix-shared-memory-allocation
                                          :name name
                                          :length (expt 2 num-qubits)))
    (setf (aref vec 0) (cflonum 1))
    (values vec finalizer)))

(defmethod make-shared-wavefunction ((simulation-method (eql 'full-density-matrix)) num-qubits name)
    (multiple-value-bind (vec finalizer)
        (qvm:allocate-vector (make-instance 'qvm:posix-shared-memory-allocation
                                            :name name
                                            :length (expt 2 (* 2 num-qubits))))
      ;; It just so happens that the pure zero state in the
      ;; density matrix formalism is the same as the pure zero state
      ;; in the state-vector formalism)
      (setf (aref vec 0) (cflonum 1))
      (values vec finalizer)))

(defun quit-nicely (&optional (code 0))
  #+sbcl
  (sb-ext:exit :code code :abort nil)
  #-sbcl
  (uiop:quit code t))

(defun print-classical-memory (qvm)
  "Print all of the QVM's classical memory to *STANDARD-OUTPUT*."
  (let ((memories (qvm::classical-memories qvm)))
    (format t "Classical memory (low -> high indexes):")
    (cond
      ((zerop (hash-table-count memories))
       (format t "~&    No memory."))
      (t
       (maphash (lambda (name mv)
                  (format t "~&    ~A:" name)
                  (loop :for i :below (qvm::memory-view-length mv)
                        :do (format t " ~A" (qvm::memory-view-ref mv i))))
                memories)))
    (terpri)))

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
                          execute
                          help
                          memory-limit
                          server
                          port
                          #-forest-sdk swank-port
                          num-workers
                          time-limit
                          qubit-limit
                          parallelization-limit
                          safe-include-directory
                          qubits
                          benchmark
                          benchmark-type
                          compile
                          shared
                          simulation-method
                          #-forest-sdk debug
                          #+forest-sdk skip-version-check
                          quiet
                          log-level)

  (setf *logger* (make-instance 'cl-syslog:rfc5424-logger
                                :app-name "qvm"
                                :facility ':local0
                                :maximum-priority (log-level-string-to-symbol log-level)
                                :log-writer
                                #+windows
                                (cl-syslog:stream-log-writer)
                                #-windows
                                (cl-syslog:tee-to-stream
                                 (cl-syslog:syslog-log-writer "qvm" :local0)
                                 *error-output*)))
  (when help
    (show-help)
    (quit-nicely))

  (when version
    (show-version)
    (quit-nicely))

  (when check-libraries
    (check-libraries))

  #+forest-sdk
  (unless skip-version-check
    (multiple-value-bind (available-p version)
        (ignore-errors (sdk-update-available-p))
      (when available-p
        (format t "An update is available to the SDK. You have version ~A. ~
Version ~A is available from downloads.rigetti.com/qcs-sdk/forest-sdk.dmg~%"
                +QVM-VERSION+ version))))

  (when verbose
    (setf qvm:*transition-verbose* t))

  (when default-allocation
    (setq **default-allocation** (allocation-description-maker default-allocation)))

  (when (plusp time-limit)
    (setf *time-limit* (/ time-limit 1000.0d0)))

  (when (plusp qubit-limit)
    (setf *qubit-limit* qubit-limit))

  (unless (null parallelization-limit)
    (cond
      ((typep parallelization-limit 'qvm:parallelization-limit)
       (setf qvm:*qubits-required-for-parallelization* parallelization-limit))
      (t
       (error "Invalid parallelization limit: ~D." parallelization-limit))))

  (when (and (integerp memory-limit)
             (not (minusp memory-limit)))
    (setf qvm::**classical-memory-size-limit** memory-limit))

  #-forest-sdk
  (when debug
    (setf *debug* t))

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

  (when compile
    (setf qvm:*compile-before-running* t))

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

  (unless (member simulation-method *available-simulation-methods* :test #'string-equal)
    (error "Invalid simulation method: ~S" simulation-method))


  ;; Determine the simulation method, and set *SIMULATION-METHOD* appropriately
  (setf *simulation-method* (intern (string-upcase simulation-method) :qvm-app))
  (when (and (eq *simulation-method* 'full-density-matrix)
             (null qubits))
    (format-log "Full density matrix simulation requires --qubits to be specified.")
    (quit-nicely 1))

  (format-log "Selected simulation method: ~A" simulation-method)

  ;; Deprecation of -e/--execute
  (when execute
    (format-log "Warning: --execute/-e is deprecated. Elide this option ~
                 for equivalent behavior."))

  (cond
    ;; Benchmark mode.
    ((or (eq T benchmark)
         (plusp benchmark))
     (when shared
       (format-log "Warning: Ignoring --shared option in benchmark mode."))
     ;; Default number of qubits
     (when (eq T benchmark)
       (setf benchmark 26))
     (unless (member benchmark-type *benchmark-types* :test #'string-equal)
       (error "Invalid benchmark type: ~S" benchmark-type))
     (perform-benchmark benchmark-type benchmark))

    ;; Server mode.
    ((or server port)
     (when execute
       (format-log "Warning: Ignoring execute option: ~S" execute)
       (setf execute nil))

     ;; Handle persistency and shared memory.
     (when shared
       (unless qubits
         (format-log "The --qubits option must be specified for --shared.")
         (quit-nicely 1))
       (let ((shm-name (if (zerop (length shared))
                           (format nil "QVM~D" (get-universal-time))
                           shared)))
         (setf *shared-memory-object-name* shm-name)
         (multiple-value-setq (**persistent-wavefunction**
                               **persistent-wavefunction-finalizer**)
           (make-shared-wavefunction *simulation-method* qubits shm-name))
         (format-log "ATTENTION! Created POSIX shared memory with name: ~A" shm-name)
         (start-shm-info-server shm-name (length **persistent-wavefunction**)))

       (format-log "Created persistent memory for ~D qubits" qubits))
     ;; Start the server
     (start-server-app port))

    ;; Batch mode.
    (t
     (when shared
       (format-log "Warning: Ignoring --shared option in execute mode."))
     (when (eq *simulation-method* 'full-density-matrix)
       (format-log "Full density matrix simulation not yet supported in batch mode.")
       (quit-nicely 1))
     (let (qvm program alloc-time exec-time qubits-needed)
       ;; Read the Quil.
       (cond
         ((interactive-stream-p *standard-input*)
          (format-log "Reading program from interactive terminal. Press Control-D ~
                       when finished."))
         (t
          (format-log "Reading program.")))
       (setf program (safely-read-quil))

       ;; Figure out how many qubits we need.
       (let ((really-needed (cl-quil:qubits-needed program)))
         (when qubits
           (assert (>= qubits really-needed) ()
                   "Computation restricted to ~D qubit~:P but ~D are needed."
                   qubits
                   really-needed))
         (setf qubits-needed (or qubits really-needed)))

       ;; Allocate the QVM.
       (format-log "Allocating memory for QVM of ~D qubits." qubits-needed)
       (with-timing (alloc-time)
         (setf qvm (make-qvm qubits-needed
                             :allocation (funcall **default-allocation**
                                                  (expt 2 qubits-needed)))))
       (format-log "Allocation completed in ~D ms." alloc-time)

       ;; Load our program.
       (format-log "Loading quantum program.")
       (load-program qvm program :supersede-memory-subsystem t)

       ;; Execute it.
       (format-log "Executing quantum program.")
       ;; Fresh random state.
       (qvm:with-random-state ((qvm:seeded-random-state nil))
         (with-timing (exec-time)
           (run qvm)))
       (format-log "Execution completed in ~D ms." exec-time)

       ;; Print our answer to stdout.
       (format-log "Printing classical memory and ~D-qubit state." qubits-needed)
       (print-classical-memory qvm)
       (format t "~&Amplitudes:")
       (let ((nq (qvm:number-of-qubits qvm)))
         (qvm:map-amplitudes
          qvm
          (let ((i 0))
            (lambda (z)
              (format t
                      "~%    |~v,'0B>: ~/QVM-APP::PPRINT-COMPLEX/, ~64TP=~5F%"
                      nq
                      i
                      z
                      (* 100 (qvm:probability z)))
              (incf i)))))
       (terpri))))

  (quit-nicely))

(defun command-line-debugger (condition previous-hook)
  (declare (ignore previous-hook))
  (format *error-output* "~&Fatal ~A: ~%  ~A~%"
          (type-of condition)
          condition)
  (force-output *error-output*)
  (uiop:quit 1))

(defun setup-debugger ()
  #+forest-sdk
  (setf *debugger-hook* 'command-line-debugger)
  #-forest-sdk
  (disable-debugger))

(defun %main (argv)
  (setup-debugger)
  (setf *entered-from-main* t)

  ;; This finalizer can _always_ be called even if there is no
  ;; persistent wavefunction. Also, we note that the library
  ;; implementing shared memory also takes care of this. We just want
  ;; to be extra sure.
  (qvm::call-at-exit **persistent-wavefunction-finalizer**)

  ;; Save the program name away.
  (setf *program-name* (pop argv))

  (qvm::initialize-random-state)

  ;; Run the program.
  (handler-case
      (handler-bind ((style-warning #'muffle-warning))
        (command-line-arguments:handle-command-line
         *option-spec*
         'process-options
         :command-line argv
         :name "qvm"
         :rest-arity nil))
    ;; Handle Ctrl-C
    #+sbcl
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (format-log "Caught Control-C. Quitting.")
      (quit-nicely))
    ;; General errors.
    (error (c)
      (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
      (format-log "Error encountered, Qutting.")
      (quit-nicely 1))))

(defun asdf-entry-point ()
  (%main (list* "qvm-app" (uiop:command-line-arguments))))

(defun start-server (port)
  #+forest-sdk
  (setq tbnl:*log-lisp-backtraces-p* nil
        tbnl:*log-lisp-errors-p* nil)
  (tbnl:reset-session-secret)

  (setq tbnl:*show-lisp-errors-p* *debug*
        tbnl:*show-lisp-backtraces-p* *debug*
        tbnl:*catch-errors-p* (not *debug*))
  (setq *app* (make-instance
               'vhost
               :address *host-address*
               :port port
               :taskmaster (make-instance 'tbnl:one-thread-per-connection-taskmaster)))
  (when (null (dispatch-table *app*))
    (push
     (create-prefix/method-dispatcher "/" ':POST 'handle-post-request)
     (dispatch-table *app*)))
  (tbnl:start *app*))

(defun stop-server ()
  (tbnl:stop *app*))
