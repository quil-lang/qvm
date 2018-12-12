;;;; src/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

;;;; Entry-point into binary executable.

(defvar *entered-from-main* nil)
(defvar *program-name* "qvm")

(defparameter *benchmark-types* '("bell" "qft" "hadamard" #-forest-sdk "suite")
  "List of allowed benchmark names.")

(defparameter *available-simulation-methods* '("pure-state" "full-density-matrix")
  "List of available simulation methods.")

(defparameter *option-spec*
  `((("execute" #\e)
     :type boolean
     :optional t
     :documentation "read a Quil program from stdin and execute it")

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
     :documentation "debug mode, specifically this causes the QVM to not automatically catch execution errors allowing interactive debugging via SWANK.")))

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
Copyright (c) 2018 Rigetti Computing.~2%")
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

(defun format-complex (c)
  (cond
    ((zerop (imagpart c))
     (format nil "~F" (realpart c)))
    ((plusp (imagpart c))
     (format nil "~F+~Fi" (realpart c) (imagpart c)))
    ((minusp (imagpart c))
     (format nil "~F-~Fi" (realpart c) (abs (imagpart c))))))

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
      (qvm::make-shared-array name (expt 2 num-qubits) 'qvm:cflonum)
    (setf (aref vec 0) (cflonum 1))
    (values vec finalizer)))

(defmethod make-shared-wavefunction ((simulation-method (eql 'full-density-matrix)) num-qubits name)
    (multiple-value-bind (vec finalizer)
        (qvm::make-shared-array name (expt 2 (* 2 num-qubits)) 'qvm:cflonum)
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
  (let ((memories (qvm::classical-memories qvm)))
    (cond
      ((zerop (hash-table-count memories))
       (format-log "No classical memory present."))
      (t
       (format-log "Classical memory (low -> high indexes):")
       (maphash (lambda (name mv)
                  (let ((data (with-output-to-string (s)
                                (loop :for i :below (qvm::memory-view-length mv)
                                      :do (format s " ~A" (qvm::memory-view-ref mv i))))))
                    (format-log "    ~A: ~A~%" name data)))
                memories)))))

(defun process-options (&key version
                             check-libraries
                             verbose
                             execute
                             help
                             memory-limit
                             server
                             port
                             #-forest-sdk swank-port
                             num-workers
                             time-limit
                             qubit-limit
                             safe-include-directory
                             qubits
                             benchmark
                             benchmark-type
                             compile
                             shared
                             simulation-method
                             #-forest-sdk debug)
  (when help
    (show-help)
    (quit-nicely))

  (when version
    (show-version)
    (quit-nicely))

  (when check-libraries
    (check-libraries))

  (when verbose
    (setf qvm:*transition-verbose* t))

  (when (plusp time-limit)
    (setf *time-limit* (/ time-limit 1000.0d0)))

  (when (plusp qubit-limit)
    (setf *qubit-limit* qubit-limit))
  
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
  (show-welcome)

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
       (format-log "Warning: Ignoring execute option: ~S" execute))

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
    (execute
     (when shared
       (format-log "Warning: Ignoring --shared option in execute mode."))
     (when (eq *simulation-method* 'full-density-matrix)
       (format-log "Full density matrix simulation not yet supported in batch mode.")
       (quit-nicely 1))
     (let (qvm program alloc-time exec-time qubits-needed)
       (format-log "Reading program.")
       (setf program (safely-read-quil))
       (setf qubits-needed (or qubits (cl-quil:qubits-needed program)))

       (format-log "Allocating memory for QVM of ~D qubits." qubits-needed)
       (with-timing (alloc-time)
         (setf qvm (make-qvm qubits-needed)))
       (format-log "Allocation completed in ~D ms." alloc-time)

       (format-log "Loading quantum program.")
       (load-program qvm program :supersede-memory-subsystem t)

       (format-log "Executing quantum program.")
       ;; Fresh random state.
       (qvm:with-random-state ((qvm:seeded-random-state nil))
         (with-timing (exec-time)
           (run qvm)))
       (format-log "Execution completed in ~D ms." exec-time)
       (when (or verbose (<= qubits-needed 10))
         (format-log "Printing ~D-qubit state." qubits-needed)
         (format-log "Amplitudes:")
         (qvm:map-amplitudes
          qvm
          (let ((i 0))
            (lambda (z)
              (format-log "  |~v,'0B>: ~A, P=~5F%"
                          (qvm:number-of-qubits qvm)
                          i
                          (format-complex z)
                          (* 100 (qvm:probability z)))
              (incf i)))))
       (print-classical-memory qvm)))
    (t
     (format-log "You must benchmark, start a server, or execute a Quil file.")
     (quit-nicely)))
  
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
        (cond
          ((null argv)
           (show-help)
           (quit-nicely))
          (t
           (command-line-arguments:handle-command-line
            *option-spec*
            'process-options
            :command-line argv
            :name "qvm"
            :rest-arity nil))))
    ;; Handle Ctrl-C
    #+sbcl
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (quit-nicely))
    ;; General errors.
    (error (c)
      (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
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
