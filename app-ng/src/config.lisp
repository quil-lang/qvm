;;;; There are two sources of configuration. Config variables are
;;;; defined in order of
;;;;
;;;;   1. Default values specified by :INITIAL-VALUE in *OPTION-SPEC*
;;;;   2. A configuration file; e.g. ~/.qvm_config
;;;;   3. Command-line options; e.g. --memory-limit
;;;;
;;;; where variables configured in "higher" layers may override those from lower layers.
(in-package #:qvm-app-ng.config)

;;; OPTION-SPEC
;;;
;;; The variable *OPTION-SPEC* defines permissible options.

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

    (("server" #\S)
     :type boolean
     :optional t
     :documentation "start a QVM server")

    (("host")
     :type string
     :optional t
     :initial-value "0.0.0.0"
     :documentation "host on which to start the QVM server")

    (("port" #\p)
     :type integer
     :optional t
     :initial-value 5000
     :validator check-port
     :documentation "port to start the QVM server on")

    (("qubits" #\q)
     :type integer
     :optional t
     :validator check-non-negative
     :documentation "Number of qubits to force to use.")

    (("num-workers" #\w)
     :type integer
     :initial-value 0
     :validator check-non-negative
     :documentation "workers to use in parallel (0 => maximum number)")

    #-forest-sdk
    (("swank-port")
     :type integer
     :optional t
     :validator check-port
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

    (("check-libraries")
     :type boolean
     :optional t
     :documentation "check that foreign libraries are adequate")

    (("proxy")
     :type string
     :optional t
     :documentation "Proxy to use when checking for an SDK update.")

    (("quiet")
     :type boolean
     :optional t
     :documentation "Disable all non-essential printed output to stdout (banner, etc.).")

    (("log-level")
     :type string
     :optional t
     :initial-value "debug"
     :validator check-log-level
     :documentation "maximum logging level (\"debug\", \"info\", \"notice\", \"warning\", \"err\", \"crit\", \"alert\", or \"emerg\").")

    (("qubit-limit")
     :type integer
     :optional t
     :validator check-non-negative
     :documentation "Maximum number of qubits that a QVM can use (in both batch and server modes).")

    (("memory-limit")
     :type integer
     :optional t
     :initial-value #.qvm::**classical-memory-size-limit**
     :validator check-non-negative
     :documentation "Limit the number of octets of usable classical memory per program (this does *not* limit the amount of memory this program consumes).")

    (("safe-include-directory")
     :type string
     :optional t
     :validator check-directory
     :documentation "allow INCLUDE only files in this directory")

    (("allocation-method")
     :type string
     :optional t
     :initial-value "native"
     :validator check-allocation-method
     :documentation "select default wavefunction allocation method: \"native\" (default) for native allocation, \"foreign\" for C-compatible allocation.")

    (("simulation-method")
     :type string
     :optional t
     :initial-value "pure-state"
     :validator check-simulation-method
     :documentation "select the default simulation method: \"pure-state\", \"full-density-matrix\".")))

(defun canonicalize-option-name (name-designator)
  "Convert NAME-DESIGNATOR to a canonical option name, i.e. a KEYWORD."
  (etypecase name-designator
    (keyword name-designator)
    (symbol (alexandria:make-keyword (symbol-name name-designator)))
    ((or string character) (alexandria:make-keyword (string-upcase name-designator)))))

(defun spec-option-name (spec)
  (canonicalize-option-name
   (etypecase (car spec)
     (string (car spec))
     (cons (caar spec)))))

(defun spec-options-plist (spec)
  (rest spec))

(defun lookup-spec-by-option-name (option-name)
  (find (canonicalize-option-name option-name)
        *option-spec*
        :key #'spec-option-name))

(defun lookup-option-option (option-name key)
  (alexandria:if-let ((spec (lookup-spec-by-option-name option-name)))
    (getf (spec-options-plist spec) key)))

(defun lookup-short-name-for-option (option-name)
  (alexandria:if-let ((spec (lookup-spec-by-option-name option-name)))
    (when (alexandria:proper-list-p (first spec))
      (cdar spec))))

(defun lookup-type-for-option (option-name)
  (lookup-option-option option-name ':type))

(defun lookup-validator-for-option (option-name)
  (lookup-option-option option-name ':validator))

(defun lookup-default-for-option (option-name)
  (lookup-option-option option-name ':initial-value))

(defun option-spec-options-as-keywords (specs)
  (mapcar #'spec-option-name specs))


;;; General Utilities

(defun make-plist (keys values)
  (assert (every #'symbolp keys))
  (assert (= (length keys) (length values)))
  (mapcan #'list keys values))

(defun plist-keys (plist)
  (loop :for key :in plist :by #'cddr :collect key))

(defun plist-values (plist)
  (loop :for value :in (rest plist) :by #'cddr :collect value))

(defun plist-p (plist)
  (and (evenp (length plist))
       ;; SYMBOLP might be a tighter restriction than technically required for a plist, but all sane
       ;; plists use symbols as keys/indicators.
       (every #'symbolp (plist-keys plist))))


;;; Validators

(define-condition config-parse-error (alexandria:simple-parse-error)
  ((name :initarg :name :type symbol :reader config-parse-error-name)
   (input :initarg :input :type string :reader config-parse-error-input))
  (:report
   (lambda (condition stream)
     (format stream "Error while parsing config option ~A" (config-parse-error-name condition))
     (format stream "~&Got invalid input: ~A" (config-parse-error-input condition))
     (alexandria:when-let ((format-control (simple-condition-format-control condition)))
       (apply #'format stream
              (concatenate 'string "~&Expecting: " format-control)
              (simple-condition-format-arguments condition))))))

(defun config-parse-error (option-name input &optional format-control &rest format-arguments)
  (error 'config-parse-error
         :name option-name
         :input input
         :format-control format-control
         :format-arguments format-arguments))

(defun check-port (option-name input)
  (unless (< 0 input 65536)
    (config-parse-error option-name input "a valid port between 1 and 65535")))

(defun check-non-negative (option-name input)
  (when (minusp input)
    (config-parse-error option-name input "a non-negative number")))

(defun check-log-level (option-name input)
  (unless (qvm-app-ng::valid-log-level input)
    (config-parse-error option-name input "a valid log level")))

(defun check-directory (option-name input)
  (unless (uiop:directory-exists-p input)
    (config-parse-error option-name input "a path to an existing directory")))

(defun check-allocation-method (option-name input)
  (unless (member input qvm-app-ng::*available-allocation-kinds* :test #'string-equal)
    (config-parse-error option-name input "a valid allocation method")))

(defun check-simulation-method (option-name input)
  (unless (member input qvm-app-ng::*available-simulation-methods* :test #'string-equal)
    (config-parse-error option-name input "a valid simulation method")))

(defun validate-options (&rest options-plist)
  "Validate config options."
  (alexandria:doplist (option-name input options-plist options-plist)
    ;; Type checking is really only required for config-file options.
    (alexandria:if-let ((type (lookup-type-for-option option-name)))
      (unless (typep input type)
        (config-parse-error option-name input "an input of type ~A" type)))
    (alexandria:if-let ((validator (lookup-validator-for-option option-name)))
      (funcall validator option-name input))))


;;; Config

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-empty-config ()
    '()))

(global-vars:define-global-var **config** (make-empty-config)
  "A hash-table holding global configuration values. See *OPTION-SPEC* for a list of those options.")

(defun reset-config ()
  (setf **config** (make-empty-config)))

(defun call-with-config (function)
  (apply (alexandria:ensure-function function) **config**))

(defun strip-spec-options (specs &rest keys)
  (mapcar (lambda (spec)
            (destructuring-bind (name . options-plist) spec
              (cons name (uiop:remove-plist-keys keys options-plist))))
          specs))

(defun config-defaults ()
  (command-line-arguments:process-command-line-options
   (strip-spec-options *option-spec* ':validator)
   '()))

(defun parse-command-line-options (argv)
  (command-line-arguments:handle-command-line
   ;; Strip :VALIDATOR because it's non-standard and COMMAND-LINE-ARGUMENTS will choke on it. Strip
   ;; :INITIAL-VALUE because we already processed default values by this point. We want priority
   ;; order defaults < config-file < command-line, but if we include defaults here, we'd get
   ;; something like defaults < config-file < defaults < command-line.
   (strip-spec-options *option-spec* ':initial-value ':validator)
   #'validate-options
   :command-line argv
   :name qvm-app-ng::*program-name*
   :rest-arity nil))

(defun sanity-check-options-plist (options-plist)
  (unless (plist-p options-plist)
    (error "OPTIONS-PLIST must have an even number of key/value pairs: ~A" options-plist))

  (alexandria:when-let (diff (set-difference (plist-keys options-plist)
                                             (option-spec-options-as-keywords *option-spec*)))
    (error "The following config options are invalid: ~A" diff))
  options-plist)

(defun canonicalize-plist-keys (plist)
  (make-plist
   (mapcar #'canonicalize-option-name (plist-keys plist))
   (plist-values plist)))

(defun read-config-safely (input)
  (uiop:with-input (input)
    (uiop:with-safe-io-syntax ()
      ;; UIOP:SLURP-STREAM-FORMS with :COUNT 1 here rather than UIOP:SLURP-STREAM-FORM in order to
      ;; handle an empty config file.
      (first (uiop:slurp-stream-forms input :count 1)))))

(defun parse-config-file (file-or-input)
  "Load config items specified in SPEC from FILE, storing them in the hash-table TABLE. Returns TABLE."
  (apply #'validate-options
         (sanity-check-options-plist
          (canonicalize-plist-keys
           (read-config-safely file-or-input)))))


;;; Exported symbols

(defun get-config (option-name)
  "Return the config value for OPTION-NAME.

OPTION-NAME is a string-designator for desired option."
  (getf **config** (canonicalize-option-name option-name)))

(defun load-config (argv config-file &optional (option-spec *option-spec*))
  "Load and merge config from the command line ARGV and CONFIG-FILE as specified in OPTION-SPEC.

ARGV is a list of strings representing the command line options and arguments.
CONFIG-FILE is an \"input designator\" in any a subset of the formats accepted by UIOP:WITH-INPUT, namely:
    If NIL, it is ignored (note this differs from UIOP:WITH-INPUT where NIL means *STANDARD-INPUT*)
    If a STREAM, use it as the stream.
    If a STRING, use it as a string-input-stream.
    If a PATHNAME, open it via WITH-INPUT-FILE and default options
OPTION-SPEC is an option specification in the same format as that accepted by COMMAND-LINE-ARGS.

If an error occurs during processing, LOAD-CONFIG leaves the config in an empty state."
  (check-type argv alexandria:proper-list)
  (assert (every #'stringp argv))
  (check-type config-file (or null string stream pathname))
  (reset-config)
  ;; Ensure config file and command line can be parsed before loading defaults. This function either
  ;; succeeds, or throws an error and leaves **config** empty.
  (let* ((*option-spec* option-spec)
         (config-file-options (and (not (null config-file)) (parse-config-file config-file)))
         (command-line-options (and (not (null argv)) (parse-command-line-options argv))))
    (setf **config** (append command-line-options
                             config-file-options
                             (config-defaults))))
  (values))

(defun handle-config (argv config-file callback)
  "APPLY CALLBACK to the config loaded by calling (LOAD-CONFIG ARGV CONFIG-FILE).

CALLBACK is a function-designator that can be called like (APPLY CALLBACK CONFIG-AS-PLIST). For example, CALLBACK might define a keyword argument for each config key it wants to handle (specifying &ALLOW-OTHER-KEYS if not all config options are explicitly listed.)

This function is intended to present an interface similar to COMMAND-LINE-ARGUMENTS:HANDLE-COMMAND-LINE."
  (check-type callback (or symbol function))
  (load-config argv config-file)
  (call-with-config callback))

(defun show-option-help ()
  (command-line-arguments:show-option-help *option-spec* :sort-names t))
