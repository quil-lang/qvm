(in-package #:qvm-app-ng)

(global-vars:define-global-var *config*
    (make-hash-table :test #'equalp)
  "A hash-table holding global configuration values. See *OPTION-SPEC* for a list of those options.")

;; There are two sources of configuration. Config variables are
;; defined in order of
;;
;;   1. A configuration file; e.g. ~/.qvm_config
;;   2. Command-line options; e.g. --memory-limit
;;
;; where variables configured in 2 may override those set in 1.

;; The variable *OPTION-SPEC* below defines permissible options. The
;; config file is a plist whose keys are the long names in
;; *OPTION-SPEC*.

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
     :documentation "maximum logging level (\"debug\", \"info\", \"notice\", \"warning\", \"err\", \"crit\", \"alert\", or \"emerg\").")

    (("qubits" "q")
     :type integer
     :optional t
     :documentation "Maximum number of qubits that a QVM can use (in both batch and server modes).")

    (("memory-limit")
     :type integer
     :optional t
     :initial-value #.qvm::**classical-memory-size-limit**
     :documentation "Limit the number of octets of usable classical memory per program (this does *not* limit the amount of memory this program consumes).")

    (("simulation-method"
      :type string
      :optional t
      :initial-value "pure-state"
      :documentation ""))

    (("allocation-method"
      :type string
      :optional t
      :initial-value "native"
      :documentation ""))))

(defun options-spec-options (spec)
  "Pull out the option names from SPEC."
  (mapcar #'caar spec))

(defun reset-config ()
  (setf *config* (make-hash-table :test #'equalp)))

(defun load-options-spec (&optional (specs *option-spec*) (table *config*))
  "Load configurables and their defaults from SPECS into TABLE."
  (reset-config)
  (dolist (spec specs)
    ;; Reappropriated from #:command-line-arguments.
    (destructuring-bind ((name . short-names) &rest option-options
                         &key (initial-value nil initial-value-p)
                         &allow-other-keys)
        spec
      (declare (ignore short-names option-options))
      (when initial-value-p
        (setf (gethash name table) initial-value)))))

(defun load-config-file (&key (file #P"~/.qvm_config") (spec *option-spec*) (table *config*) reset)
  "Load config items specified in SPEC from FILE, storing them in the hash-table TABLE. Returns TABLE."
  (when reset
    (reset-config)
    (load-options-spec))
  
  (when (probe-file file)
    (let* ((forms (uiop:read-file-forms file))
           (options (options-spec-options spec)))
      (loop :for option :in options
            :for (option-name value) := (assoc option forms :test #'equalp)
            :when option :do
              (setf (gethash option *config*) value))))
  table)
