;;;; qvm-app/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

;;;; Entry-point into binary executable.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun system-version (system-designator)
    (let ((sys (asdf:find-system system-designator nil)))
      (if (and sys (slot-boundp sys 'asdf:version))
          (asdf:component-version sys)
          "unknown")))

  (defun git-hash (system)
    "Get the short git hash of the system SYSTEM."
    (let ((sys-path (namestring (asdf:system-source-directory system))))
      (multiple-value-bind (output err-output status)
          (uiop:run-program `("git" "-C" ,sys-path "rev-parse" "--short" "HEAD")
                            :output '(:string :stripped t)
                            :ignore-error-status t)
        (declare (ignore err-output))
        (if (not (zerop status))
            "unknown"
            output)))))

(eval-when (:compile-toplevel :load-toplevel)
  (alexandria:define-constant +APP-VERSION+
      (system-version '#:qvm-app)
    :test #'string=
    :documentation "The version of the QVM application.")

  (alexandria:define-constant +QVM-VERSION+
      (system-version '#:qvm)
    :test #'string=
    :documentation "The version of the QVM itself.")

  (alexandria:define-constant +GIT-HASH+
      (git-hash '#:qvm)
    :test #'string=
    :documentation "The git hash of the QVM repo.")
  )

(defvar *entered-from-main* nil)

(defun image-p ()
  *entered-from-main*)

(defun image-directory-pathname ()
  (if (image-p)
      (cl-fad:pathname-directory-pathname
       sb-ext:*core-pathname*)
      nil))

(defvar *program-name* "qvm")
(defvar *num-workers* nil)
(defvar *time-limit* nil)
(defvar *qubit-limit* nil)              ; Maximum no. of qubits.
(defvar *safe-include-directory* nil)

(defmacro with-timeout (&body body)
  (let ((f (gensym "TIME-LIMITED-BODY-")))
    `(flet ((,f () ,@body))
       (declare (dynamic-extent (function ,f)))
       (if (null *time-limit*)
           (,f)
           (sb-ext:with-timeout *time-limit*
             (,f))))))

(defparameter *benchmark-types* '("bell" "qft" "hadamard")
  "List of allowed benchmark names.")

(defparameter *option-spec*
  `((("execute" #\e)
     :type string
     :optional t
     :documentation "execute a Quil file")

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

    (("memory" #\m)
     :type integer
     :initial-value 64
     :documentation "classical memory size in bits")

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
     :documentation ,(format nil "the type of benchmark to run; includes {~{~A~^, ~}}" *benchmark-types*))

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

    (("swank-port")
     :type integer
     :optional t
     :documentation "port to start a Swank server on")

    (("db-host")
     :type string
     :optional t
     :documentation "hostname of Redis DB")

    (("db-port")
     :type integer
     :optional t
     :documentation "port of Redis DB")

    (("safe-include-directory")
     :type string
     :optional t
     :documentation "allow INCLUDE only files in this directory")))

(defun session-info ()
  (if (or (not (boundp 'tbnl:*session*))
          (null tbnl:*session*))
      ""
      (format nil
              "[~A Session:~D] "
              (tbnl:session-remote-addr tbnl:*session*)
              (tbnl:session-id tbnl:*session*))))

(defun format-log (fmt-string &rest args)
  (cond
    ((boundp 'tbnl:*acceptor*)
     (apply #'tbnl:log-message* ':INFO
            (concatenate 'string (session-info) fmt-string)
            args))
    (t
     (format t "[~A] ~?" (tbnl::iso-time) fmt-string args)
     (terpri))))

(defun show-help ()
  (format t "Usage:~%")
  (format t "    ~A [<options>...]~%~%" *program-name*)
  (command-line-arguments:show-option-help *option-spec* :sort-names t))

(defun show-version ()
  (format t "~A (qvm: ~A) [~A]~%" +APP-VERSION+ +QVM-VERSION+ +GIT-HASH+))

(defun show-welcome ()
  (format t "~&~
******************************
* Welcome to the Rigetti QVM *~%~
******************************~%")
  (format t "(Configured with ~D MiB of workspace and ~D worker~:P.)~2%"
          (floor (sb-ext:dynamic-space-size) (expt 1024 2))
          (or *num-workers* (max 1 (qvm:count-logical-cores))))
  nil)

(defmacro with-timing ((var) &body body)
  (let ((start (gensym "START-")))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,var (round (* 1000 (- (get-internal-real-time) ,start))
                           internal-time-units-per-second))))))

(defun slurp-stream (stream)
  (with-output-to-string (s)
    (loop :for byte := (read-byte stream nil nil) :then (read-byte stream nil nil)
          :until (null byte)
          :do (write-char (code-char byte) s))))

(defun keywordify (str)
  (intern (string-upcase str) :keyword))

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

(defun safely-read-quil (filename)
  "Safely read the Quil file designated by FILENAME."
  (flet ((read-it (file)
           (let ((quil::*allow-unresolved-applications* t))
             (quil:read-quil-file file))))
    (if (null *safe-include-directory*)
        (read-it filename)
        (let ((quil:*resolve-include-pathname* #'resolve-safely))
          (read-it filename)))))

(defun safely-parse-quil-string (string)
  "Safely parse a Quil string STRING."
  (flet ((parse-it (string)
           (let ((quil::*allow-unresolved-applications* t))
             (quil:parse-quil-string string))))
    (if (null *safe-include-directory*)
        (parse-it string)
        (let ((quil:*resolve-include-pathname* #'resolve-safely))
          (parse-it string)))))

(defun throw-error-if-over-allocated (num-qubits)
    "Throws an error if the number of qubits requested exceeds the max (defined from command line parameter --qubit-limit)."
  (when (and (integerp *qubit-limit*) (> num-qubits *qubit-limit*))
    (error "~D qubits were requested, but the QVM ~
            is limited to ~D qubits." num-qubits *qubit-limit*)))

(defun process-options (&key version verbose execute help memory server port swank-port db-host db-port num-workers time-limit qubit-limit safe-include-directory qubits benchmark benchmark-type)
  (when help
    (show-help)
    (uiop:quit))

  (when version
    (show-version)
    (uiop:quit))

  (when verbose
    (setf qvm:*transition-verbose* t))

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

  (when (and db-host db-port)
    (check-type db-host string)
    (check-type db-port (integer 0 65535))
    (setf *qvm-db-host* db-host
          *qvm-db-port* db-port)
    (handler-case (ping-redis)
      (error (c)
        (warn "Could not connect to Redis at ~S on port ~D. Error was:~2%    ~A"
              *qvm-db-host*
              *qvm-db-port*
              c)
        (setf *qvm-db-host* nil
              *qvm-db-port* nil))))

  ;; Show the welcome message.
  (show-welcome)

  ;; Start Swank if we were asked. Re-enable the debugger.

  (when swank-port
   (sb-ext:enable-debugger)
   (format-log "Starting Swank on port ~D" swank-port)
   (setf swank:*use-dedicated-output-stream* nil)
   (swank:create-server :port swank-port
                        :dont-close t))

  ;; Warm the matrix apply cache.
  ;;
  ;; NOTE: This is already done at compile-time in src/apply-gate.lisp
  #+#:ignore
  (progn
    (format-log "Warming the operator cache. This may take a few seconds...")
    (qvm:warm-matrix-apply-cache :max-qubits 30))

  (cond
    ;; Benchmark mode.
    ((or (eq T benchmark)
         (plusp benchmark))
     ;; Default number of qubits
     (when (eq T benchmark)
       (setf benchmark 26))
     (unless (member benchmark-type *benchmark-types* :test #'string-equal)
       (error "Invalid benchmark type: ~S" benchmark-type))
     (perform-benchmark benchmark-type benchmark))

    ;; Server mode.
    ((or server port)
     (when execute
       (format-log "Ignoring execute option: ~S" execute))
     (start-server-app port))

    ;; Batch mode.
    (execute
     (let (qvm program alloc-time exec-time qubits-needed)
       (format-log "Reading program.")
       (setf program (safely-read-quil execute))
       (setf qubits-needed (or qubits (cl-quil:qubits-needed program)))

       (format-log "Allocating memory for QVM of ~D qubits." qubits-needed)
       (with-timing (alloc-time)
         (setf qvm (make-qvm qubits-needed :classical-memory-size memory)))
       (format-log "Allocation completed in ~D ms." alloc-time)

       (format-log "Loading quantum program.")
       (load-program qvm program)

       (format-log "Executing quantum program.")
       (setf *random-state* (make-random-state t)) ; Seed random.
       (with-timing (exec-time)
         (run qvm))
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
       (format-log "Classical memory (LSB -> MSB): ~A"
                   (reverse
                    (format nil "~v,'0B"
                            (qvm::classical-memory-size qvm)
                            (qvm::classical-memory qvm))))))
    (t
     (format-log "You must benchmark, start a server, or execute a Quil file.")
     (uiop:quit)))

  (uiop:quit))

(defun %main (argv)
  (sb-ext:disable-debugger)
  (setf *entered-from-main* t)

  ;; Save the program name away.
  (setf *program-name* (pop argv))

  ;; Run the program.
  (handler-case
      (cond
        ((null argv)
         (show-help)
         (uiop:quit))
        (t
         (command-line-arguments:handle-command-line
          *option-spec*
          'process-options
          :command-line argv
          :name "qvm"
          :rest-arity nil)))
    ;; Handle Ctrl-C
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (uiop:quit 0))
    ;; General errors.
    (error (c)
      (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
      (uiop:quit 1))))

(defclass vhost (tbnl:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  (:default-initargs
   :address *host-address*
   :document-root nil
   :error-template-directory nil
   :persistent-connections-p t))

(defmethod tbnl:acceptor-status-message ((acceptor vhost) http-status-code &key error &allow-other-keys)
  (if (eql http-status-code tbnl:+http-internal-server-error+)
      error
      (call-next-method)))

(defun create-prefix/method-dispatcher (prefix method handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
starts with the string PREFIX."
  (lambda (request)
    (and (eq method (tbnl:request-method request))
         (let ((mismatch (mismatch (tbnl:script-name request) prefix
                                   :test #'char=)))
           (and (or (null mismatch)
                    (>= mismatch (length prefix)))
                handler)))))

(defmethod tbnl:acceptor-dispatch-request ((vhost vhost) request)
  ;; try REQUEST on each dispatcher in turn
  (mapc (lambda (dispatcher)
          (let ((handler (funcall dispatcher request)))
            (when handler               ; Handler found. FUNCALL it and return result
              (return-from tbnl:acceptor-dispatch-request (funcall handler request)))))
        (dispatch-table vhost))
  (call-next-method))

(defun process-quil (quil)
  (let* ((mapping (quil::compute-qubit-mapping quil))
         (trivial-mapping-p
           (loop :for x :across mapping
                 :for i :from 0
                 :always (= x i))))
    (unless trivial-mapping-p
      (format-log "Mapping qubits: ~{~A~^, ~}"
                  (loop :for x :across mapping
                        :for i :from 0
                        :when (/= i x)
                          :collect (format nil "~D -> ~D" x i)))
      (quil::transform 'quil::compress-qubits quil))
    quil))

(declaim (inline write-64-be))
(defun write-64-be (byte stream)
  "Write the 64-bit unsigned-byte BYTE to the binary stream STREAM."
  (declare (optimize speed (safety 0) (debug 0))
           (type (unsigned-byte 64) byte))
  (let ((a (ldb (byte 8 0) byte))
        (b (ldb (byte 8 8) byte))
        (c (ldb (byte 8 16) byte))
        (d (ldb (byte 8 24) byte))
        (e (ldb (byte 8 32) byte))
        (f (ldb (byte 8 40) byte))
        (g (ldb (byte 8 48) byte))
        (h (ldb (byte 8 56) byte)))
    (declare (type (unsigned-byte 8) a b c d e f g h))
    (write-byte h stream)
    (write-byte g stream)
    (write-byte f stream)
    (write-byte e stream)
    (write-byte d stream)
    (write-byte c stream)
    (write-byte b stream)
    (write-byte a stream)
    nil))

(declaim (inline write-complex-double-float-as-binary))
(defun write-complex-double-float-as-binary (z stream)
  "Take a complex double-float and write to STREAM its binary representation in big endian (total 16 octets)."
  (declare (optimize speed (safety 0) (debug 0))
           (type (complex double-float) z))
  (let ((re (realpart z))
        (im (imagpart z)))
    (declare (type double-float re im)
             (dynamic-extent re im))
    (let ((encoded-re (ieee-floats:encode-float64 re))
          (encoded-im (ieee-floats:encode-float64 im)))
      (declare (type (unsigned-byte 64) encoded-re encoded-im)
               (dynamic-extent encoded-re encoded-im))
      (write-64-be encoded-re stream)
      (write-64-be encoded-im stream))))

(defun get-random-state (arg)
  (etypecase arg
    (null (make-random-state t))
    (unsigned-byte (sb-ext:seed-random-state arg))))

(defun check-required-fields (hash-table &rest fields)
  (dolist (field fields t)
    (when (null (nth-value 1 (gethash field hash-table)))
      (error "Expected the field ~A to exist." field))))

(defun handle-post-request (request)
  (when (null tbnl:*session*)
    (tbnl:start-session))

  (let* ((api-key (tbnl:header-in* ':X-API-KEY request))
         (user-id (tbnl:header-in* ':X-USER-ID request))
         (data (hunchentoot:raw-post-data :request request
                                          :force-text t))
         (js (let* ((yason:*parse-object-key-fn* #'keywordify)
                    (js (ignore-errors (yason:parse data))))
               (unless (and (hash-table-p js)
                            (check-required-fields js ':TYPE))
                 (error "Invalid request."))
               js))
         (type (gethash ':TYPE js))
         (gate-noise (gethash ':GATE-NOISE js))
         (measurement-noise (gethash ':MEASUREMENT-NOISE js))
         (*random-state* (get-random-state (gethash ':RNG-SEED js))))

    ;; Basic analytics
    (when (and (not (null api-key))
               (stringp api-key))
      (format-log "Got ~S request from API key: ~S" type api-key)
      (with-redis (nil nil)
        ;; Record the API key in the DB.
        (record-api-key api-key)

        ;; Record the user ID if it's present.
        (when (and (not (null user-id))
                   (stringp user-id))
          (record-user-id user-id))

        ;; Record the payload in most cases.
        (unless (member type '(:ping :version :instructions-served)
                        :test #'string-equal)
          (record-request-payload api-key data user-id))))

    ;; Dispatch
    (ecase (keywordify type)
      ;; For simple tests.
      ((:ping)
       (format nil "pong ~D" (get-universal-time)))

      ((:instructions-served)
       (format nil "~D" (instructions-served)))

      ;; Get the version of everything.
      ((:version)
       (string-right-trim
        '(#\Newline)
        (with-output-to-string (*standard-output*)
          (show-version))))

      ;; Multishot experiments.
      ((:multishot)
       (check-required-fields js
                              ':ADDRESSES
                              ':TRIALS
                              ':QUIL-INSTRUCTIONS)
       (let* ((addresses (gethash ':ADDRESSES js))
              (num-trials (gethash ':TRIALS js))
              (isns (gethash ':QUIL-INSTRUCTIONS js))
              (quil (let ((quil::*allow-unresolved-applications* t))
                      (process-quil (safely-parse-quil-string isns))))
              (num-qubits (cl-quil:qubits-needed quil))
              (results (perform-multishot quil num-qubits addresses num-trials
                                          :gate-noise gate-noise
                                          :measurement-noise measurement-noise)))
         (with-output-to-string (s)
           (yason:encode results s))))

      ;; Multishot with final measure.
      ((:multishot-measure)
       (check-required-fields js
                              ':QUBITS
                              ':TRIALS
                              ':QUIL-INSTRUCTIONS)
       (let* ((quil:*allow-unresolved-applications* t)
              (qubits (gethash ':QUBITS js))
              (num-trials (gethash ':TRIALS js))
              (quil (safely-parse-quil-string
                     (gethash ':QUIL-INSTRUCTIONS js)))
              (num-qubits (cl-quil:qubits-needed quil))
              (results (perform-multishot-measure
                        quil
                        num-qubits
                        qubits
                        num-trials)))
         (with-output-to-string (s)
           (yason:encode results s))))

      ;; Expectation value computation.
      ((:expectation)
       (check-required-fields js
                              ':STATE-PREPARATION
                              ':OPERATORS)
       (let* ((quil:*allow-unresolved-applications* t)
              (state-prep (safely-parse-quil-string
                           (gethash ':STATE-PREPARATION js)))
              (operators (map 'list #'safely-parse-quil-string
                              (gethash ':OPERATORS js)))
              (num-qubits (loop :for p :in (cons state-prep operators)
                                :maximize (cl-quil:qubits-needed p)))
              (results (perform-expectation state-prep operators num-qubits
                                            :gate-noise gate-noise
                                            :measurement-noise measurement-noise)))
         (with-output-to-string (s)
           (yason:encode results s))))

      ;; Wavefunction computation.
      ((:wavefunction)
       (check-required-fields js
                              ':QUIL-INSTRUCTIONS
                              ':ADDRESSES)
       (let* ((isns (gethash ':QUIL-INSTRUCTIONS js))
              (addresses (gethash ':ADDRESSES js))
              (quil (let ((quil:*allow-unresolved-applications* t))
                      (process-quil (safely-parse-quil-string isns))))
              (num-qubits (cl-quil:qubits-needed quil)))
         ;; Sanity check the addresses, since we are unpacking them
         ;; here, not in PERFORM-WAVEFUNCTION.
         (check-type addresses alexandria:proper-list)
         (assert (every (alexandria:conjoin #'integerp (complement #'minusp)) addresses))

         (let ((qvm (perform-wavefunction quil num-qubits
                                          :gate-noise gate-noise
                                          :measurement-noise measurement-noise))
               (num-mem-octets (ceiling (/ (length addresses) 8)))
               send-response-time)
           (with-timing (send-response-time)
             (setf (tbnl:content-type*) "application/octet-stream")
             (setf (tbnl:header-out ':ACCEPT) "application/octet-stream")
             (setf (tbnl:content-length*)
                   (+ num-mem-octets (* 2 ; doubles/complex
                                        8 ; octets/double
                                        (expt 2 (qvm:number-of-qubits qvm)))))
             (let ((reply-stream (tbnl:send-headers)))
               ;; Write out the classical bits.
               (loop :with memory := (make-array num-mem-octets :element-type '(unsigned-byte 8)
                                                                :initial-element 0)
                     :for i :from 0
                     :for a :in addresses
                     :do (multiple-value-bind (octet-number bit-number) (floor i 8)
                           (when (= 1 (classical-bit qvm a))
                             (setf (aref memory octet-number)
                                   (dpb 1 (byte 1 bit-number) (aref memory octet-number)))))
                     :finally (map nil (lambda (o) (write-byte o reply-stream)) memory))

               ;; Write out the wavefunction.
               (qvm:map-amplitudes
                qvm
                (lambda (z) (write-complex-double-float-as-binary z reply-stream)))))
           (format-log "Response sent in ~D ms." send-response-time)))))))

(defun make-appropriate-qvm (num-qubits gate-noise measurement-noise)
  (throw-error-if-over-allocated num-qubits)
  (format-log "Making qvm of ~D qubit~:P" num-qubits)
  (if (and (null gate-noise) (null measurement-noise))
      (make-instance 'profiled-pure-state-qvm
                     :number-of-qubits num-qubits
                     :classical-memory-size 64)
      (let ((gate-noise (or gate-noise '(0.0 0.0 0.0)))
            (measurement-noise (or measurement-noise '(0.0 0.0 0.0))))
        (make-instance 'profiled-depolarizing-qvm
                       :number-of-qubits num-qubits
                       :classical-memory-size 64
                       :x (elt gate-noise 0)
                       :y (elt gate-noise 1)
                       :z (elt gate-noise 2)
                       :measure-x (elt measurement-noise 0)
                       :measure-y (elt measurement-noise 1)
                       :measure-z (elt measurement-noise 2)))))

(defun bell-program (n)
  (let ((quil:*allow-unresolved-applications* t))
    (safely-parse-quil-string
     (with-output-to-string (*standard-output*)
       (format t "H 0~%")
       (loop :for i :below (1- n)
             :do (format t "CNOT ~D ~D~%" i (1+ i)))
       (loop :for i :below n
             :do (format t "MEASURE ~D [~D]~%" i i))))))

(defun qft-program (n)
  (qvm-examples:qft-circuit (loop :for i :below n :collect i)))

(defun hadamard-program (n)
  (make-instance
   'quil:parsed-program
   :executable-code (loop :with v := (make-array (* 2 n))
                          :for i :below n
                          :do (setf (aref v i)
                                    (make-instance
                                     'quil:unresolved-application
                                     :operator "H"
                                     :arguments (list (quil:qubit i))))
                              (setf (aref v (+ n i))
                                    (make-instance
                                     'quil:measure
                                     :qubit (quil:qubit i)
                                     :address (quil:address i)))
                          :finally (return v))))

(defun perform-benchmark (type num-qubits)
  (check-type num-qubits (integer 1))
  (let ((p (alexandria:eswitch (type :test #'string-equal)
             ("bell" (bell-program num-qubits))
             ("qft"  (qft-program num-qubits))
             ("hadamard" (hadamard-program num-qubits))))
        (q (qvm:make-qvm num-qubits))
        timing)
    (qvm:load-program q p)

    (format-log "Performing ~S benchmark with ~D qubits...~%" type num-qubits)

    (sb-ext:gc :full t)

    (with-timing (timing)
      (time (qvm:run q)))

    (room)
    (terpri)
    (format-log "Total time for program run: ~D ms" timing)))

(defun perform-multishot (quil num-qubits addresses num-trials &key gate-noise measurement-noise)
  (check-type quil quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type num-trials (integer 0))
  (check-type addresses alexandria:proper-list)
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (every (alexandria:conjoin #'integerp (complement #'minusp)) addresses))
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  (when (or (null addresses) (zerop num-trials))
    (return-from perform-multishot nil))

  (let ((qvm (make-appropriate-qvm num-qubits gate-noise measurement-noise))
        (trial-results nil)
        timing)
    (flet ((collect-bits (qvm)
             (loop :for address :in addresses
                   :collect (qvm:classical-bit qvm address))))
      (qvm:load-program qvm quil)
      (format-log "Running experiment with ~D trial~:P on ~A"
                  num-trials
                  (class-name (class-of qvm)))
      (with-timing (timing)
        (dotimes (trial num-trials)
          ;; Reset the program counter.
          (setf (qvm::pc qvm) 0)
          ;; Reset the amplitudes.
          (qvm::reset qvm)
          ;; Run the program.
          (with-timeout
            (qvm:run qvm))
          ;; Collect bits.
          (push (collect-bits qvm) trial-results))
        (increment-instruction-counter (instructions-executed qvm)))

      (format-log "Finished in ~D ms" timing)
      (nreverse trial-results))))

(defun perform-multishot-measure (quil num-qubits qubits num-trials)
  (check-type quil quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type num-trials (integer 0))
  (check-type qubits alexandria:proper-list)
  (assert (every (alexandria:conjoin #'integerp (complement #'minusp)) qubits))

  (when (or (null qubits) (zerop num-trials))
    (return-from perform-multishot-measure nil))

  (setf num-qubits (max num-qubits (1+ (reduce #'max qubits))))

  (let ((qvm (make-appropriate-qvm num-qubits nil nil))
        timing)
    ;; Make the initial state.
    (qvm:load-program qvm quil)
    (format-log "Computing wavefunction for multishot measure on ~A."
                (class-name (class-of qvm)))
    (with-timing (timing)
      (with-timeout
          (qvm:run qvm)))
    (format-log "Finished wavefunction computation in ~D ms." timing)
    (format-log "Copying state.")
    (let ((prepared-wf
            (with-timing (timing)
              (copy-seq (qvm::amplitudes qvm))))
          (first-time t))
      (format-log "Copied prepared state in ~D ms." timing)
      (flet ((reload (qvm)
               (unless first-time
                 (replace (qvm::amplitudes qvm) prepared-wf))
               (setf first-time nil)))
        ;; Do the parallel measurements
        (format-log "Doing ~D ~D-qubit measurements." num-trials (length qubits))
        (prog1
            (with-timing (timing)
              (loop :repeat num-trials
                    :collect (progn
                               (reload qvm)
                               (nth-value 1 (qvm::parallel-measure qvm qubits)))))
          (increment-instruction-counter (instructions-executed qvm))
          (format-log "Done measuring in ~D ms." timing))))))

(defun perform-expectation (state-prep operators num-qubits &key gate-noise measurement-noise)
  "Let F be the wavefunction resulting from STATE-PREP on the zero state. Then compute a list of real expectation values of the operators in OPERATORS, namely,

    <F| O1 |F>,  <F| O2 |F>,  ...    for Oi in OPERATORS.
"
  (check-type state-prep quil:parsed-program)
  (dolist (o operators) (check-type o quil:parsed-program))
  (check-type num-qubits (integer 0))
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  ;; If we have nothing to compute the expectation of, then return
  ;; nothing.
  (when (null operators)
    (format-log "No operators to compute expectation of. Returning NIL.")
    (return-from perform-expectation '()))

  ;; Otherwise, go about business.
  (let ((qvm (make-appropriate-qvm num-qubits gate-noise measurement-noise))
        timing)
    ;; Make the initial state.
    (qvm:load-program qvm state-prep)
    (format-log "Computing initial state for expectation value ~
                 computation on ~A"
                (class-name (class-of qvm)))
    (with-timing (timing)
      (with-timeout
          (qvm:run qvm)))
    (format-log "Finished state prep in ~D ms." timing)
    (format-log "Copying prepared state.")
    (let ((prepared-wf
            (with-timing (timing)
              (copy-seq (qvm::amplitudes qvm))))
          (first-time t))
      (format-log "Copied prepared state in ~D ms." timing)
      (flet ((inner-product (a b)
               (declare (type qvm::quantum-state a b))
               (loop :for ai :of-type qvm::cflonum :across a
                     :for bi :of-type qvm::cflonum :across b
                     :sum (* (conjugate ai) bi)))
             (reload (qvm program)
               (unless first-time
                 (replace (qvm::amplitudes qvm) prepared-wf))
               (qvm:load-program qvm program)
               (setf first-time nil)))
        ;; Compute the expectations of the operators.
        (prog1
            (loop :for i :from 1
                  :for op :in operators
                  :collect (let (expectation)
                             (format-log "Computing the expectation value of the ~:R operator." i)
                             (with-timing (timing)
                               (reload qvm op)
                               (qvm:run qvm)
                               (setf expectation (inner-product
                                                  prepared-wf
                                                  (qvm::amplitudes qvm))))
                             (format-log "Computed ~:R expectation value in ~D ms." i timing)
                             (assert (< (abs (imagpart expectation)) 1e-14))
                             (unless (zerop (imagpart expectation))
                               (warn "Non-zero but acceptable imaginary part of expectation value: ~A" expectation))
                             (realpart expectation)))
          (increment-instruction-counter (instructions-executed qvm)))))))

(defun perform-wavefunction (quil num-qubits &key gate-noise measurement-noise)
  (check-type quil quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  (let ((qvm (make-appropriate-qvm num-qubits gate-noise measurement-noise))
        timing)
    (qvm:load-program qvm quil)
    (format-log "Running experiment on ~A" (class-name (class-of qvm)))
    (with-timing (timing)
      (with-timeout
        (qvm:run qvm)))
    (increment-instruction-counter (instructions-executed qvm))
    (format-log "Finished in ~D ms" timing)
    qvm))


(defvar *app* nil)

(defun start-server (port)
  (setq tbnl:*show-lisp-errors-p* nil
        tbnl:*show-lisp-backtraces-p* nil
        tbnl:*catch-errors-p* (image-p))
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
