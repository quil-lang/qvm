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
          "unknown"))))

(alexandria:define-constant +APP-VERSION+
    (system-version '#:qvm-app)
  :test #'string=
  :documentation "The version of the QVM application.")

(alexandria:define-constant +QVM-VERSION+
    (system-version '#:qvm)
  :test #'string=
  :documentation "The version of the QVM itself.")

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

(defparameter *option-spec*
  '((("execute" #\e)
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

    (("memory" #\m)
     :type integer
     :initial-value 64
     :documentation "classical memory size in bits")

    (("num-workers" #\w)
     :type integer
     :initial-value 0
     :documentation "workers to use in parallel (0 => maximum number)")

    (("help" #\h)
     :type boolean
     :optional t
     :documentation "display help")

    (("version" #\v)
     :type boolean
     :optional t
     :documentation "display the versions of the app and underlying QVM")

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
     :documentation "port of Redis DB")))

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
  (format t "~A (qvm: ~A)~%" +APP-VERSION+ +QVM-VERSION+))

(defun show-welcome ()
  (format t "~&~
******************************
* Welcome to the Rigetti QVM *~%~
******************************~%")
  (format t "(Configured with ~D MiB of workspace and ~D worker~:P.)~2%"
          (floor (sb-ext:dynamic-space-size) (expt 1024 2))
          (or *num-workers* (max 1 (1- (qvm:count-logical-cores))))))

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

(defun process-options (&key version execute help memory server port swank-port db-host db-port num-workers)
  (when help
    (show-help)
    (uiop:quit))

  (when version
    (show-version)
    (uiop:quit))

  (cond
    ((zerop num-workers)
     (qvm:prepare-for-parallelization))
    (t
     (qvm:prepare-for-parallelization num-workers)
     (setf *num-workers* num-workers)))

  (setf *qvm-db-host* db-host
        *qvm-db-port* db-port)

  ;; Show the welcome message.
  (show-welcome)

  ;; Start Swank if we were asked. Re-enable the debugger.

  (when swank-port
   (sb-ext:enable-debugger)
   (format-log "Starting Swank on port ~D" swank-port)
   (setf swank:*use-dedicated-output-stream* nil)
   (swank:create-server :port swank-port
                        :dont-close t))

  (cond
    ;; Server mode.
    ((or server port)
     (when execute
       (format-log "Ignoring execute option: ~S" execute))
     (start-server-app port))
    ;; Batch mode.
    (t
     (let (qvm program alloc-time exec-time qubits)
       (format-log "Reading program.")
       (setf program (let ((quil::*allow-unresolved-applications* t))
                       (quil:read-quil-file execute)))
       (setf qubits (cl-quil:qubits-needed program))

       (format-log "Allocating memory for QVM of ~D qubits." qubits)
       (with-timing (alloc-time)
         (setf qvm (make-qvm qubits :classical-memory-size memory)))
       (format-log "Allocation completed in ~D ms." alloc-time)

       (format-log "Loading quantum program.")
       (load-program qvm program)

       (format-log "Executing quantum program.")
       (setf *random-state* (make-random-state t)) ; Seed random.
       (with-timing (exec-time)
         (run qvm))
       (format-log "Execution completed in ~D ms." exec-time)
       (when (<= qubits 5)
         (format-log "Printing state.")
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
       (format-log "Classical memory (MSB -> LSB): ~v,'0B"
                   (qvm::classical-memory-size qvm)
                   (qvm::classical-memory qvm)))))

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

(defun handle-post-request (request)
  (when (null tbnl:*session*)
    (tbnl:start-session))
  (let* ((data (hunchentoot:raw-post-data :request request
                                          :force-text t))
         (js (let ((yason:*parse-object-key-fn* #'keywordify))
               (yason:parse data)))
         (type (gethash ':TYPE js))
         (gate-noise (gethash ':GATE-NOISE js))
         (measurement-noise (gethash ':MEASUREMENT-NOISE js)))
    (ecase (keywordify type)
      ;; For simple tests.
      ((:ping)
       (format nil "pong ~D" (get-universal-time)))

      ;; Get the version of everything.
      ((:version)
       (string-right-trim
        '(#\Newline)
        (with-output-to-string (*standard-output*)
          (show-version))))

      ;; Multishot experiments.
      ((:multishot)
       (let* ((addresses (gethash ':ADDRESSES js))
              (num-trials (gethash ':TRIALS js))
              (isns (gethash ':QUIL-INSTRUCTIONS js))
              (quil (let ((quil::*allow-unresolved-applications* t))
                      (process-quil (quil:parse-quil-string isns))))
              (num-qubits (cl-quil:qubits-needed quil))
              (results (perform-multishot quil num-qubits addresses num-trials
                                          :gate-noise gate-noise
                                          :measurement-noise measurement-noise)))
         (with-output-to-string (s)
           (yason:encode results s))))

      ;; Wavefunction computation.
      ((:wavefunction)
       (let* ((isns (gethash ':QUIL-INSTRUCTIONS js))
              (quil (let ((quil::*allow-unresolved-applications* t))
                      (process-quil (quil:parse-quil-string isns))))
              (num-qubits (cl-quil:qubits-needed quil))
              (qvm (perform-wavefunction quil num-qubits
                                         :gate-noise gate-noise
                                         :measurement-noise measurement-noise))
              send-response-time)
         (with-timing (send-response-time)
           (setf (tbnl:content-type*) "application/octet-stream")
           (setf (tbnl:content-length*) (* 2 ; doubles/complex
                                           8 ; octets/double
                                           (expt 2 (qvm:number-of-qubits qvm))))
           (let ((reply-stream (tbnl:send-headers)))
             (qvm:map-amplitudes
              qvm
              (lambda (z) (write-complex-double-float-as-binary z reply-stream)))))
         (format-log "Response sent in ~D ms." send-response-time))))))

(defun make-appropriate-qvm (num-qubits gate-noise measurement-noise)
  (format-log "Making qvm of ~D qubit~:P" num-qubits)
  (if (and (null gate-noise) (null measurement-noise))
      (qvm:make-qvm num-qubits)
      (let ((gate-noise (or gate-noise '(0.0 0.0 0.0)))
            (measurement-noise (or measurement-noise '(0.0 0.0 0.0))))
        (make-instance 'qvm::noisy-qvm
                       :number-of-qubits num-qubits
                       :classical-memory-size 64
                       :x (elt gate-noise 0)
                       :y (elt gate-noise 1)
                       :z (elt gate-noise 2)
                       :measure-x (elt measurement-noise 0)
                       :measure-y (elt measurement-noise 1)
                       :measure-z (elt measurement-noise 2)))))

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
          (qvm:run qvm)
          ;; Collect bits.
          (push (collect-bits qvm) trial-results)))
      (format-log "Finished in ~D ms" timing)
      (nreverse trial-results))))

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
      (qvm:run qvm))
    (format-log "Finished in ~D ms" timing)
    qvm))


(defvar *app* nil)

(defun static-file-dispatcher (uri path)
  ;; the dispatcher
  (lambda (request)
    (when (string= uri (tbnl:script-name request))
      ;; the handler
      (lambda (&rest args)
        (declare (ignore args))
        (tbnl:handle-static-file path nil)))))

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
