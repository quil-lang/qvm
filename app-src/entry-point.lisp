;;;; qvm-app/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

;;;; Entry-point into binary executable.

(defvar *program-name* "qvm")

(defparameter *option-spec*
  '((("server" #\S) :type boolean :optional t :documentation "start the QVE server")
    (("memory" #\m) :type integer :initial-value 8 :documentation "classical memory size in bytes")
    (("help" #\h) :type boolean :optional t :documentation "display help")))

(defun format-log (fmt-string &rest args)
  (format *trace-output* "~&[~A] " (get-universal-time))
  (apply 'format *trace-output* fmt-string args)
  (terpri *trace-output*))

(defun show-help ()
  (format t "Run Quil file:~%")
  (format t "    ~A <num-qubits> <quil-file> [<options>...]~%~%" *program-name*)
  (format t "Start QVE server:~%")
  (format t "    ~A~%~%" *program-name*)
  (command-line-arguments:show-option-help *option-spec* :sort-names t))

(defmacro with-timing ((var) &body body)
  (let ((start (gensym "START-")))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,var (round (* 1000 (- (get-internal-real-time) ,start))
                           internal-time-units-per-second))))))

(defun parity-even-p (state qubits)
  (labels ((qubits-mask (qubits)
             (loop :with mask := 0
                   :for q :in qubits
                   :do (setf (ldb (byte 1 q) mask) 1)
                   :finally (return mask))))
    (evenp (logcount (logand state (qubits-mask qubits))))))

(defun expectation-value (qvm qubits)
  (loop :with expectation := 0.0d0
        :for state :from 0
        :for a :across (qvm::amplitudes qvm)
        :do (if (parity-even-p state qubits)
                (incf expectation (probability a))
                (decf expectation (probability a)))
        :finally (return expectation)))

(defun run-experiment (num-qubits qubits-to-measure trials quil)
  (let ((qvm (qvm:make-qvm num-qubits))
        (stats (make-array (length qubits-to-measure) :initial-element 0))
        timing
        (expectation 0))
    (format-log "Running experiment with ~D trial~:P" trials)
    ;; Direct wavefunction probability computation
    #-ignore
    (with-timing (timing)
      (qvm:load-program qvm quil)
      (qvm:run qvm)
      (setf (aref stats 0)
            (* trials (expectation-value qvm qubits-to-measure))))
    ;; Sampling
    #+ignore
    (with-timing (timing)
      (dotimes (i trials)
        (qvm:load-program qvm quil)
        (qvm:run qvm)
        (multiple-value-bind (new-qvm cbits)
            (qvm:parallel-measure qvm qubits-to-measure)
          (setf qvm (qvm::reset new-qvm))
          (if (oddp (count 1 cbits))
              (decf expectation)
              (incf expectation)))))
    (format-log "Finished in ~D ms" timing)
    (coerce stats 'list)))

(defun slurp-stream (stream)
  (with-output-to-string (s)
    (loop :for byte := (read-byte stream nil nil) :then (read-byte stream nil nil)
          :until (null byte)
          :do (write-char (code-char byte) s))))

(defun keywordify (str)
  (intern (string-upcase str) :keyword))

(defparameter *host-address* "127.0.0.1")
(defparameter *host-port* 5000)

(defun start-server-app ()
  (format-log "Starting server on port ~D." *host-port*)
  (start-server))

(defun format-complex (c)
  (cond
    ((zerop (imagpart c))
     (format nil "~F" (realpart c)))
    ((plusp (imagpart c))
     (format nil "~F+~Fi" (realpart c) (imagpart c)))
    ((minusp (imagpart c))
     (format nil "~F-~Fi" (realpart c) (abs (imagpart c))))))

(defun process-options (qubits file &key help memory server)
  (when help
    (show-help)
    (uiop:quit))

  (setf qubits (parse-integer qubits :junk-allowed nil))

  (cond
    (server (start-server-app))
    (t
     (let (qvm program alloc-time exec-time)
       (format-log "Allocating memory for QVM")
       (with-timing (alloc-time)
         (setf qvm (make-qvm qubits :classical-memory-size memory)))
       (format-log "Allocation completed in ~D ms. Reading in program," alloc-time)
       (setf program (let ((quil::*allow-unresolved-applications* t))
                       (quil:read-quil-file file)))
       (format-log "Loading quantum program.")
       (load-program qvm program)
       (format-log "Executing quantum program.")
       (setf *random-state* (make-random-state t)) ; Seed random.
       (with-timing (exec-time)
         (run qvm))
       (format-log "Execution completed in ~D ms. Printing state." exec-time)
       (when (<= qubits 5)
         (format-log "Amplitudes: ~{~A~^, ~}" (map 'list 'format-complex (qvm::amplitudes qvm)))
         (format-log "Probabilities: ~{~F~^, ~}" (map 'list 'probability (qvm::amplitudes qvm))))
       (format-log "Classical memory (MSB -> LSB): ~v,'0B"
                   (* 8 (qvm::classical-memory-size qvm))
                   (qvm::classical-memory qvm)))))

  (uiop:quit))

(defun %main (argv)
  ;; Save the program name away.
  (setf *program-name* (pop argv))

  ;; Welcome message.
  (format-log "Welcome to the Rigetti QVM.")

  ;; Run the program.
  (handler-case
      (cond
        ((null argv)
         (start-server-app)
         (loop (sleep 1)))
        ((null (cdr argv))
         (show-help)
         (uiop:quit))
        (t
         (command-line-arguments:handle-command-line
          *option-spec*
          'process-options
          :command-line argv
          :name "qvm"
          :positional-arity 2
          :rest-arity nil)))
    (error (c)
      (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
      (uiop:quit 1))))

(defclass vhost (tbnl:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  (:default-initargs
   :address *host-address*))

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

(defun handle-get-request (request)
  (declare (ignore request))
  "<marquee direction=\"right\">Hello from the <strong><blink>Rigetti</blink> QVM</strong>.</marquee>")

(defun handle-post-request (request)
  (let* ((data (hunchentoot:raw-post-data :request request
                                          :force-text t))
         (js (let ((yason:*parse-object-key-fn* #'keywordify))
               (yason:parse data)))
         (qubits (gethash :QUBITS js))
         (qubits-to-measure (gethash :MEASURE js))
         (trials (gethash :TRIALS js))
         (isns (gethash :QUIL-INSTRUCTIONS js))
         (quil (let ((quil::*allow-unresolved-applications* t))
                 (quil:parse-quil-string isns))))
    (format-log "~D qubits, measuring ~A, over ~D trial~:P"
                qubits
                qubits-to-measure
                trials)
    (let* ((counts (run-experiment qubits
                                   qubits-to-measure
                                   trials
                                   quil))
           (res-payload
             (yason:with-output-to-string* (:indent t)
               (yason:with-object ()
                 (yason:encode-object-element "statistics" counts)))))
      ;; Return the payload.
      res-payload)))

(defparameter *app* nil)

(defun start-server ()
  (setq tbnl:*show-lisp-errors-p* nil
        tbnl:*show-lisp-backtraces-p* nil
        tbnl:*catch-errors-p* nil)
  (setq *app* (make-instance 'vhost
                             :address *host-address*
                             :port *host-port*))
  (when (null (dispatch-table *app*))
    (push
     (create-prefix/method-dispatcher "/" :GET 'handle-get-request)
     (dispatch-table *app*))
    (push
     (create-prefix/method-dispatcher "/" :POST 'handle-post-request)
     (dispatch-table *app*)))
  (tbnl:start *app*))

(defun stop-server ()
  (tbnl:stop *app*))

#+#:ignore
(defmethod tbnl:handle-request :before ((acceptor vhost) (request tbnl:request))
  (multiple-value-bind (username pwd) (tbnl:authorization)
    (if (string= pwd "spandex")
        nil
        (tbnl:require-authorization))))
