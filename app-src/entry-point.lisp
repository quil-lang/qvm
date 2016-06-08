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

(defun slurp-stream (stream)
  (with-output-to-string (s)
    (loop :for byte := (read-byte stream nil nil) :then (read-byte stream nil nil)
          :until (null byte)
          :do (write-char (code-char byte) s))))

(defun keywordify (str)
  (intern (string-upcase str) :keyword))

(defparameter *host-address* "0.0.0.0")
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
         (type (gethash ':TYPE js)))
    (ecase (keywordify type)
      ((:multishot)
       (let* ((num-qubits (gethash ':NUM-QUBITS js))
              (addresses (gethash ':ADDRESSES js))
              (num-trials (gethash ':TRIALS js))
              (isns (gethash ':QUIL-INSTRUCTIONS js))
              (quil (let ((quil::*allow-unresolved-applications* t))
                      (quil:parse-quil-string isns)))
              (results (perform-multishot quil num-qubits addresses num-trials)))
         (with-output-to-string (s)
           (yason:encode results s))))
      ((:wavefunction)
       (let* ((num-qubits (gethash ':NUM-QUBITS js))
              (isns (gethash ':QUIL-INSTRUCTIONS js))
              (quil (let ((quil::*allow-unresolved-applications* t))
                      (quil:parse-quil-string isns)))
              (results (perform-wavefunction quil num-qubits)))
         (with-output-to-string (s)
           (yason:encode
            (map 'list (lambda (z) (list (realpart z) (imagpart z)))
                 results)
            s)))))))

(defun perform-multishot (quil num-qubits addresses num-trials)
  (check-type quil quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type num-trials (integer 0))
  (check-type addresses alexandria:proper-list)
  (assert (every (alexandria:conjoin #'integerp (complement #'minusp)) addresses))

  (let ((qvm (qvm:make-qvm num-qubits))
        (trial-results nil)
        timing)
    (flet ((collect-bits (qvm)
             (loop :for address :in addresses
                   :collect (qvm:classical-bit qvm address))))
      (qvm:load-program qvm quil)
      (format-log "Running experiment with ~D trial~:P" num-trials)
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

(defun perform-wavefunction (quil num-qubits)
  (check-type quil quil:parsed-program)
  (check-type num-qubits (integer 0))

  (let ((qvm (qvm:make-qvm num-qubits))
        timing)
    (qvm:load-program qvm quil)
    (format-log "Running experiment")
    (with-timing (timing)
      (qvm:run qvm))
    (format-log "Finished in ~D ms" timing)
    (qvm::amplitudes qvm)))


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
     (create-prefix/method-dispatcher "/" ':GET 'handle-get-request)
     (dispatch-table *app*))
    (push
     (create-prefix/method-dispatcher "/" ':POST 'handle-post-request)
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
