;;;; qvm-app/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

;;;; Entry-point into binary executable.

(defvar *entered-from-main* nil)

(defun image-p ()
  *entered-from-main*)

(defun image-directory-pathname ()
  (if (image-p)
      (cl-fad:pathname-directory-pathname
       sb-ext:*core-pathname*)
      nil))

(defvar *program-name* "qvm")

(defparameter *option-spec*
  '((("server" #\S) :type boolean :optional t :documentation "start the QVE server")
    (("memory" #\m) :type integer :initial-value 8 :documentation "classical memory size in bytes")
    (("help" #\h) :type boolean :optional t :documentation "display help")))

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
  (format t "Run Quil file:~%")
  (format t "    ~A <quil-file> [<options>...]~%~%" *program-name*)
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

(defun process-options (file &key help memory server)
  (when help
    (show-help)
    (uiop:quit))

  (cond
    (server (start-server-app))
    (t
     (let (qvm program alloc-time exec-time qubits)
       (format-log "Reading program.")
       (setf program (let ((quil::*allow-unresolved-applications* t))
                       (quil:read-quil-file file)))
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
         (format-log "Amplitudes: ~{~A~^, ~}" (map 'list 'format-complex (qvm::amplitudes qvm)))
         (format-log "Probabilities: ~{~F~^, ~}" (map 'list 'probability (qvm::amplitudes qvm))))
       (format-log "Classical memory (MSB -> LSB): ~v,'0B"
                   (* 8 (qvm::classical-memory-size qvm))
                   (qvm::classical-memory qvm)))))

  (uiop:quit))

(defun %main (argv)
  (setf *entered-from-main* t)
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
        (t
         (command-line-arguments:handle-command-line
          *option-spec*
          'process-options
          :command-line argv
          :name "qvm"
          :positional-arity 1
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
       "pong")

      ;; Multishot experiments.
      ((:multishot)
       (let* ((addresses (gethash ':ADDRESSES js))
              (num-trials (gethash ':TRIALS js))
              (isns (gethash ':QUIL-INSTRUCTIONS js))
              (quil (let ((quil::*allow-unresolved-applications* t))
                      (quil:parse-quil-string isns)))
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
                      (quil:parse-quil-string isns)))
              (num-qubits (cl-quil:qubits-needed quil))
              (results (perform-wavefunction quil num-qubits
                                             :gate-noise gate-noise
                                             :measurement-noise measurement-noise)))
         (with-output-to-string (s)
           (yason:encode
            (map 'list (lambda (z) (list (realpart z) (imagpart z)))
                 results)
            s)))))))

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
    (format-log "Running experiment")
    (with-timing (timing)
      (qvm:run qvm))
    (format-log "Finished in ~D ms" timing)
    (qvm::amplitudes qvm)))


(defvar *app* nil)

(defun static-file-dispatcher (uri path)
  ;; the dispatcher
  (lambda (request)
    (when (string= uri (tbnl:script-name request))
      ;; the handler
      (lambda (&rest args)
        (declare (ignore args))
        (tbnl:handle-static-file path nil)))))

(defun start-server ()
  (setq tbnl:*show-lisp-errors-p* nil
        tbnl:*show-lisp-backtraces-p* nil
        tbnl:*catch-errors-p* (image-p))
  (setq *app* (make-instance
               'vhost
               :address *host-address*
               :port *host-port*
               :taskmaster (make-instance 'tbnl:one-thread-per-connection-taskmaster)))
  (when (null (dispatch-table *app*))
    (push
     (create-prefix/method-dispatcher "/" ':POST 'handle-post-request)
     (dispatch-table *app*)))
  (tbnl:start *app*))

(defun stop-server ()
  (tbnl:stop *app*))
