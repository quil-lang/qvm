;;;; qvm-app/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

;;;; Entry-point into binary executable.

(defvar *program-name* "qvm")

(defparameter *option-spec*
  '((("memory" #\m) :type integer :initial-value 8 :documentation "classical memory size in bytes")
    (("help" #\h) :type boolean :optional t :documentation "display help")))

(defun format-log (fmt-string &rest args)
  (format *trace-output* "~&[~A] " (get-universal-time))
  (apply 'format *trace-output* fmt-string args)
  (terpri *trace-output*))

(defun show-help ()
  (format t "Run QIL file:~%")
  (format t "    ~A <num-qubits> <qil-file> [<options>...]~%~%" *program-name*)
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

(defun run-experiment (num-qubits qubits-to-measure trials qil)
  (let ((qvm (qvm:make-qvm num-qubits))
        (stats (make-array (length qubits-to-measure) :initial-element 0))
        timing
        (expectation 0))
    (format-log "Running experiment with ~D trial~:P" trials)
    ;; Direct wavefunction probability computation
    #-ignore
    (with-timing (timing)
      (qvm:load-program qvm qil)
      (qvm:run qvm)
      (setf (aref stats 0)
            (* trials (expectation-value qvm qubits-to-measure))))
    ;; Sampling
    #+ignore
    (with-timing (timing)
      (dotimes (i trials)
        (qvm:load-program qvm qil)
        (qvm:run qvm)
        (multiple-value-bind (new-qvm cbits)
            (qvm:parallel-measure qvm qubits-to-measure)
          (setf qvm (qvm::reset new-qvm))
          (if (oddp (count 1 cbits))
              (decf expectation)
              (incf expectation)))))
    (format-log "Finished in ~D ms" timing)
    (coerce stats 'list)))

(defparameter *default-port* 5000)

(defun slurp-stream (stream)
  (with-output-to-string (s)
    (loop :for byte := (read-byte stream nil nil) :then (read-byte stream nil nil)
          :until (null byte)
          :do (write-char (code-char byte) s))))

(defun keywordify (str)
  (intern (string-upcase str) :keyword))

(defun start-server ()
  (format-log "Starting server on port ~D." *default-port*)
  (woo:run
   (lambda (env)
     (let ((body-stream (getf env :raw-body)))
       (cond
         ((null body-stream)
          '(400 (:content-type "text/plain") ("Nothing received.")))
         (t
          (let* ((data (slurp-stream body-stream))
                 (js (let ((yason:*parse-object-key-fn* #'keywordify))
                       (yason:parse data)))
                 (qubits (gethash :QUBITS js))
                 (qubits-to-measure (gethash :MEASURE js))
                 (trials (gethash :TRIALS js))
                 (isns (gethash :QIL-INSTRUCTIONS js))
                 (qil (mapcar #'qvm::parse-qil-instruction isns)))
            (format-log "~D qubits, measuring ~A, over ~D trial~:P"
                        qubits
                        qubits-to-measure
                        trials)
            (let ((*print-pretty* nil)
                  (*print-length* 8))
              (format-log "QIL Code: ~S" qil))
            (let* ((counts (run-experiment qubits
                                           qubits-to-measure
                                           trials
                                           qil))
                   (res-payload
                     (yason:with-output-to-string* (:indent t)
                       (yason:with-object ()
                         (yason:encode-object-element "statistics" counts)))))
              `(200 (:content-type "text/plain") (,res-payload))))))))
   :port *default-port*))

(defun format-complex (c)
  (cond
    ((zerop (imagpart c))
     (format nil "~F" (realpart c)))
    ((plusp (imagpart c))
     (format nil "~F+~Fi" (realpart c) (imagpart c)))
    ((minusp (imagpart c))
     (format nil "~F-~Fi" (realpart c) (abs (imagpart c))))))

(defun process-options (qubits file &key help memory)
  (when help
    (show-help)
    (uiop:quit))

  (setf qubits (parse-integer qubits :junk-allowed nil))

  (cond
    (server (start-server))
    (t
     (let (qvm program alloc-time exec-time)
       (format-log "Allocating memory for QVM")
       (with-timing (alloc-time)
         (setf qvm (make-qvm qubits :classical-memory-size memory)))
       (format-log "Allocation completed in ~D ms. Reading in program," alloc-time)
       (setf program (read-qil-file file))
       (format-log "Loading quantum program.")
       (load-program qvm program)
       (format-log "Executing quantum program.")
       (setf *random-state* (make-random-state t)) ; Seed random.
       (with-timing (exec-time)
         (run qvm))
       (format-log "Execution completed in ~D ms. Printing state." exec-time)
       (when (<= qubits 5)
         (format-log "Amplitudes: ~{~A~^, ~}" (map 'list 'format-complex (amplitudes qvm)))
         (format-log "Probabilities: ~{~F~^, ~}" (map 'list 'probability (amplitudes qvm))))
       (format-log "Classical memory (MSB -> LSB): ~v,'0B" (* 8 (classical-memory-size qvm)) (classical-memory qvm)))))

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
         (start-server))
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
    (condition (c)
      (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
      (uiop:quit 1))))
