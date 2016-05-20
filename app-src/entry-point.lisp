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
  (format t "Usage: ~A <num-qubits> <qil-file> [<options>...]~%~%" *program-name*)
  (command-line-arguments:show-option-help *option-spec* :sort-names t))

(defmacro with-timing ((var) &body body)
  (let ((start (gensym "START-")))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,var (round (* 1000 (- (get-internal-real-time) ,start))
                           internal-time-units-per-second))))))

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
    (format-log "Classical memory (MSB -> LSB): ~v,'0B" (* 8 (classical-memory-size qvm)) (classical-memory qvm)))
  
  (uiop:quit))

(defun slurp-stream (stream)
  (with-output-to-string (s)
    (loop :for byte := (read-byte stream nil nil) :then (read-byte stream nil nil)
          :until (null byte)
          :do (write-char (code-char byte) s))))

(defun keywordify (str)
  (intern (string-upcase str) :keyword))

(defun run-experiment (num-qubits qubits-to-measure trials qil)
  (let ((qvm (qvm:make-qvm num-qubits))
        (stats (make-array (length qubits-to-measure) :initial-element 0))
        timing)
    (format-log "Running experiment with ~D trials" trials)
    (with-timing (timing)
      (qvm:load-program qvm qil)
      (qvm:run qvm)
      (print (qvm::amplitudes qvm))
      (setf (aref stats 0)
            (* trials (qvm::multi-qubit-probability qvm qubits-to-measure)))
      #+ignore
      (dotimes (i trials)
        (qvm:load-program qvm qil)
        (qvm:run qvm)
        (multiple-value-bind (new-qvm cbits)
            (qvm:parallel-measure qvm qubits-to-measure)
          (setf qvm (qvm::reset new-qvm))
          (map-into stats #'+ stats cbits))))
    (format-log "Finished ~D trials in ~D ms" trials timing)
    (coerce stats 'list)))

(defun filt (isn)
  (if (string-equal "RZ" (symbol-name (car isn)))
      (list (car (second isn)))
      nil))

(defun %main (argv)
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
            ;(format-log "HTTP Payload: ~S" data)
            (format-log "  Qubits: ~A" qubits)
            (format-log "  Measure: ~A" qubits-to-measure)
            (format-log "  Trials: ~A" trials)
            (let ((*print-pretty* nil))
              (format-log "  QIL: ~S" (mapcan #'filt qil)))
            (let* ((counts (run-experiment qubits
                                           qubits-to-measure
                                           trials
                                           qil))
                   (res-payload
                     (yason:with-output-to-string* (:indent t)
                       (yason:with-object ()
                         (yason:encode-object-element "statistics" counts)))))
              `(200 (:content-type "text/plain") (,res-payload))))))))))

#+#:ignore
(defun %main (argv)
  ;; Save the program name away.
  (setf *program-name* (pop argv))

  ;; Check for required arguments.
  (unless (<= 2 (length argv))
    (show-help)
    (uiop:quit))

  ;; Run the program.
  (handler-case
      (command-line-arguments:handle-command-line
       *option-spec*
       'process-options
       :command-line argv
       :name "qvm"
       :positional-arity 2
       :rest-arity nil)
    (condition (c)
      (format *error-output* "~&! ! ! Condition raised: ~A~%" c)
      (uiop:quit 1))))
