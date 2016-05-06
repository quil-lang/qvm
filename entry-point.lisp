;;;; entry-point.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Entry-point into (optional) binary executable.

(defvar *program-name* "qvm")

(defparameter *option-spec*
  '((("memory" #\m) :type integer :initial-value 8 :documentation "classical memory size in bytes")
    (("help" #\h) :type boolean :optional t :documentation "display help")))

(defun format-log (fmt-string &rest args)
  (format *trace-output* "~&[~A] " (get-universal-time))
  (apply 'format *trace-output* fmt-string args)
  (terpri *trace-output*))

(defun read-qil-file (filespec)
  (let ((*read-eval* nil))
    (with-open-file (s filespec :direction ':input
                                :if-does-not-exist ':error)
      (let ((*package* (find-package :qvm)))
        (read s)))))

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
    (format-log "Classical memory: ~A" (classical-memory qvm)))
  
  (uiop:quit))

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
