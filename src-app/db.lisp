;;;; app-src/db.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(alexandria:define-constant +instruction-counter-key+ "instr_counter"
  :test #'string=
  :documentation "The database key to store the number of instructions executed.")

(defvar *qvm-db-host* nil
  "The hostname of the QVM stats DB.")

(defvar *qvm-db-port* nil
  "The port of the QVM stats DB.")

(defmacro with-redis ((&optional (run-anyway nil) (pipeline t)) &body body)
  "Execute BODY, attempting to make a Redis connection around it. If there is a failure in making a Redis connection, then ignore the failure and execute BODY anyway only if RUN-ANYWAY is true.

If PIPELINE is true, then pipeline the requests."
  (alexandria:with-gensyms (fun c)
    (alexandria:once-only (run-anyway pipeline)
      `(flet ((,fun () ,@body))
         (cond
           ((not (and *qvm-db-host* *qvm-db-port*))
            (when ,run-anyway
              (,fun)))
           (t
            (handler-case (redis:with-connection (:host *qvm-db-host*
                                                  :port *qvm-db-port*)
                            (if ,pipeline
                                (redis:with-pipelining
                                  (,fun))
                                (,fun)))
              (usocket:connection-refused-error (,c)
                (declare (ignore ,c))
                (warn "Connection refused, continuing without Redis connection.")
                (when ,run-anyway
                  (,fun)))
              (usocket:host-unreachable-error (,c)
                (declare (ignore ,c))
                (warn "Host unreachable, continuing without Redis connection.")
                (when ,run-anyway
                  (,fun))))))))))

(defun ping-redis ()
  "Ping Redis with the configured values. Return \"PONG\" if all was well. Error otherwise."
  (redis:with-connection (:host *qvm-db-host*
                          :port *qvm-db-port*)
    (red:PING)))

;;; Instruction Counting

(defclass profiled-qvm-mixin ()
  ((instructions-executed :initform 0
                          :accessor instructions-executed)))

(defclass profiled-pure-state-qvm (qvm:pure-state-qvm profiled-qvm-mixin)
  ())

(defclass profiled-noisy-qvm (qvm::noisy-qvm profiled-qvm-mixin)
  ())

(defun increment-instruction-counter (n)
  (with-redis (nil)
    (red:INCRBY +instruction-counter-key+ n)))

(defvar *instruction-counter*)

(defmethod qvm:run :around ((qvm profiled-qvm-mixin))
  (let ((*instruction-counter* 0))
    (prog1 (call-next-method)
      (incf (instructions-executed qvm) *instruction-counter*))))

(defmethod qvm:transition :after ((qvm profiled-qvm-mixin) instr)
  (incf *instruction-counter*))

(defun instructions-served ()
  "Get the number of instructions served!"
  (or (with-redis (nil nil)
        (red:GET +instruction-counter-key+))
      -1))
