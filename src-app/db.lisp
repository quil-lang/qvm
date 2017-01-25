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

(defmacro with-redis ((&optional (run-anyway nil)) &body body)
  "Execute BODY, attempting to make a Redis connection around it. If there is a failure in making a Redis connection, then ignore the failure and execute BODY anyway only if RUN-ANYWAY is true."
  (alexandria:with-gensyms (fun c)
    (alexandria:once-only (run-anyway)
      `(flet ((,fun () ,@body))
         (cond
           ((not (and *qvm-db-host* *qvm-db-port*))
            (when ,run-anyway
              (,fun)))
           (t
            (handler-case (redis:with-connection (:host *qvm-db-host*
                                                  :port *qvm-db-port*)
                            (redis:with-pipelining
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

(defvar *instruction-counter*)

(defmethod qvm:run :around ((qvm qvm:quantum-virtual-machine))
  (let ((*instruction-counter* 0))
    (prog1 (call-next-method)
      (with-redis (nil)
        (red:INCRBY +instruction-counter-key+ *instruction-counter*)))))

(defmethod qvm:transition :after ((qvm qvm:quantum-virtual-machine) instr)
  (incf *instruction-counter*))
