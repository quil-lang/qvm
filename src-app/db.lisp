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

(defmacro with-redis (&body body)
  "Execute BODY, attempting to make a Redis connection around it. If there is a failure in making a Redis connection, then ignore the failure and execute BODY anyway."
  (let ((fun (gensym "BODY-"))
        (c   (gensym "C")))
    `(flet ((,fun () ,@body))
       (cond
         ((not (and *qvm-db-host* *qvm-db-port*))
          (,fun))
         (t
          (handler-case (redis:with-connection (:host *qvm-db-host*
                                                :port *qvm-db-port*)
                          (redis:with-pipelining
                            (,fun)))
            (usocket:connection-refused-error (,c)
              (declare (ignore ,c))
              (warn "Connection refused, continuing without Redis connection.")
              (,fun))
            (usocket:host-unreachable-error (,c)
              (declare (ignore ,c))
              (warn "Host unreachable, continuing without Redis connection.")
              (,fun))))))))

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
      (with-redis
        (when (and *qvm-db-host* *qvm-db-port*)
          (red:INCRBY +instruction-counter-key+ *instruction-counter*))))))

(defmethod qvm:transition :after ((qvm qvm:quantum-virtual-machine) instr)
  (incf *instruction-counter*))
