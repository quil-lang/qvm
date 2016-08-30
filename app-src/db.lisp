;;;; app-src/db.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(alexandria:define-constant +instruction-counter-key+ "instr_counter"
  :test #'string=
  :documentation "The database key to store the number of instructions executed.")

(defvar *qvm-db-host* #(127 0 0 1)
  "The hostname of the QVM stats DB.")

(defvar *qvm-db-port* 6379
  "The port of the QVM stats DB.")

(defmacro with-redis (&body body)
  "Execute BODY, attempting to make a Redis connection around it. If there is a failure in making a Redis connection, then ignore the failure and execute BODY anyway."
  (let ((fun (gensym "BODY-"))
        (c   (gensym "C")))
    `(flet ((,fun () ,@body))
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
           (,fun))))))

(defmethod qvm::transition-qvm :after ((qvm qvm:quantum-virtual-machine) instr)
  (unless (null redis:*connection*)
    (red:INCR +instruction-counter-key+)))
