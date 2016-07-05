#!/usr/local/bin/sbcl --script
(load "~/.sbclrc")

(require :sb-sprof)
(ql:quickload :qvm)
(ql:quickload :cl-quil)

(defparameter *quil*
  (let ((cl-quil::*allow-unresolved-applications* t))
    (cl-quil:read-quil-file "qaoa_8q.quil")))

(let ((q (qvm:make-qvm (cl-quil:qubits-needed *quil*))))
  (qvm:load-program q *quil*)
  (sb-sprof:with-profiling (:max-samples 1000
                            :report :graph
                            :loop t
                            :reset t)
    (qvm:run q)
    (setf (qvm::pc q) 0))

  (qvm::reset q)
  (setf (qvm::pc q) 0)
  
  (sb-sprof:with-profiling (:max-samples 10000
                            :report :graph
                            :mode :alloc
                            :loop t
                            :reset t)
    (qvm:run q)
    (setf (qvm::pc q) 0)))
