#!/usr/local/bin/sbcl --script
(load "~/.sbclrc")

(require :sb-sprof)
(ql:quickload :qvm)
(ql:quickload :cl-quil)

(defparameter *quil-file* "H6.quil")
(defvar *quil* nil)


(format t "PARSING========================================~%")
(sb-sprof:with-profiling (:max-samples 1000
                          :report :graph
                          :loop t
                          :reset t)
  (setq *quil*
        (let ((cl-quil::*allow-unresolved-applications* t))
          (cl-quil:read-quil-file *quil-file*))))

(format t "RUNNING========================================~%")
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
  #+ignore
  (sb-sprof:with-profiling (:max-samples 10000
                            :report :graph
                            :mode :alloc
                            :loop t
                            :reset t)
    (qvm:run q)
    (setf (qvm::pc q) 0)))
