#!/usr/local/bin/sbcl --script
(require :asdf)
(defparameter *quil-file* (first (uiop:command-line-arguments)))
(format t "Benchmarking quil file ~S~%" *quil-file*)
(load "~/.sbclrc")

(require :sb-sprof)
(ql:quickload '(:qvm :cl-quil) :silent t)

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
  (qvm:load-program q *quil* :supersede-memory-subsystem t)
  (sb-sprof:with-profiling (:max-samples 1000
                            :report :graph
                            :loop t
                            :reset t)
    (qvm:run q)
    (setf (qvm::pc q) 0))

  (qvm::reset-quantum-state q)
  (qvm::reset-classical-memory q)
  (setf (qvm::pc q) 0)
  #+ignore
  (sb-sprof:with-profiling (:max-samples 10000
                            :report :graph
                            :mode :alloc
                            :loop t
                            :reset t)
    (qvm:run q)
    (setf (qvm::pc q) 0)))
