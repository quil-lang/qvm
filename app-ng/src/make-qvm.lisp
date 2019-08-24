;;;; src/make-qvm.lisp
;;;;
;;;; Author: appleby

(in-package #:qvm-app-ng)

(defun run-program-on-qvm (qvm parsed-program)
  ;; Like QVM:RUN-PROGRAM, but bring your own QVM.
  (qvm:load-program qvm parsed-program :supersede-memory-subsystem t)
  (qvm:run qvm))

(defun make-requested-qvm (simulation-method num-qubits)
  (check-non-negative num-qubits)
  (ecase simulation-method
    (pure-state
     (qvm:make-qvm num-qubits))
    (full-density-matrix
     (qvm:make-density-qvm num-qubits))))
