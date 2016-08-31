;;;; execution.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Execution of quantum programs on QVM instances.

(defun run (qvm)
  "Simulate until completion the quantum virtual machine QVM. Return the QVM in its end state."
  (let ((pc 0))
    (loop :until (or (null pc) (>= pc (loaded-program-length qvm))) :do
      (setf (pc qvm) pc)
      (multiple-value-setq (qvm pc)
        (transition-qvm qvm (current-instruction qvm)))))
  qvm)

(defun run-program (num-qubits program &key (classical-memory-size 8))
  "Run the program PROGRAM on a QVM of NUM-QUBITS qubits, with a classical memory size of CLASSICAL-MEMORY-SIZE."
  (let ((qvm (make-qvm num-qubits :classical-memory-size classical-memory-size)))
    (load-program qvm program)
    (run qvm)))
