;;;; execution.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Execution of the QVM.

(defun run (qvm)
  "Simulate until completion the quantum virtual machine QVM. Return the QVM in its end state."
  ;; If the program is empty or if there was a zero probability of
  ;; achieving this state, then bail out.
  (if (null (program qvm))
      qvm
      ;; Get the next instruction and add it to the history of
      ;; executed instructions.
      (let ((instruction (first (program qvm))))
        (case (first instruction)
          ;; HALT: End execution.
          ((halt)
           qvm)

          ((when)
           (let ((rest-program (rest (program qvm))))
             (when (= 1 (classical-bit qvm (second instruction)))
               (setf rest-program (append (cddr instruction) rest-program)))
             (setf (program qvm) rest-program)
             (run qvm)))

          ((unless)
           (let ((rest-program (rest (program qvm))))
             (unless (= 1 (classical-bit qvm (second instruction)))
               (setf rest-program (append (cddr instruction) rest-program)))
             (setf (program qvm) rest-program)
             (run qvm)))

          (otherwise
           (let ((resulting-qvm
                   (apply (first instruction) (cons qvm (rest instruction)))))
             (setf (program resulting-qvm) (rest (program resulting-qvm)))
             (run resulting-qvm)))))))

(defun run-program (num-qubits program &key (classical-memory-size 8))
  "Run the program PROGRAM on a QVM of NUM-QUBITS qubits, with a classical memory size of CLASSICAL-MEMORY-SIZE."
  (let ((qvm (make-qvm num-qubits :classical-memory-size classical-memory-size)))
    (load-program qvm program)
    (run qvm)))
