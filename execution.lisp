;;;; execution.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Execution of the QVM.

(define-condition unsupported-instruction (error)
  ((opcode :initarg :opcode
           :reader unsupported-instruction-opcode)
   (instruction :initarg :instruction
                :reader unsupported-instruction-full))
  (:documentation "An error raised when an unsupported or unimplemented instruction is encountered.")
  (:report (lambda (condition stream)
             (format stream "Unsupported instruction opcode ~A encountered."
                     (unsupported-instruction-opcode condition)))))

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

          ((when unless)
           (let ((rest-program (rest (program qvm)))
                 (test-for (ecase (first instruction)
                             ((when) 1)
                             ((unless) 0))))
             (when (= test-for (classical-bit qvm (second instruction)))
               (setf rest-program (append (cddr instruction) rest-program)))
             (setf (program qvm) rest-program)
             (run qvm)))

          ((while until)
           (let ((rest-program (rest (program qvm)))
                 (test-index (second instruction))
                 (loop-body (cddr instruction))
                 (test-for (ecase (first instruction)
                             ((while) 1)
                             ((until) 0))))
             ;; Loop, constantly resetting the program to the loop
             ;; body.
             (loop :while (= test-for (classical-bit qvm test-index)) :do
               (setf (program qvm) loop-body)
               (run qvm))

             ;; Loop finished. Continue executing the rest of the
             ;; program.
             (setf (program qvm) rest-program)
             (run qvm)))

          ((wait defgate defcircuit)
           (error 'unsupported-instruction
                  :instruction instruction
                  :opcode (first instruction)))

          (otherwise
           (let ((resulting-qvm
                   (apply (first instruction) (cons qvm (rest instruction)))))
             (pop (program resulting-qvm))
             (run resulting-qvm)))))))

(defun run-program (num-qubits program &key (classical-memory-size 8))
  "Run the program PROGRAM on a QVM of NUM-QUBITS qubits, with a classical memory size of CLASSICAL-MEMORY-SIZE."
  (let ((qvm (make-qvm num-qubits :classical-memory-size classical-memory-size)))
    (load-program qvm program)
    (run qvm)))
