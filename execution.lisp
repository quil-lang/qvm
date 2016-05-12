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

(define-condition invalid-gate-invocation (error)
  ((gate-name :initarg :gate-name
              :reader invalid-gate-invocation-gate-name)
   (instruction :initarg :instruction
                :reader unsupported-instruction-full))
  (:documentation "An error raised when a gate is invoked incorrectly.")
  (:report (lambda (condition stream)
             (format stream "Invalid invocation of the gate ~A."
                     (invalid-gate-invocation-gate-name condition)))))

(defun run (qvm)
  "Simulate until completion the quantum virtual machine QVM. Return the QVM in its end state."
  ;; If the program is empty then we are done.
  (if (null (program qvm))
      qvm
      ;; Get the next instruction and add it to the history of
      ;; executed instructions.
      (let ((instruction (first (program qvm))))
        (case (first instruction)
          ;; HALT: End execution.
          ((halt)
           qvm)

          ;; WHEN, UNLESS: Conditional branching.
          ((when unless)
           (let ((rest-program (rest (program qvm)))
                 (test-for (ecase (first instruction)
                             ((when) 1)
                             ((unless) 0))))
             (when (= test-for (classical-bit qvm (second instruction)))
               (setf rest-program (append (cddr instruction) rest-program)))
             (setf (program qvm) rest-program)
             (run qvm)))

          ;; WHILE, UNTIL: Looping.
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

          ;; RESET: Reset qubits to 0.
          ((reset)
           (let ((resulting-qvm (reset qvm)))
             (pop (program resulting-qvm))
             (run resulting-qvm)))

          ;; MEASURE: Perform a measurement on qubits.
          ((measure)
           (let ((resulting-qvm (measure qvm
                                         (second instruction)
                                         (third instruction))))
             (pop (program resulting-qvm))
             (run resulting-qvm)))

          ((wait defgate defcircuit include)
           (error 'unsupported-instruction
                  :instruction instruction
                  :opcode (first instruction)))

          ;; Gate/function application.
          (otherwise
           (let ((gate (lookup-gate qvm (first instruction))))
             (cond
               ;; The gate wasn't found. Perform the legacy operation
               ;; of computing the function.
               ((null gate)
                (let ((resulting-qvm
                        (apply (first instruction) (cons qvm (rest instruction)))))
                  (pop (program resulting-qvm))
                  (run resulting-qvm)))

               ;; The gate is defined. Parse out the rest of the
               ;; instruction and apply it.
               (t
                (let ((args (rest instruction)))
                  (if (null args)
                      (error 'invalid-gate-invocation
                             :gate-name (gate-name gate)
                             :instruction instruction)
                      (let (params qubits)
                        ;; Parse out the complex params.
                        (if (listp (first args))
                            (setf params (first args)
                                  qubits (rest args))
                            (setf params nil
                                  qubits args))
                        ;; Do some sanity checking.
                        (assert (every #'integerp qubits))
                        ;; Get the gate operator and apply it.
                        (let* ((operator (apply #'gate-operator gate params))
                               (resulting-qvm (apply-operator qvm operator (apply #'nat-tuple qubits))))
                          (pop (program resulting-qvm))
                          (run resulting-qvm)))))))))))))

(defun run-program (num-qubits program &key (classical-memory-size 8))
  "Run the program PROGRAM on a QVM of NUM-QUBITS qubits, with a classical memory size of CLASSICAL-MEMORY-SIZE."
  (let ((qvm (make-qvm num-qubits :classical-memory-size classical-memory-size)))
    (load-program qvm program)
    (run qvm)))
