;;;; execution.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Execution of the QVM.

(define-condition invalid-gate-invocation (error)
  ((gate-name :initarg :gate-name
              :reader invalid-gate-invocation-gate-name)
   (instruction :initarg :instruction
                :reader unsupported-instruction-full))
  (:documentation "An error raised when a gate is invoked incorrectly.")
  (:report (lambda (condition stream)
             (format stream "Invalid invocation of the gate ~A."
                     (invalid-gate-invocation-gate-name condition)))))

(defun parse-parameter (qvm param)
  "Parse the parameter PARAM in the context of QVM."
  (etypecase param
    (real  (coerce param 'double-float))
    (complex (coerce param '(complex double-float)))
    (bit-range
     (ecase (bit-range-width param)
       (64  (classical-double-float qvm param))
       (128 (classical-complex-double-float qvm param))))))

(defgeneric transition-qvm (qvm instr)
  (:documentation "Execute the instruction INSTR on the QVM.

Return two values:

    1. The resulting (possibly modified) QVM after executing INSTR.

    2. The new value the program counter should be to execute the next
       relevant instruction. If this value is null, then execution
       should be halted."))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:no-operation))
  (values qvm (1+ (pc qvm))))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:halt))
  (values qvm nil))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:reset))
  (values (reset qvm) (1+ (pc qvm))))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:wait))
  (warn "WAIT executed. Nothing to wait on.")
  (values qvm nil))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:toggle))
  (let* ((address (quil:address-value (quil:toggle-address instr)))
         (b (classical-bit qvm address)))
    (setf (classical-bit qvm address) (- 1 b))
    (values qvm (1+ (pc qvm)))))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:unconditional-jump))
  (values qvm (quil:jump-label instr)))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:jump-when))
  (values qvm
          (if (= 1 (classical-bit qvm (quil:address-value
                                       (quil:conditional-jump-address instr))))
              (quil:jump-label instr)
              (1+ (pc qvm)))))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:jump-unless))
  (values qvm
          (if (zerop (classical-bit qvm (quil:address-value
                                         (quil:conditional-jump-address instr))))
              (quil:jump-label instr)
              (1+ (pc qvm)))))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:measure))
  (values
   (measure qvm
            (quil:qubit-index (quil:measure-qubit instr))
            (quil:address-value (quil:measure-address instr)))
   (1+ (pc qvm))))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:measure-discard))
  (values
   (measure qvm (quil:qubit-index (quil:measure-discard-qubit instr)) nil)
   (1+ (pc qvm))))

(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil:gate-application))
  (let* ((gate (lookup-gate qvm (quil:application-operator instr) :error t))
         (params (quil:application-parameters instr))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (operator (apply #'gate-operator gate params)))
    (values
     (apply-operator qvm operator (apply #'nat-tuple qubits))
     (1+ (pc qvm)))))

;;; XXX: Temporary measure.
(defmethod transition-qvm ((qvm quantum-virtual-machine) (instr quil::unresolved-application))
  (let* ((gate (lookup-gate qvm (quil:application-operator instr) :error t))
         (params (mapcar #'quil:constant-value (quil:application-parameters instr)))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (operator (apply #'gate-operator gate params)))
    (values
     (apply-operator qvm operator (apply #'nat-tuple qubits))
     (1+ (pc qvm)))))

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
