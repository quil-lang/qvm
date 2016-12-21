;;;; transition.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Transition of the QVM state as a QAM.

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
    (real  (coerce param 'flonum))
    (complex (coerce param 'cflonum))
    (bit-range
     (ecase (bit-range-width param)
       (64  (coerce (classical-double-float qvm param) 'flonum))
       (128 (coerce (classical-complex-double-float qvm param) 'cflonum))))))

(defgeneric transition (qvm instr)
  (:documentation "Execute the instruction INSTR on the QVM.

Return two values:

    1. The resulting (possibly modified) QVM after executing INSTR.

    2. The new value the program counter should be to execute the next
       relevant instruction. If this value is null, then execution
       should be halted."))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:no-operation))
  (declare (ignore instr))
  (values qvm (1+ (pc qvm))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:halt))
  (declare (ignore instr))
  (values qvm nil))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:reset))
  (declare (ignore instr))
  (values (reset qvm) (1+ (pc qvm))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:wait))
  (declare (ignore instr))
  (warn "WAIT executed. Nothing to wait on.")
  (values qvm (1+ (pc qvm))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:pragma))
  (declare (ignore instr))
  (warn "Ignoring PRAGMA: ~A" instr)
  (values qvm (1+ (pc qvm))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:toggle))
  (let* ((address (quil:address-value (quil:toggle-address instr)))
         (b (classical-bit qvm address)))
    (setf (classical-bit qvm address) (- 1 b))
    (values qvm (1+ (pc qvm)))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:unconditional-jump))
  (values qvm (quil:jump-label instr)))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:jump-when))
  (values qvm
          (if (= 1 (classical-bit qvm (quil:address-value
                                       (quil:conditional-jump-address instr))))
              (quil:jump-label instr)
              (1+ (pc qvm)))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:jump-unless))
  (values qvm
          (if (zerop (classical-bit qvm (quil:address-value
                                         (quil:conditional-jump-address instr))))
              (quil:jump-label instr)
              (1+ (pc qvm)))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:measure))
  (values
   (measure qvm
            (permuted-qubit qvm (quil:qubit-index (quil:measurement-qubit instr)))
            (quil:address-value (quil:measure-address instr)))
   (1+ (pc qvm))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:measure-discard))
  (values
   (measure qvm
            (permuted-qubit qvm
                            (quil:qubit-index (quil:measurement-qubit instr)))
            nil)
   (1+ (pc qvm))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:gate-application))
  (let* ((gate (lookup-gate qvm (quil:application-operator instr) :error t))
         (params (mapcar #'quil:constant-value (quil:application-parameters instr)))
         (qubits (mapcar (lambda (q)
                           (permuted-qubit qvm (quil:qubit-index q)))
                         (quil:application-arguments instr)))
         (operator (apply #'gate-operator gate params)))
    (apply-operator (amplitudes qvm) operator (apply #'nat-tuple qubits))
    (values
     qvm
     (1+ (pc qvm)))))

(defmethod transition ((qvm quantum-virtual-machine) (instr quil:swap-application))
  (destructuring-bind (q0 q1) (quil:application-arguments instr)
    (swap-qubits qvm (quil:qubit-index q0) (quil:qubit-index q1))
    (values
     qvm
     (1+ (pc qvm)))))

;;; XXX: Temporary measure. Unresolved applications should error at
;;; parse time.
(defmethod transition ((qvm quantum-virtual-machine) (instr quil::unresolved-application))
  (let* ((gate (lookup-gate qvm (quil:application-operator instr) :error t))
         (params (mapcar #'quil:constant-value (quil:application-parameters instr)))
         (qubits (mapcar (lambda (q)
                           (permuted-qubit qvm (quil:qubit-index q)))
                         (quil:application-arguments instr)))
         (operator (apply #'gate-operator gate params)))
    ;; Do some error checking.
    (let ((given-qubits (length qubits))
          (expected-qubits (1- (integer-length (array-dimension operator 0)))))
      (assert (= given-qubits expected-qubits)
              ()
              "Attempting to apply the ~D-qubit gate ~A to ~D qubit~:P ~
               in the instruction:~2%    ~A"
              expected-qubits
              (gate-name gate)
              given-qubits
              (with-output-to-string (s)
                (quil::print-instruction instr s))))

    ;; Transition the QVM.
    (apply-operator (amplitudes qvm) operator (apply #'nat-tuple qubits))
    (values
     qvm
     (1+ (pc qvm)))))

;;; XXX: This method should not exist in the end.
(defmethod transition ((qvm quantum-virtual-machine) (instr quil::circuit-application))
  (cerror "Ignore circuit."
          "DEFCIRCUIT and circuit expansion is currently not supported.")
  (values
   qvm
   (1+ (pc qvm))))
