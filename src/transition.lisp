;;;; src/transition.lisp
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
    (real (coerce param 'flonum))
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

(defmethod transition :around (qvm instr)
  (cond
    ((not *transition-verbose*) (call-next-method))
    (t
     (let ((start (get-internal-real-time))
           gc-time bytes-alloc)
       (multiple-value-prog1 (measuring-gc (gc-time bytes-alloc) (call-next-method))
         (format *trace-output* "~&; Transition ~A took ~D ms (gc: ~D ms; alloc: ~D bytes)~%"
                 (with-output-to-string (s) (cl-quil::print-instruction instr s))
                 (* (/ 1000 internal-time-units-per-second)
                    (- (get-internal-real-time) start))
                 gc-time
                 bytes-alloc)
         (finish-output *trace-output*))))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:no-operation))
  (declare (ignore instr))
  (values qvm (1+ (pc qvm))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:halt))
  (declare (ignore instr))
  (values qvm nil))

(defmethod transition ((qvm pure-state-qvm) (instr quil:reset))
  (declare (ignore instr))
  (values (reset qvm) (1+ (pc qvm))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:wait))
  (declare (ignore instr))
  (warn "WAIT executed. Nothing to wait on.")
  (values qvm (1+ (pc qvm))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:pragma))
  (warn "Ignoring PRAGMA: ~A" instr)
  (values qvm (1+ (pc qvm))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-true))
  (let ((address (quil:address-value (quil:classical-target instr))))
    (setf (classical-bit qvm address) 1)
    (values qvm (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-false))
  (let ((address (quil:address-value (quil:classical-target instr))))
    (setf (classical-bit qvm address) 0)
    (values qvm (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-not))
  (let* ((address (quil:address-value (quil:classical-target instr)))
         (b (classical-bit qvm address)))
    (setf (classical-bit qvm address) (- 1 b))
    (values qvm (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-and))
  (let ((left   (quil:address-value (quil:classical-left-operand instr)))
        (target (quil:address-value (quil:classical-target instr))))
    (setf (classical-bit qvm target) (logand (classical-bit qvm left)
                                             (classical-bit qvm target)))
    (values qvm (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-or))
  (let ((left   (quil:address-value (quil:classical-left-operand instr)))
        (target (quil:address-value (quil:classical-target instr))))
    (setf (classical-bit qvm target) (logior (classical-bit qvm left)
                                             (classical-bit qvm target)))
    (values qvm (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-move))
  (let ((left   (quil:address-value (quil:classical-left-operand instr)))
        (target (quil:address-value (quil:classical-target instr))))
    (setf (classical-bit qvm target) (classical-bit qvm left))
    (values qvm (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-exchange))
  (let ((left  (quil:address-value (quil:classical-left-operand instr)))
        (right (quil:address-value (quil:classical-right-operand instr))))
    (rotatef (classical-bit qvm left) (classical-bit qvm right))
    (values qvm (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:unconditional-jump))
  (values qvm (quil:jump-label instr)))

(defmethod transition ((qvm pure-state-qvm) (instr quil:jump-when))
  (values qvm
          (if (= 1 (classical-bit qvm (quil:address-value
                                       (quil:conditional-jump-address instr))))
              (quil:jump-label instr)
              (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:jump-unless))
  (values qvm
          (if (zerop (classical-bit qvm (quil:address-value
                                         (quil:conditional-jump-address instr))))
              (quil:jump-label instr)
              (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:measure))
  (values
   (measure qvm
            (permuted-qubit qvm (quil:qubit-index (quil:measurement-qubit instr)))
            (quil:address-value (quil:measure-address instr)))
   (1+ (pc qvm))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:measure-discard))
  (values
   (measure qvm
            (permuted-qubit qvm
                            (quil:qubit-index (quil:measurement-qubit instr)))
            nil)
   (1+ (pc qvm))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:gate-application))
  (let ((gate (lookup-gate qvm (quil:application-operator instr) :error t))
        (params (mapcar #'quil:constant-value (quil:application-parameters instr)))
        (qubits (mapcar (lambda (q)
                          (permuted-qubit qvm (quil:qubit-index q)))
                        (quil:application-arguments instr))))
    ;; Do some error checking.
    (let ((given-qubits (length qubits))
          (expected-qubits (1- (integer-length (quil:gate-dimension gate)))))
      (assert (= given-qubits expected-qubits)
              ()
              "Attempting to apply the ~D-qubit gate ~A to ~D qubit~:P ~
               in the instruction:~2%    ~A"
              expected-qubits
              (quil:gate-name gate)
              given-qubits
              (with-output-to-string (s)
                (quil::print-instruction instr s))))

    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
    (values
     qvm
     (1+ (pc qvm)))))

(defmethod transition ((qvm pure-state-qvm) (instr quil:swap-application))
  (destructuring-bind (q0 q1) (quil:application-arguments instr)
    (swap-qubits qvm (quil:qubit-index q0) (quil:qubit-index q1))
    (values
     qvm
     (1+ (pc qvm)))))

;;; XXX: Temporary measure. Unresolved applications should error at
;;; parse time.
(defmethod transition ((qvm pure-state-qvm) (instr quil::unresolved-application))
  (let ((gate (lookup-gate qvm (quil:application-operator instr) :error t))
        (params (mapcar #'quil:constant-value (quil:application-parameters instr)))
        (qubits (mapcar (lambda (q)
                          (permuted-qubit qvm (quil:qubit-index q)))
                        (quil:application-arguments instr))))
    ;; Do some error checking.
    (let ((given-qubits (length qubits))
          (expected-qubits (1- (integer-length (quil:gate-dimension gate)))))
      (assert (= given-qubits expected-qubits)
              ()
              "Attempting to apply the ~D-qubit gate ~A to ~D qubit~:P ~
               in the instruction:~2%    ~A"
              expected-qubits
              (quil:gate-name gate)
              given-qubits
              (with-output-to-string (s)
                (quil::print-instruction instr s))))

    (apply #'apply-gate gate (amplitudes qvm) (apply #'nat-tuple qubits) params)
    (values
     qvm
     (1+ (pc qvm)))))

;;; XXX: This method should not exist after Quil is properly
;;; processed.
(defmethod transition ((qvm pure-state-qvm) (instr quil::circuit-application))
  (cerror "Ignore circuit."
          "DEFCIRCUIT and circuit expansion is currently not supported.")
  (values
   qvm
   (1+ (pc qvm))))
