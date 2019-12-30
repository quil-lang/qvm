;;;; src/transition.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Transition of the QVM state as a QAM.
;;;
;;; Some of these TRANSITION methods specialize on
;;; CLASSICAL-MEMORY-MIXIN, if there's no need to have quantum bits
;;; participate.

(define-condition invalid-instruction-encountered (error)
  ((instruction :initarg :instruction
                :reader invalid-instruction)
   (reason :initarg :because
           :reader invalid-reason))
  (:default-initargs :because nil)
  (:documentation "An error raised when a gate is invoked incorrectly.")
  (:report (lambda (condition stream)
             (format stream "Encountered the invalid instruction~2%    ")
             (quil:print-instruction (invalid-instruction condition) stream)
             (alexandria:when-let ((reason (invalid-reason condition)))
               (format stream "~2%which could not be executed because ~A" reason)))))

(defgeneric transition (qvm instr)
  (:documentation "Execute the instruction INSTR on the QVM.

Return just the resulting (possibly modified) QVM after executing INSTR. (Historical note: used to also return pc)"))

(defmethod transition :around (qvm instr)
  (cond
    ((not *transition-verbose*) (call-next-method))
    (t
     (let ((start (get-internal-real-time))
           gc-time bytes-alloc)
       (multiple-value-prog1 (measuring-gc (gc-time bytes-alloc) (call-next-method))
         (format *trace-output* "~&; Transition ~A took ~D ms (gc: ~D ms; alloc: ~D bytes)~%"
                 (with-output-to-string (s) (cl-quil:print-instruction instr s))
                 (round (* (/ 1000 internal-time-units-per-second)
                           (- (get-internal-real-time) start)))
                 gc-time
                 bytes-alloc)
         (finish-output *trace-output*))))))

(defmethod transition (qvm (instr quil:unresolved-application))
  (error 'invalid-instruction-encountered
         :instruction instr
         :because (format nil "the operator ~A is not known"
                          (quil::operator-description-root-name
                           (quil:application-operator instr)))))

(defmethod transition ((qvm classical-memory-mixin) (instr quil:no-operation))
  (declare (ignore instr))
  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm classical-memory-mixin) (instr quil:pragma))
  ;; Ignore the pragma. Warn only when we want verbose output.
  (when *transition-verbose*
    (warn "Ignoring PRAGMA: ~A" instr))
  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm classical-memory-mixin) (instr quil:halt))
  (declare (ignore instr))
  (setf (pc qvm) nil)
  qvm)

(defmethod transition ((qvm base-qvm) (instr quil:reset))
  (declare (ignore instr))
  (set-to-zero-state (state qvm))
  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:reset-qubit))
  ;; We have to be careful here. We can't just project out this qubit,
  ;; because that isn't ever something that could happen in real
  ;; life. Instead, we have to do a MEASURE+X gate
  ;; non-deterministically, which is one example of a physically
  ;; realizable process.
  (let ((q (quil:qubit-index (quil:reset-qubit-target instr))))
    ;; Do the measurement: MEASURE q
    (multiple-value-bind (measured-qvm measured-bit)
        (measure qvm q)
      ;; Conditionally do an X.
      (when (= 1 measured-bit)
        (apply-gate-to-state (load-time-value
                              (quil:gate-definition-to-gate
                               (quil:lookup-standard-gate "X"))
                              t)
                             (state measured-qvm)
                             (list q)))
      (setf (pc qvm) (1+ (pc measured-qvm)))
      qvm)))


;;;;;;;;;;;;;;;;;;;; JUMP, JUMP-WHEN, JUMP-UNLESS ;;;;;;;;;;;;;;;;;;;;

(defmethod transition ((qvm classical-memory-mixin) (instr quil:unconditional-jump))
  (setf (pc qvm) (quil:jump-label instr))
  qvm)

(defmethod transition ((qvm classical-memory-mixin) (instr quil:jump-when))
  (cond ((= 1 (dereference-mref qvm (quil:conditional-jump-address instr)))
         (setf (pc qvm) (quil:jump-label instr)))
        (t (incf (pc qvm))))
  qvm)

(defmethod transition ((qvm classical-memory-mixin) (instr quil:jump-unless))
  (cond ((zerop (dereference-mref qvm (quil:conditional-jump-address instr)))
         (setf (pc qvm) (quil:jump-label instr)))
        (t (incf (pc qvm))))
  qvm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MEASURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod transition ((qvm base-qvm) (instr quil:measure))
  (incf (pc qvm))
  (measure-and-store qvm
                     (quil:qubit-index (quil:measurement-qubit instr))
                     (quil:measure-address instr)))

(defmethod transition ((qvm base-qvm) (instr quil:measure-discard))
  (incf (pc qvm))
  (apply-measure-discard-to-state qvm (state qvm) instr)
  qvm)

(defmethod transition ((qvm base-qvm) (instr measure-all))
  (multiple-value-bind (qvm state) (measure-all qvm)
    (loop :for (qubit . address) :in (measure-all-storage instr)
          :do (setf (dereference-mref qvm address) (nth qubit state)))
    (incf (pc qvm))
    qvm))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Gate Application ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The word "force" here is borrowed from the functional programming
;;; world, where a promise or delayed expression may be "forced"
;;; (evaluated as if by applicative order evaluation) into a value.
(defun force-parameter (param qvm)
  "Force evaluation of an application parameter PARAM, with respect to
the specified QVM."
  (etypecase param
    (quil:constant
     (quil:constant-value param))
    (quil::delayed-expression
     (quil:constant-value
      (quil::evaluate-delayed-expression
       param
       (lambda (mref)
         (memory-ref qvm (quil:memory-ref-name mref) (quil:memory-ref-position mref))))))))

(defmethod transition ((qvm base-qvm) (instr quil:gate-application))
  ;; Apply the GATE-APPLICATION INSTR to the QVM. This
  ;; GATE-APPLICATION can either be a superoperator if there are
  ;; SUPEROPERATOR-DEFINITIONS associated with the INSTR, or a QUIL
  ;; gate.
  (let* ((gate   (pull-teeth-to-get-a-gate instr)) 
         (gate-name (quil::application-operator-root-name instr))
         (params (mapcar #'(lambda (p) (force-parameter p qvm))
                         (quil:application-parameters instr)))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         ;; If there are SUPEROPERATOR-DEFINITIONS for INSTR, apply
         ;; those, else, apply the GATE.
         (operator (or (gethash (list gate-name qubits) (superoperator-definitions qvm))
                       gate)))
    ;; Do some error checking.
    (let ((given-qubits (length qubits))
          (expected-qubits (1- (integer-length (quil:gate-dimension gate)))))
      (unless (= given-qubits expected-qubits)
        (error 'invalid-instruction-encountered
               :instruction instr
               :because (format nil "I attempted to apply the ~D-qubit gate to ~D qubit~:P"
                                expected-qubits
                                given-qubits))))
    (apply #'apply-gate-to-state operator (state qvm) qubits params)
    (incf (pc qvm))
    qvm))

(defmethod transition ((qvm pure-state-qvm) (instr compiled-gate-application))
  ;; The instruction itself is a gate.
  (apply-gate-to-state instr (state qvm) nil)
  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr compiled-measurement))
  (let ((bit (funcall (projector-operator instr) (amplitudes qvm)))
        (src (source-instruction instr)))
    (when (typep src 'quil:measure)
      (setf (dereference-mref qvm (quil:measure-address src)) bit)))
  (incf (pc qvm))
  qvm)
