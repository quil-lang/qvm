;;;; noisy-qvm.lisp
;;;;
;;;; Author: Nikolas Tezak

(in-package #:qvm)

(defclass noisy-qvm (pure-state-qvm)
  ((noisy-gate-definitions
    :initarg :noisy-gate-definitions
    :accessor noisy-gate-definitions
    :initform (make-hash-table :test 'equalp)
    :documentation "Noisy gate definitions that, if present, override those stored in GATE-DEFINITIONS.")
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table :test 'eql)
    :documentation "Noisy readout encoded as diagonal single qubit
    POVM given as a 4-element list

          (p(0|0) p(0|1)
           p(1|0) p(1|1))

which for each qubit gives the probability p(j|k) of measuring outcome
j given actual state k. Note that we model purely classical readout
error, i.e., the post measurement qubit state is always k, but the
recorded outcome j may be different.")))

(defmethod initialize-instance :after ((qvm noisy-qvm) &rest args)
  ;; If any noisy gates are defined on the QVM, allocate trial
  ;; amplitudes for the state evolution computations.
  (declare (ignore args))
  (check-all-kraus-ops (alexandria:hash-table-values (noisy-gate-definitions qvm)))
  (check-all-povms (alexandria:hash-table-values (readout-povms qvm)))
  (when (plusp (hash-table-count (noisy-gate-definitions qvm)))
    (check-allocate-computation-space (state qvm))))

(defmethod amplitudes ((qvm noisy-qvm))
  (amplitudes (state qvm)))

(defmethod %trial-amplitudes ((qvm noisy-qvm))
  (%trial-amplitudes (state qvm)))

(defmethod (setf amplitudes) (new-amplitudes (qvm noisy-qvm))
  (setf (amplitudes (state qvm)) new-amplitudes))

(defun make-pauli-noise-map (px py pz)
  "Generates a Kraus map for a noisy identity channel:

  M[rho] = (1-px-py-pz) rho + px X rho X + py Y rho Y + pz Z rho Z.

The arguments PX, PY and PZ can be interpreted as probabilities of a X, Y or Z error affecting the qubit.
"
  (check-type px (real 0 1))
  (check-type py (real 0 1))
  (check-type pz (real 0 1))
  (let* ((psum (+ px py pz))
         (scaled-paulis
           (loop :for sj :in '("X" "Y" "Z")
                 :for pj :in (list px py pz)
                 :collect (magicl:scale (sqrt pj)
                                        (quil:gate-matrix
                                         (quil:gate-definition-to-gate
                                          (quil:lookup-standard-gate sj)))))))
    (check-type psum (real 0 1))
    (when (< psum 1)
      (push (magicl:scale (sqrt (- 1.0 psum)) (magicl:make-identity-matrix 2)) scaled-paulis))
    scaled-paulis))

(defun make-pauli-perturbed-1q-gate (gate-name px py pz)
  "Generate a Kraus map that represents a noisy version of the standard gate U identified
by GATE-NAME. The resulting gate is equivalent to I' * U, i.e., the ideal gate U followed by
a noisy identity gate I' as defined in MAKE-PAULI-NOISE-MAP.
"
  (let ((kraus-ops (make-pauli-noise-map px py pz))
        (u (quil:gate-matrix
            (quil:gate-definition-to-gate
             (quil:lookup-standard-gate gate-name)))))
    (mapcar (lambda (v) (magicl:multiply-complex-matrices v u)) kraus-ops)))

(defmethod set-noisy-gate ((qvm noisy-qvm) gate-name qubits kraus-ops)
  (check-kraus-ops kraus-ops)
  (check-allocate-computation-space (state qvm))
  (setf (gethash (list gate-name qubits) (noisy-gate-definitions qvm)) kraus-ops)
  nil)

(defmethod set-readout-povm ((qvm noisy-qvm) qubit povm)
  (check-povm povm)
  (setf (gethash qubit (readout-povms qvm)) povm)
  nil)

(defmethod transition ((qvm noisy-qvm) (instr quil:gate-application))
  (assert (typep (quil:application-operator instr) 'quil:named-operator)
          (instr)
          "The noisy QVM doesn't support gate modifiers.")
  (let* ((gate-name (quil::operator-description-name (quil:application-operator instr)))
         (gate (pull-teeth-to-get-a-gate instr))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (kraus-ops (gethash (list gate-name qubits) (noisy-gate-definitions qvm))))
    (cond
      (kraus-ops
       ;; Found noisy realization of current gate, need to randomly
       ;; select one of several Kraus operators to apply for the
       ;; transition
       (check-type gate quil:static-gate)
       (apply-gate-to-state (convert-to-kraus-list kraus-ops) (state qvm) qubits)
       (incf (pc qvm))
       qvm)
      (t
       ;; if we cannot find a noise model for the gate forward args to
       ;; the original (transition ...) call defined for
       ;; the parent class
       (call-next-method qvm instr)))))

(defmethod transition :around ((qvm noisy-qvm) (instr quil:measurement))
  ;; perform actual measurement
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))

(defmethod apply-classical-readout-noise ((qvm noisy-qvm) (instr quil:measure-discard))
  ;; Readout noise only happens to the resulting classical bit (i.e.,
  ;; it's classical noise). As such, discarding that bit doesn't
  ;; warrant any sort of special treatment.
  (declare (ignore qvm instr))
  nil)

(defmethod apply-classical-readout-noise ((qvm noisy-qvm) (instr quil:measure))
  ;; We do have a readout bit, and we want to corrupt it.
  (%corrupt-qvm-memory-with-povm qvm instr (readout-povms qvm)))

(defmethod apply-classical-readout-noise ((qvm noisy-qvm) (instr compiled-measurement))
  ;; For compiled measurements, refer to the source of that
  ;; instruction.
  (apply-classical-readout-noise qvm (source-instruction instr)))

(defmethod measure-all ((qvm noisy-qvm))
  (declare (ignore qvm))
  (multiple-value-bind (qvm-ret measured-bits)
      (call-next-method)
    (values
     qvm-ret
     (perturb-measured-bits qvm-ret measured-bits (readout-povms qvm)))))

;;; Don't compile things for the noisy-qvm.
(defmethod compile-loaded-program ((qvm noisy-qvm))
  qvm)

(defmethod compile-instruction ((qvm noisy-qvm) isn)
  (declare (ignore qvm))
  isn)
