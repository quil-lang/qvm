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

(defun check-kraus-ops (kraus-ops)
  "Verify that a list KRAUS-OPS of Kraus operators given as MAGICL:MATRIX objects encodes a proper
Kraus map. In particular, we require that the Kraus operators are all of equal matrix dimension with equal numbers
of rows and columns. Furthermore, to ensure that the Kraus map preserves trace, they must be normalized such that

  sum_{j=1}^n K_j^H K_j = I

where I is the identity matrix of equal dimensions."
  (let* ((m (magicl:matrix-rows (first kraus-ops)))
         (n (magicl:matrix-cols (first kraus-ops)))
         (kraus-sum (magicl:make-zero-matrix m n)))
    (assert (= m n) ((first kraus-ops)) "The Kraus operators be square matrices.")
    (loop :for k :in kraus-ops
          :do
             (assert (= m (magicl:matrix-rows k) (magicl:matrix-cols k))
                     (k)
                     "All Kraus operators must have matching dimensions")
             ;; This MAGICL provided BLAS:ZGEMM call effectively performs the following operation
             ;; KRAUS-SUM -> KRAUS-SUM + K^H . K
             (magicl.blas-cffi:%zgemm
              "C" "N" m m m
              (complex 1d0) (magicl::matrix-data k) m (magicl::matrix-data k) m
              (complex 1d0) (magicl::matrix-data kraus-sum) m))

    ;; Warning, if this consistently leads to assertion errors increase the
    ;; tolerance *DEFAULT-ZERO-COMPARISON-EPSILON*
    (let ((magicl::*default-zero-comparison-epsilon* 1d-5))
      (assert
       (magicl:identityp kraus-sum)
       (kraus-sum)
       "The Kraus map must preserve trace or equivalently this matrix ~
        ~S must be equal to the identity" kraus-sum))) t)


(defgeneric set-noisy-gate (qvm gate-name qubits kraus-ops)
  (:documentation "Add noisy gate definition to QVM for a SIMPLE-GATE specified by
GATE-NAME in terms of the Kraus operator representation

   rho -> sum_{j=1}^n K_j rho K_j^H.

The argument KRAUS-OPS should hold the Kraus operators as list of
MAGICL matrices '(K1 K2 ... Kn)."))


(defmethod set-noisy-gate ((qvm noisy-qvm) gate-name qubits kraus-ops)
  (check-kraus-ops kraus-ops)
  (setf (gethash (list gate-name qubits) (noisy-gate-definitions qvm)) kraus-ops)
  nil)

(defgeneric set-readout-povm (qvm qubit povm)
  (:documentation "For a QUBIT belonging to a QVM specify a POVM to encode
possible readout errors.

POVM must be a 4-element list of double-floats."))

(defmethod set-readout-povm ((qvm noisy-qvm) qubit povm)
  (check-povm povm)
  (setf (gethash qubit (readout-povms qvm)) povm)
  nil)

(defmethod run :after ((qvm noisy-qvm))
  ;; Only copy if we really need to.
  (when (requires-swapping-amps-p (state qvm))
    (swap-internal-amplitude-pointers (state qvm))))


(defmethod transition ((qvm noisy-qvm) (instr quil:gate-application))
  (assert (typep (quil:application-operator instr) 'quil:named-operator)
          (instr)
          "The noisy QVM doesn't support gate modifiers.")
  (let* ((gate-name (quil::operator-description-name (quil:application-operator instr)))
         (gate (pull-teeth-to-get-a-gate instr))
         (logical-qubits (quil:application-arguments instr))
         (qubits (mapcar #'quil:qubit-index logical-qubits))
         (kraus-ops (gethash (list gate-name (mapcar #'quil:qubit-index logical-qubits)) (noisy-gate-definitions qvm))))
    (cond
      (kraus-ops
       ;; Found noisy realization of current gate, need to randomly
       ;; select one of several Kraus operators to apply for the
       ;; transition
       (check-type gate quil:static-gate)
       (apply-noise-to-state kraus-ops (state qvm) qubits)
       (incf (pc qvm))
       qvm)
      (t
       ;; if we cannot find a noise model for the gate forward args to
       ;; the original (transition ...) call defined for
       ;; the parent class
       (call-next-method qvm instr)))))


(defgeneric apply-classical-readout-noise (qvm instr)
  (:documentation "Given a QVM and a (measurement) instruction INSTR, corrupt the readout bit according to the POVM specifications of QVM.")
  ;; Make sure readout noise is never applied to a pure-state qvm
  (:method ((qvm pure-state-qvm) (instr quil:measurement))
    (declare (ignore qvm instr))
    nil)
  ;; Readout noise only happens to the resulting classical bit (i.e.,
  ;; it's classical noise). As such, discarding that bit doesn't
  ;; warrant any sort of special treatment.
  (:method ((qvm noisy-qvm) (instr quil:measure-discard))
    (declare (ignore qvm instr))
    nil)
  ;; We do have a readout bit, and we want to corrupt it.
  (:method ((qvm noisy-qvm) (instr quil:measure))
    (%corrupt-qvm-memory-with-povm qvm instr (readout-povms qvm)))
  ;; For compiled measurements, refer to the source of that
  ;; instruction.
  (:method ((qvm noisy-qvm) (instr compiled-measurement))
    (apply-classical-readout-noise qvm (source-instruction instr))))


(defmethod transition :around ((qvm noisy-qvm) (instr quil:measurement))
  ;; perform actual measurement
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))

(defun perturb-measured-bits (qvm measured-bits)
  "Randomly perturb the values of the bits in MEASURED-BITS in
accordance with any available readout POVMs on the QVM. Returns an
updated list of measured bits."
  ;; This models purely classical bit flips of the measurement record
  ;; which captures the reality of noisy low power dispersive
  ;; measurements of superconducting qubits very well. Here the
  ;; dominant source of error is misclassifying a readout signal due
  ;; to thermal noise that corrupts the signal on its return path out
  ;; of the cryostat.
  (loop :for i :below (number-of-qubits qvm)
        :for c :in measured-bits
        :collect (let ((povm (gethash i (readout-povms qvm))))
                   (if povm
                       (destructuring-bind (p00 p01 p10 p11) povm
                         (perturb-measurement c p00 p01 p10 p11))
                       c))))

(defmethod measure-all ((qvm noisy-qvm))
  (declare (ignore qvm))
  (multiple-value-bind (qvm-ret measured-bits)
      (call-next-method)
    (values
     qvm-ret
     (perturb-measured-bits qvm-ret measured-bits))))

;;; Don't compile things for the noisy-qvm.
(defmethod compile-loaded-program ((qvm noisy-qvm))
  qvm)

(defmethod compile-instruction ((qvm noisy-qvm) isn)
  (declare (ignore qvm))
  isn)
