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
    :initform (make-hash-table)
    :documentation "Noisy readout encoded as diagonal single qubit
    POVM given as a 4-element list

          (p(0|0) p(0|1)
           p(1|0) p(1|1))

which for each qubit gives the probability p(j|k) of measuring outcome
j given actual state k. Note that we model purely classical readout
error, i.e., the post measurement qubit state is always k, but the
recorded outcome j may be different."))
  (:documentation "A quantum virtual machine with noisy gates and readout."))


(defun make-pauli-noise-map (px py pz)
  "Generates a Kraus map for a noisy identity channel:

  M[rho] = (1-px-py-pz) rho + px X rho X + py Y rho Y + pz Z rho Z.

The arguments PX, PY and PZ can be interpreted as probabilities of a X, Y or Z error affecting the qubit.
"
  (check-type px (real 0 1))
  (check-type py (real 0 1))
  (check-type pz (real 0 1))
  (let* ((psum (+ px py pz))
         (scaled-paulis (loop :for sj :in '("X" "Y" "Z")
                              :for pj :in (list px py pz)
                              :collect (magicl:scale (sqrt pj)
                                                     (quil:gate-matrix (quil:lookup-standard-gate sj))))))
    (check-type psum (real 0 1))
    (when (< psum 1)
      (push (magicl:scale (sqrt (- 1.0 psum)) (magicl:make-identity-matrix 2)) scaled-paulis))
    scaled-paulis))

;; TODO declare inlinable, turn on optimization
(defun perturb-measurement (actual-outcome p00 p01 p10 p11)
  "Given the readout error encoded in the POVM (see documentation of NOISY-QVM)
randomly sample the observed (potentially corrupted) measurement outcome."
  (check-type actual-outcome bit)
  (check-type p00 (double-float 0.0d0 1.0d0))
  (check-type p01 (double-float 0.0d0 1.0d0))
  (check-type p10 (double-float 0.0d0 1.0d0))
  (check-type p11 (double-float 0.0d0 1.0d0))
  (let ((r (random 1.0d0)))
    (ecase actual-outcome
      ((0) (if (<= r p00) 0 1))
      ((1) (if (<= r p01) 0 1)))))

(defun make-pauli-perturbed-1q-gate (gate-name px py pz)
  "Generate a Kraus map that represents a noisy version of the standard gate U identified
by GATE-NAME. The resulting gate is equivalent to I' * U, i.e., the ideal gate U followed by
a noisy identity gate I' as defined in MAKE-PAULI-NOISE-MAP.
"
  (let ((kraus-ops (make-pauli-noise-map px py pz))
        (u (quil:gate-matrix (quil:lookup-standard-gate gate-name))))
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

(defun check-povm (povm)
  "Verify that the list POVM contains a valid single qubit diagonal POVM.
Also see the documentation for the READOUT-POVMS slot of NOISY-QVM."
  (destructuring-bind (p00 p01 p10 p11) povm
    (check-type p00 (double-float 0.0d0 1.0d0))
    (check-type p01 (double-float 0.0d0 1.0d0))
    (check-type p10 (double-float 0.0d0 1.0d0))
    (check-type p11 (double-float 0.0d0 1.0d0))
    (assert (cl-quil::double= 1.0d0 (+ p00 p10)))
    (assert (cl-quil::double= 1.0d0 (+ p01 p11)))))


(defun set-noisy-gate (qvm gate-name qubits kraus-ops)
  "Add noisy gate definition to QVM for a SIMPLE-GATE specified by
GATE-NAME in terms of the Kraus operator representation

   rho -> sum_{j=1}^n K_j rho K_j^H.

The argument KRAUS-OPS should hold the Kraus operators as list of
MAGICL matrices '(K1 K2 ... Kn)."
  (check-type qvm noisy-qvm)
  (check-kraus-ops kraus-ops)
  (setf (gethash (list gate-name qubits) (noisy-gate-definitions qvm)) kraus-ops)
  nil)


(defun set-readout-povm (qvm qubit povm)
  "For a QUBIT belonging to a noisy QVM specify a POVM to encode
possible readout errors.

POVM must be a 4-element list of double-floats.
"
  (check-type qvm noisy-qvm)
  (check-povm povm)
  (setf (gethash qubit (readout-povms qvm)) povm)
  nil)


(defmethod transition ((qvm noisy-qvm) (instr quil:gate-application))
  (let* ((gate (lookup-gate qvm (quil:application-operator instr) :error t))
         (logical-qubits (quil:application-arguments instr))
         (qubits (mapcar #'quil:qubit-index logical-qubits))
         (kraus-ops (gethash (list (quil:gate-name gate) (mapcar #'quil:qubit-index logical-qubits)) (noisy-gate-definitions qvm))))
    (cond
      (kraus-ops
       ;; Found noisy realization of current gate, need to randomly
       ;; select one of several Kraus operators to apply for the
       ;; transition
       (check-type gate quil:static-gate)
       (let ((amps (make-vector (expt 2 (number-of-qubits qvm))))
             (r (random 1.0))
             (summed-p 0.0))
         ;; Randomly select one of the Kraus operators by inverse
         ;; transform sampling (cf [1]): We divide the unit interval
         ;; [0,1] into n bins where the j-th bin size equals the
         ;; probability p_j with which the j-th Kraus operator k_j
         ;; should be applied. We know that the Kraus operators are
         ;; normalized such that p_j = <psi|k_j^H k_j |psi> where x^H
         ;; denotes hermitian conjugation of x and can therefore
         ;; perform this sampling lazily: First generate a uniformly
         ;; sampled random number r in [0,1]. Next, find j such that
         ;;
         ;;       sum_{k=1}^{j-1} p_k < r <= sum_{k=1}^{j} p_k
         ;;
         ;; This is possible by evaluating only all p_k for k<=j. Then
         ;; pick this j as the choice of Kraus operator to apply.
         ;;
         ;; [1]: https://en.wikipedia.org/wiki/Inverse_transform_sampling
         (loop :until (>= summed-p r)
               :for kj :in kraus-ops
               :do
                  ;; initialize amps total the current wavefunction
                  (replace amps (amplitudes qvm))
                  ;; apply the current Kraus operator KJ to amps
                  ;; in-place |AMPS> -> KJ |AMPS>
                  (apply-matrix-operator
                   (magicl-matrix-to-quantum-operator kj)
                   amps
                   (apply #'nat-tuple qubits))
                  ;; compute <AMPS|KJ^H KJ|AMPS> = p_j and increase
                  ;; SUMMED-PY by this value to facilitate randomly
                  ;; selecting one transition
                  (incf summed-p (psum #'probability amps)))
         (replace (amplitudes qvm) amps)
         (setf amps nil)
         (normalize-wavefunction (amplitudes qvm))
         (values
          qvm
          (1+ (pc qvm)))))
      (t
       ;; if we cannot find a noise model for the gate forward args to
       ;; the original (transition ...) call defined for
       ;; the parent class
       (call-next-method qvm instr)))))


(defmethod transition ((qvm noisy-qvm) (instr quil:measure))
  (multiple-value-bind (ret-qvm counter)
      ;; perform actual measurement
      (call-next-method qvm instr)

    ;; randomly corrupt outcome
    (let* ((q (quil:qubit-index (quil:measurement-qubit instr)))
           (a (quil:address-value (quil:measure-address instr)))
           (c (classical-bit ret-qvm a))
           (povm (gethash q (readout-povms ret-qvm))))
      (when povm
        (destructuring-bind (p00 p01 p10 p11) povm
          (setf (classical-bit ret-qvm a)
                (perturb-measurement c p00 p01 p10 p11)))))
    (values
     ret-qvm
     counter)))


(defmethod measure-all ((qvm noisy-qvm))
  (multiple-value-bind (qvm-ret measured-bits)
      (call-next-method qvm)
    (values
     qvm-ret
     ;; Perturb measured-bits: This models purely classical bit flips
     ;; of the measurement record which captures the reality of noisy
     ;; low power dispersive measurements of superconducting qubits
     ;; very well. Here the dominant source of error is misclassifying
     ;; a readout signal due to thermal noise that corrupts the signal
     ;; on its return path out of the cryostat.
     (loop :for i :below (number-of-qubits qvm)
           :for c :in measured-bits
           :collect (let ((povm (gethash i (readout-povms qvm))))
                      (if povm
                          (destructuring-bind (p00 p01 p10 p11) povm
                            (perturb-measurement c p00 p01 p10 p11))
                          c))))))
