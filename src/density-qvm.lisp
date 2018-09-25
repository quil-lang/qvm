;;;; src/density-qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file implements efficient density matrix evolution.


(defclass density-qvm (pure-state-qvm)
  ((temporary-state :accessor temporary-state
                    :initform nil
                    :documentation "Sometimes the simulation needs to use some temporary state. We save it so we don't have to alloc/dealloc frequently."))
  (:documentation "A density matrix simulator."))

;;; Creation and Initialization


;;; TODO XXX: the density matrix should have only real entries
;;; (except in intermediate computations)
(defun make-density-qvm (num-qubits)
  ;; The amplitudes actually represent the entries of the density
  ;; matrix in row-major order. For a system of N qubits, the
  ;; density matrix is 2^N x 2^N, hence a total of 2^(2N) entries.
  (let ((density-matrix (make-vector (expt 2 (* 2 num-qubits)))))
    ;; It just so happens that the pure, zero state is the same in
    ;; this formalism, i.e., a 1 in the first entry.
    (bring-to-zero-state density-matrix)
    (make-instance 'density-qvm
                   :number-of-qubits num-qubits
                   :amplitudes density-matrix)))



(adt:defdata superoperator
  "Representation of a linear operator on density operators."
  ;; Let ' mean † aka conjugate transpose.
  ;;
  ;; ρ ↦ U ρ U'
  (plain-unitary quil:gate)
  ;; ρ ↦ A ρ B
  (left-right quil:gate quil:gate)
  ;; ρ ↦ ∑ᵢ Aᵢ ρ Bᵢ
  (linear-combo list))

(defmethod apply-superoperator (sop vec-density qubits ghost-qubits &key temporary-storage)
  "Apply a superoperator to a vectorized density matrix."
  (check-type sop superoperator)
  ;; We use the following law to help our calculation:
  ;;
  ;;     vec(AρB) = (A ⊗ Bᵀ)vec(ρ)
  ;;
  ;; where vec(·) is row-major vectorization. The quantity vec(ρ) is
  ;; the VEC-DENSITY argument.
  (adt:match superoperator sop
    ((plain-unitary U)
     ;; (U ⊗ U'ᵀ) = (U ⊗ U*), where * is entrywise conjugate.
     (let ((U* (make-instance 'quil:simple-gate
                              :name (concatenate 'string (quil:gate-name U) "*")
                              :dimension (quil:gate-dimension U)
                              :matrix (magicl:conjugate-entrywise (quil:gate-matrix U)))))
       (apply-gate U* vec-density qubits)
       (apply-gate U  vec-density ghost-qubits)
       (values vec-density temporary-storage)))

    ((left-right A B)
     (let ((BT (make-instance 'quil:simple-gate
                              :name (concatenate 'string (quil:gate-name B) "T")
                              :dimension (quil:gate-dimension B)
                              :matrix (magicl:transpose (quil:gate-matrix B)))))
       (apply-gate BT vec-density qubits)
       (apply-gate A  vec-density ghost-qubits)
       (values vec-density temporary-storage)))

    ((linear-combo list)
     (cond
       ;; Empty. Just treat as identity.
       ((endp list)
        (values vec-density temporary-storage))
       ;; Degenerate case of just 1 superoperator.
       ((endp (rest list))
        (apply-superoperator (first list) vec-density qubits ghost-qubits :temporary-storage temporary-storage))
       ;; General (and super expensive) case.
       (t
        ;; XXX FIXME: We could eliminate one copy if our APPLY-GATE
        ;; function could understand a source and destination array.
        (let ((pristine (copy-seq vec-density))
              (sum      (fill (or temporary-storage (copy-seq vec-density)) (cflonum 0))))
          (dolist (sub-sop list)
            ;; Apply the operator.
            (apply-superoperator sub-sop vec-density qubits ghost-qubits)
            ;; Increment our running sum.
            (map-into sum #'+ sum vec-density)
            ;; Reset vec-density to a pristine state.
            ;;
            ;; XXX FIXME: Note that on the last loop, this is
            ;; wasteful!
            (replace vec-density pristine))
          ;; Replace our vec-density with our computed map.
          (replace vec-density sum)
          ;; Let pristine be wild and free for the GC to catch.
          (setf pristine nil)
          ;; Return our purchase, including temporary storage we've
          ;; allocated.
          (values vec-density sum)))))))



(defmethod transition ((qvm density-qvm) (instr quil:gate-application))
  (assert (typep (quil:application-operator instr) 'quil:named-operator)
          (instr)
          "The noisy QVM doesn't support gate modifiers.")
  (let*  ((gate (pull-teeth-to-get-a-gate instr))
          (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
          (ghosts (mapcar (alexandria:curry #'+ (number-of-qubits qvm)) qubits)))

    ;; TODO XXX handle parametric gates or at least error out

    ;; Normalize the gate to a superoperator.
    (unless (typep gate 'superoperator)
      (setf gate (plain-unitary gate)))

    ;; Do the superoperator application.
    (multiple-value-bind (new-density temp-storage)
        (apply-superoperator gate
                             (amplitudes qvm)
                             (apply #'nat-tuple qubits)
                             (apply #'nat-tuple ghosts)
                             :temporary-storage (temporary-state qvm))
      (declare (ignore new-density))
      (setf (temporary-state qvm) temp-storage)) ; stash this away for future use

    (values
     qvm
     (1+ (pc qvm)))))



(defun density-qvm-qubit-probability (qvm qubit)
  "The probability that the physical qubit addressed by QUBIT is 1."
  
  ;; TODO XXX optimize
  (let ((sum (flonum 0)))
    ;; This is a sum along the diagonal of the density-matrix
    (dotimes (i (half (expt 2 (number-of-qubits qvm))) sum)
      (let ((address (index-to-address i qubit 1)))
        (incf sum (realpart (density-matrix-entry qvm address address)))))))

(defun density-matrix-entry (qvm i j)
  "Returns the i,j entry of the density matrix encoded by the density matrix qvm."
  
  ;;  TODO XXX type checks here

  ;; wf is vec(ρ), ρ[i,j] has index i*2^n + j where n is the number of qubits  
  (let* ((n (number-of-qubits qvm))
         (index (+ (* i (expt 2 n)) j)))
    (aref (amplitudes qvm) index)))

(defun (setf density-matrix-entry) (value qvm i j)
  (let* ((n (number-of-qubits qvm))
         (index (+ (* i (expt 2 n)) j)))
    (setf (aref (amplitudes qvm) index) value)))

(defun density-qvm-force-measurement (measured-value qubit qvm excited-probability)
  "Forse the quantum system VM to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the density matrix appropriately.

EXCITED-PROBABILITY should be the probability that QUBIT measured to |1>, regardless of what it's being forced as.
"
  ;; measurement of qubit i corresponds to roughly this:
  ;; if outcome = 0, set rows/columns corresponding to i = 1 to zero
  ;; if outcome = 1, set rows/columns corresponding to i = 0 to zero
  (let* ((annihilated-state (- 1 measured-value))
         (inv-norm (if (zerop annihilated-state)
                       (/ excited-probability)
                       (/ (- (flonum 1) excited-probability)))))
    (dotimes (i (number-of-qubits qvm) qvm)
      (dotimes (j (number-of-qubits qvm))
        (if (or (= annihilated-state (ldb (byte 1 qubit) i))
                (= annihilated-state (ldb (byte 1 qubit) j)))
            (setf (density-matrix-entry qvm i j) (cflonum 0))
            (setf (density-matrix-entry qvm i j) (* inv-norm (density-matrix-entry qvm i j)))))))
  )

(defmethod measure ((qvm density-qvm) q c)
  (check-type c (or null quil:memory-ref))
  (let* ((r (random 1.0d0))
         (excited-probability (density-qvm-qubit-probability qvm q))
         (cbit (if (<= r excited-probability)
                   1
                   0)))
    ;; Force the non-deterministic measurement.
    (density-qvm-force-measurement cbit q qvm excited-probability)

    ;; Store the classical bit if necessary.
    (unless (null c)
      (setf (dereference-mref qvm c) cbit))

    ;; Return the qvm.
    (values qvm cbit)))

;;; TODO XXX can we do this in parallel?
(defmethod measure-all ((qvm density-qvm))
  (loop :for q :upto (number-of-qubits qvm)
        :collect (measure qvm q nil)))


(defun density-matrix-trace (qvm)
  "Compute the trace of the density matrix associated with density qvm."
  (let ((sum (flonum 0)))
    ;; This is a sum along the diagonal of the density-matrix
    (dotimes (i (expt 2 (number-of-qubits qvm)) sum)
      (incf sum (realpart (density-matrix-entry qvm i i))))))
