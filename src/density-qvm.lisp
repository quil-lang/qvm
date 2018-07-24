;;;; src/density-qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file implements efficient density matrix evolution.

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

(defclass density-qvm (pure-state-qvm)
  ((temporary-state :accessor temporary-state
                    :initform nil
                    :documentation "Sometimes the simulation needs to use some temporary state. We save it so we don't have to alloc/dealloc frequently."))
  (:documentation "A density matrix simulator."))

;;; Creation and Initialization

(defmethod initialize-instance :after ((qvm density-qvm) &rest args)
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qvm)))
    ;; The amplitudes actually represent the entries of the density
    ;; matrix in row-major order. For a system of N qubits, the
    ;; density matrix is 2^N x 2^N, hence a total of 2^(2N) entries.
    (unless (and (slot-boundp qvm 'amplitudes)
                 (not (null (slot-value qvm 'amplitudes))))
      (setf (amplitudes qvm)
            (make-vector (expt 2 (* 2 num-qubits))))
      ;; It just so happens that the pure, zero state is the same in
      ;; this formalism, i.e., a 1 in the first entry.
      (bring-to-zero-state (amplitudes qvm)))))

(defun make-density-qvm (num-qubits)
  (check-type classical-memory-size (integer 0))
  (make-instance 'density-qvm
                 :number-of-qubits num-qubits))

(defmethod apply-superoperator (sop vec-density qubits ghost-qubits &key temporary-storage)
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
  (let* ((gate (lookup-gate qvm (quil:application-operator instr) :error t))
         (params (mapcar #'quil:constant-value (quil:application-parameters instr)))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (ghosts (mapcar (alexandria:curry #'+ (number-of-qubits qvm)) qubits)))
    ;; Normalize the gate to a superoperator.
    (unless (typep gate 'superoperator)
      (setf gate (plain-unitary gate)))

    ;; Do the superoperator application.
    (multiple-value-bind (new-density temp-storage)
        (apply-superoperator gate (amplitudes qvm) qubits ghosts
                             :temporary-storage (temporary-state qvm))
      (declare (ignore new-density))
      (setf (temporary-state qvm) temp-storage)) ; stash this away for future use

    (values
     qvm
     (1+ (pc qvm)))))
