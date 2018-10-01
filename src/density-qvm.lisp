;;;; src/density-qvm.lisp
;;;;
;;;; Authors: Robert Smith
;;;;          Erik Davis

(in-package #:qvm)

;;; This file implements inefficient density matrix evolution.


(defclass density-qvm (pure-state-qvm)
  ((matrix-view :accessor density-matrix-view
                :initform nil
                :documentation "This is a 2D array displaced to the underlying QVM amplitudes.")
   (temporary-state :accessor temporary-state
                    :initform nil
                    :documentation "Sometimes the simulation needs to use some temporary state. We save it so we don't have to alloc/dealloc frequently.")
   (noisy-gate-definitions :initarg :noisy-gate-definitions
                           :accessor noisy-gate-definitions
                           :initform (make-hash-table :test 'equalp)
                           :documentation "Noisy gate definitions that, if present, override those stored in GATE-DEFINITIONS."))
  (:documentation "A density matrix simulator."))

;;; Creation and Initialization


(defmethod initialize-instance :after ((qvm density-qvm) &rest args)
  (declare (ignore args))
  (let* ((num-qubits (number-of-qubits qvm))
         (dim        (expt 2 num-qubits)))

    ;; The amplitudes actually represent the entries of the density
    ;; matrix in row-major order. For a system of N qubits, the
    ;; density matrix is 2^N x 2^N, hence a total of 2^(2N) entries.

    ;; It's possible that some other INITIALIZE-INSTANCE made something with the wrong size.
    ;; If so, we just make a new vector.
    ;; TODO XXX think of a better way of dealing with this
    (unless (= (length (amplitudes qvm)) (expt dim 2))
      (setf (amplitudes qvm) (make-vector (expt dim 2))))
    
    ;; It just so happens that the pure, zero state is the same in
    ;; this formalism, i.e., a 1 in the first entry.
    (bring-to-zero-state (amplitudes qvm))
    (setf (density-matrix-view qvm)
          (make-array (list dim dim)
                      :element-type (array-element-type (amplitudes qvm))
                      :displaced-to (amplitudes qvm)))))


(defun make-density-qvm (num-qubits)
  (make-instance 'density-qvm
                 :number-of-qubits num-qubits))


(adt:defdata superoperator
  "Representation of a linear operator on density operators."
  ;; Let ' mean † aka conjugate transpose.
  ;;
  ;; ρ ↦ U ρ U'
  (single-kraus quil:gate)
  ;; ρ ↦ ∑ᵢ Aᵢ ρ Aᵢ'
  (kraus-list list))


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
    ((single-kraus U)
     ;; (U ⊗ U'ᵀ) = (U ⊗ U*), where * is entrywise conjugate.
     (let ((U* (make-instance 'quil:simple-gate
                              :name (concatenate 'string (quil:gate-name U) "*")
                              :dimension (quil:gate-dimension U)
                              :matrix (magicl:conjugate-entrywise (quil:gate-matrix U)))))
       (apply-gate U* vec-density qubits)
       (apply-gate U  vec-density ghost-qubits)
       (values vec-density temporary-storage)))
    ((kraus-list list)
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


(defmethod set-noisy-gate ((qvm density-qvm) gate-name qubits kraus-ops)
  (check-kraus-ops kraus-ops)
  ;; Wrap a matrix in a gate in a superoperator...
  (labels ((make-temp-sop  (mat)
             (single-kraus
              (make-instance 'quil:simple-gate
                             :name (string (gensym "KRAUS-TEMP"))
                             :dimension (quil:gate-dimension mat)
                             :matrix mat))))
    (setf (gethash (list gate-name qubits) (noisy-gate-definitions qvm))
          (kraus-list (mapcar #'make-temp-sop kraus-ops)))))


(defmethod transition ((qvm density-qvm) (instr quil:gate-application))
  (assert (typep (quil:application-operator instr) 'quil:named-operator)
          (instr)
          "The noisy QVM doesn't support gate modifiers.")
  (let*  ((gate-name (quil::operator-description-name (quil:application-operator instr)))
          (gate (pull-teeth-to-get-a-gate instr))
          (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
          (ghosts (mapcar (alexandria:curry #'+ (number-of-qubits qvm)) qubits))
          (kraus-ops (gethash (list gate-name qubits) (noisy-gate-definitions qvm))))

    ;; TODO XXX handle parametric gates or at least error out

    (let ((sop (or kraus-ops (single-kraus gate))))
      ;; Do the superoperator application.
      (multiple-value-bind (new-density temp-storage)
          (apply-superoperator sop
                               (amplitudes qvm)
                               (apply #'nat-tuple qubits)
                               (apply #'nat-tuple ghosts)
                               :temporary-storage (temporary-state qvm))
        (declare (ignore new-density))
        (setf (temporary-state qvm) temp-storage))) ; stash this away for future use

    (values
     qvm
     (1+ (pc qvm)))))


(defun density-qvm-qubit-probability (qvm qubit)
  "The probability that the physical qubit addressed by QUBIT is 1."
  
  ;; TODO XXX optimize
  (let ((sum (flonum 0))
        (density-matrix (density-matrix-view qvm)))
    ;; This is a sum along the diagonal of the density-matrix
    (dotimes (i (half (expt 2 (number-of-qubits qvm))) sum)
      (let ((address (index-to-address i qubit 1)))
        (incf sum (realpart (aref density-matrix address address)))))))



(defun density-qvm-force-measurement (measured-value qubit qvm excited-probability)
  "Force the quantum system VM to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the density matrix appropriately.

EXCITED-PROBABILITY should be the probability that QUBIT measured to |1>, regardless of what it's being forced as.
"
  ;; measurement of qubit i corresponds to roughly this:
  ;; if outcome = 0, set rows/columns corresponding to i = 1 to zero
  ;; if outcome = 1, set rows/columns corresponding to i = 0 to zero
  (let* ((density-matrix (density-matrix-view qvm))
         (annihilated-state (- 1 measured-value))
         (inv-norm (if (zerop annihilated-state)
                       (/ excited-probability)
                       (/ (- (flonum 1) excited-probability)))))
    (destructuring-bind (rows cols) (array-dimensions density-matrix)
      (dotimes (i rows qvm)
        (dotimes (j cols)
          (if (or (= annihilated-state (ldb (byte 1 qubit) i))
                (= annihilated-state (ldb (byte 1 qubit) j)))
              (setf (aref density-matrix i j) (cflonum 0))
              (setf (aref density-matrix i j)
                    (* inv-norm (aref density-matrix i j)))))))))

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
