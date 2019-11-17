;;;; src/apply-gate.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;2 Not sure if here is the best place to define this ... 
(adt:defdata superoperator
  "Representation of a linear operator on density operators."
  ;; Let ' mean † aka conjugate transpose.
  ;;
  ;; ρ ↦ U ρ U'
  (single-kraus quil:gate)
  ;; ρ ↦ ∑ᵢ Aᵢ ρ Aᵢ'
  (kraus-list list))

;;; Warm the cache at compile time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (warm-apply-matrix-operator-cache :max-qubits 30))

(defgeneric apply-noise-to-state (kraus-map state qubits)
  (:documentation "Apply the noise defined by a single KRAUS-MAP to the STATE, using QUBITS."))

(defmethod apply-noise-to-state (kraus-map (state pure-state) qubits)
  ;; Applies the noise defined by KRAUS-MAP to the STATE. Since the STATE is a PURE-STATE, the KRAUS-MAP is converted to a SUPEROPERATOR KRAUS-LIST, and sent to STOICHASTIC-PURE-STATE-EVOLUTION to apply by inverse transform sampling.  
  (stochastic-pure-state-evolution (mapcar #'lift-matrix-to-superoperator kraus-map) 
                                    state
                                    qubits))

(defmethod apply-noise-to-state (kraus-map (state density-matrix-state) qubits)
  ;; Applies the noise defined by a single KRAUS-MAP to the
  ;; STATE. Since the STATE is a DENSITY-MATRIX-STATE, the KRAUS-MAP
  ;; is converted to a superoperator which is then applied to the
  ;; STATE.
  (let* ((ghosts (mapcar (alexandria:curry #'+ (num-qubits state)) qubits))
         (sop (convert-to-kraus-list kraus-map)))
    (multiple-value-bind (new-density temp-storage)
        (apply-superoperator sop
                             state
                             qubits
                             ghosts
                             :temporary-storage (temporary-state state)
                             :params nil)
      (declare (ignore new-density))
      (setf (temporary-state state) temp-storage))))


(defmethod stochastic-pure-state-evolution (kraus-map (state pure-state) qubits)
  ;; Uniformly at random select one of the kraus operators in KRAUS-OPS to apply
  ;; for the PURE-STATE STATE.
  ;; Randomly select one of the Kraus operators by inverse transform
  ;; sampling (cf [1]): We divide the unit interval [0,1] into n
  ;; bins where the j-th bin size equals the probability p_j with
  ;; which the j-th Kraus operator k_j should be applied. We know
  ;; that the Kraus operators are normalized such that p_j =
  ;; <psi|k_j^H k_j |psi> where x^H denotes hermitian conjugation of
  ;; x and can therefore perform this sampling lazily: First
  ;; generate a uniformly sampled random number r in [0,1]. Next,
  ;; find j such that
  ;;
  ;;       sum_{k=1}^{j-1} p_k < r <= sum_{k=1}^{j} p_k
  ;;
  ;; This is possible by evaluating only all p_k for k<=j. Then
  ;; pick this j as the choice of Kraus operator to apply.
  ;;
  ;; [1]: https://en.wikipedia.org/wiki/Inverse_transform_sampling
  (let* ((r (random 1.0d0))
         (summed 0.0d0))
    (loop :for kraus-thing :in kraus-map
          :for kj := (adt:match superoperator kraus-thing
                                ((single-kraus U) U)
                                (_ (error "The elements of KRAUS-MAP must be SINGLE-KRAUS.")))
          :do
             (replace (%trial-amplitudes state) (amplitudes state))
             (apply-matrix-operator (magicl-matrix-to-quantum-operator (quil:gate-matrix kj))
                                    (%trial-amplitudes state)
                                    (apply #'nat-tuple qubits))
             (incf summed (psum #'probability (%trial-amplitudes state)))
          :until (>= summed r))
    (rotatef (amplitudes state) (%trial-amplitudes state))
    (normalize-wavefunction (amplitudes state))))


(defmethod apply-superoperator (sop (state density-matrix-state) qubits ghost-qubits &key temporary-storage params)
  "Apply a superoperator SOP to a vectorized density matrix given in the DENSITY-MATRIX-STATE STATE, where QUBITS and GHOST-QUBITS are tuples of qubit indices which the superoperator acts on (from the left and right respectively). The computation may require TEMPORARY-STORAGE, a vector of the same length as the AMPLITUDES of the STATE. If no TEMPORARY-STORAGE is provided, it will be allocated as needed. Returns the pair of updated STATE and (perhaps freshly allocated) TEMPORARY-STORAGE."
  (check-type sop superoperator)
  (let ((vec-density (amplitudes state)))
    ;; We use the following law to help our calculation:
    ;;
    ;;     vec(AρB) = (A ⊗ Bᵀ)vec(ρ)
    ;;
    ;; where vec(·) is row-major vectorization. The quantity vec(ρ) is
    ;; the VEC-DENSITY argument.
    (adt:match superoperator sop 
      ((single-kraus U)
       ;; (U ⊗ U'ᵀ) = (U ⊗ U*), where * is entrywise conjugate.
       (let ((U* (conjugate-entrywise U))
             (pure-state (make-pure-state (* 2 (num-qubits state)))))
         (setf (amplitudes pure-state) vec-density)
         (apply #'apply-gate-state U* pure-state qubits params)
         (apply #'apply-gate-state U  pure-state ghost-qubits params)
         (values (amplitudes pure-state) temporary-storage)))
      ((kraus-list list)
       (cond
         ;; Empty. Just treat as identity.
         ((endp list)
          (values vec-density temporary-storage))
         ;; Degenerate case of just 1 superoperator.
         ((endp (rest list))
          (apply-superoperator (first list) state qubits ghost-qubits
                               :temporary-storage temporary-storage
                               :params params))
         ;; General (and super expensive) case.
         (t
          ;; XXX FIXME: We could eliminate one copy if our APPLY-GATE
          ;; function could understand a source and destination array.
          (let ((pristine (copy-seq vec-density))
                (sum (fill (or temporary-storage 
                               (copy-seq vec-density)) 
                           (cflonum 0))))
            (dolist (sub-sop list)
              ;; Apply the operator.
              (apply-superoperator sub-sop state qubits ghost-qubits :params params)
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
            (values vec-density sum))))))))

(defun convert-to-kraus-list (kraus-ops)
  "Converts a list of magicl matrices KRAUS-OPS to a SUPEROPERATOR KRAUS-LIST, which is a list of SINGLE-KRAUS."
  (check-kraus-ops kraus-ops)
  (kraus-list (mapcar #'lift-matrix-to-superoperator kraus-ops)))

(defgeneric apply-gate-state (gate state qubits &rest parameters)
  (:documentation "Apply a gate GATE to the state STATE on the sub-Hilbert space defined by the NAT-TUPLE of qubit indexes QUBITS. PARAMETERS is a list of numeric parameters passed to a dynamic gate.")
  
  (:method ((gate quil:simple-gate) (state pure-state) qubits &rest parameters)
    (assert (null parameters) (parameters) "Parameters don't make sense for simple gates.")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator (quil:gate-matrix gate))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:parameterized-gate) (state pure-state) qubits &rest parameters)
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:permutation-gate) (state pure-state) qubits &rest parameters)
    (assert (null parameters) (parameters) "Parameters don't make sense for simple gates.")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator (quil:gate-matrix gate))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:controlled-gate) (state pure-state) qubits &rest parameters)
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:forked-gate) (state pure-state) qubits &rest parameters)
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:dagger-gate) (state pure-state) qubits &rest parameters)
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate compiled-matrix-gate-application) 
            (state pure-state) qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-MATRIX-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate)
             (compiled-matrix gate)
             (amplitudes state)))
  
  (:method ((gate compiled-inlined-matrix-gate-application) 
            (state pure-state) qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-INLINED-MATRIX-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate)
             (amplitudes state)))
  
  (:method ((gate compiled-permutation-gate-application) 
            (state pure-state) qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-PERMUTATION-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate) (amplitudes state)))
  
  (:method ((gate superoperator) (state pure-state) qubits &rest parameters)
    ;; Apply a superoperator GATE to a PURE-STATE STATE using
    ;; stochastic pure state evolution.
    (declare (ignore parameters))
    (adt:match superoperator gate
               ((kraus-list kraus-map)
                (stochastic-pure-state-evolution kraus-map 
                                                 state
                                                 qubits))
               (_ (error "GATE must be a KRAUS-LIST."))))
     
  (:method ((gate superoperator) (state density-matrix-state) qubits &rest parameters)
    ;; Apply a superoperator GATE to a DENSITY-MATRIX-STATE STATE
    (let ((ghosts (mapcar (alexandria:curry #'+ (num-qubits state)) qubits)))
      (multiple-value-bind (new-density temp-storage)
          (apply-superoperator gate state
                               qubits
                               ghosts
                               :temporary-storage (temporary-state state)
                               :params parameters)
        (declare (ignore new-density))
        (setf (temporary-state state) temp-storage))))

  (:method (gate (state density-matrix-state) qubits &rest parameters)
    ;; Apply a GATE to a DENSITY-MATRIX-STATE by converting the GATE
    ;; into a SINGLE-KRAUS SUPEROPERATOR and then applying the
    ;; SUPEROPERATOR.
    (let ((sop (single-kraus gate))
          (ghosts (mapcar (alexandria:curry #'+ (num-qubits state)) qubits)))
      (multiple-value-bind (new-density temp-storage)
          (apply-superoperator sop state
                               qubits
                               ghosts
                               :temporary-storage (temporary-state state)
                               :params parameters)
        (declare (ignore new-density))
        (setf (temporary-state state) temp-storage)))))

