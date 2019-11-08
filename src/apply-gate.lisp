;;;; src/apply-gate.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Warm the cache at compile time.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (warm-apply-matrix-operator-cache :max-qubits 30))

(defgeneric apply-noise-to-state (kraus-map state qubits)
  (:documentation "Apply the noise defined by a single KRAUS-MAP to the STATE, using QUBITS."))

(defmethod apply-noise-to-state (kraus-map (state pure-state) qubits)
  ;; Applies the noise defined by KRAUS-MAP to the STATE. Since the STATE is a PURE-STATE, the KRAUS-MAP is applied by uniformly at random selecting one of the kraus operators in KRAUS-MAPS to apply by inverse transform sampling.  
  (let* ((amps (%trial-amplitudes state))
         (r (random 1.0d0))
         (summed 0.0d0))
    (loop :for kj :in kraus-map
          :do
             (replace amps (amplitudes state))
             (apply-matrix-operator (magicl-matrix-to-quantum-operator kj)
                                    amps
                                    (apply #'nat-tuple qubits))
             (incf summed (psum #'probability amps))
          :until (>= summed r))
    (rotatef (amplitudes state) (%trial-amplitudes state))
    (normalize-wavefunction (amplitudes state))))

(defmethod apply-noise-to-state (kraus-map (state density-matrix-state) qubits)
  ;; Applies the noise defined by a single KRAUS-MAP to the
  ;; STATE. Since the STATE is a DENSITY-MATRIX-STATE, the KRAUS-MAP
  ;; is converted to a superoperator, which is then applied to the
  ;; STATE.
  (let* ((ghosts (mapcar (alexandria:curry #'+ (num-qubits state)) qubits))
         (sop (kraus-list (mapcar #'lift-matrix-to-superoperator kraus-map))))
    (multiple-value-bind (new-density temp-storage)
        (apply-superoperator sop
                             state
                             qubits
                             ghosts
                             :temporary-storage (temporary-state state)
                             :params nil)
      (declare (ignore new-density))
      (setf (temporary-state state) temp-storage))))


(defmethod apply-superoperator (sop state qubits ghost-qubits &key temporary-storage params)
  "Apply a superoperator SOP to a vectorized density matrix
VEC-DENSITY, where QUBITS and GHOST-QUBITS are tuples of qubit indices
which the superoperator acts on (from the left and right
respectively). The computation may require TEMPORARY-STORAGE, a vector
of the same length as VEC-DENSITY. If no TEMPORARY-STORAGE is
provided, it will be allocated as needed. Returns the pair of updated
VEC-DENSITY and (perhaps freshly allocated) TEMPORARY-STORAGE."
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


(defgeneric apply-gate-state (gate state qubits &rest parameters)
  
  (:method ((gate quil:simple-gate) (state pure-state) qubits &rest parameters)
    (print "simple gate")
    (assert (null parameters) (parameters) "Parameters don't make sense for simple gates.")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator (quil:gate-matrix gate))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:parameterized-gate) (state pure-state) qubits &rest parameters)
    (print "param gate")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:permutation-gate) (state pure-state) qubits &rest parameters)
    (print "perm gate")
    (assert (null parameters) (parameters) "Parameters don't make sense for simple gates.")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator (quil:gate-matrix gate))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:controlled-gate) (state pure-state) qubits &rest parameters)
    (print "controlled gate")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     (apply #'nat-tuple qubits))
    )
  (:method ((gate quil:forked-gate) (state pure-state) qubits &rest parameters)
    (print "forked gate")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate quil:dagger-gate) (state pure-state) qubits &rest parameters)
    (print "dagger gate")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     (apply #'nat-tuple qubits)))
  
  (:method ((gate compiled-matrix-gate-application) 
            (state pure-state) qubits &rest parameters)
    (declare (ignore qubits))
    (print "compiled matrix gate application")
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-MATRIX-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate)
             (compiled-matrix gate)
             (amplitudes state)))
  
  (:method ((gate compiled-inlined-matrix-gate-application) 
            (state pure-state) qubits &rest parameters)
    (declare (ignore qubits))
    (print "compiled inline matrix gate app")
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-INLINED-MATRIX-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate)
             (amplitudes state)))
  
  (:method ((gate compiled-permutation-gate-application) 
            (state pure-state) qubits &rest parameters)
    (declare (ignore qubits))
    (print "compiled permutation matrix gate app")
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-PERMUTATION-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate) (amplitudes state)))
  
  (:method (gate (state density-matrix-state) qubits &rest parameters)
    (print "density matrix state")
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


(defgeneric apply-gate (gate wavefunction qubits &rest parameters)
  (:documentation "Apply a gate GATE to the wavefunction WAVEFUNCTION on the sub-Hilbert space defined by the NAT-TUPLE of qubit indexes QUBITS. PARAMETERS is a list of numeric parameters passed to a dynamic gate.")

  (:method ((gate quil:simple-gate) wavefunction qubits &rest parameters)
    (assert (null parameters) (parameters) "Parameters don't make sense for simple gates.")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator (quil:gate-matrix gate))
     wavefunction
     qubits))

  (:method ((gate quil:parameterized-gate) wavefunction qubits &rest parameters)
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     wavefunction
     qubits))

  (:method ((gate quil:permutation-gate) wavefunction qubits &rest parameters)
    (assert (null parameters) (parameters) "Parameters don't make sense for a permutation gate.")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (quil:gate-matrix gate))
     wavefunction
     qubits))

  (:method ((gate quil:controlled-gate) wavefunction qubits &rest parameters)
    ;; TODO: We can be Even More Smart here and not resort to
    ;; expanding this big ol' matrix. (DAE not like big matrices?)
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     wavefunction
     qubits))

  (:method ((gate quil:forked-gate) wavefunction qubits &rest parameters)
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     wavefunction
     qubits))

  (:method ((gate quil:dagger-gate) wavefunction qubits &rest parameters)
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     wavefunction
     qubits))

  (:method ((gate compiled-matrix-gate-application) wavefunction qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-MATRIX-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate)
             (compiled-matrix gate)
             wavefunction))

  (:method ((gate compiled-inlined-matrix-gate-application) wavefunction qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-INLINED-MATRIX-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate)
             wavefunction))

  (:method ((gate compiled-permutation-gate-application) wavefunction qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-PERMUTATION-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate) wavefunction))
  )
