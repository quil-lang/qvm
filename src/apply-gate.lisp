;;;; src/apply-gate.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Warm the cache at compile time.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (warm-apply-matrix-operator-cache :max-qubits 30))

;;; Actual application of gates.


(defgeneric apply-gate-state (gate state qubits &rest parameters)
  
  (:method ((gate quil:simple-gate) (state pure-state) qubits &rest parameters)
    (print "simple gate")
    (print parameters)
    (assert (null parameters) (parameters) "Parameters don't make sense for simple gates.")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator (quil:gate-matrix gate))
     (amplitudes state)
     qubits))
  
  (:method ((gate quil:parameterized-gate) (state pure-state) qubits &rest parameters)
    (print "param gate")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     qubits))
  
  (:method ((gate quil:permutation-gate) (state pure-state) qubits &rest parameters)
    (print "perm gate")
    (assert (null parameters) (parameters) "Parameters don't make sense for simple gates.")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator (quil:gate-matrix gate))
     (amplitudes state)
     qubits))
  
  (:method ((gate quil:controlled-gate) (state pure-state) qubits &rest parameters)
    (print "controlled gate")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     qubits)
    )
  (:method ((gate quil:forked-gate) (state pure-state) qubits &rest parameters)
    (print "forked gate")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     qubits))
  
  (:method ((gate quil:dagger-gate) (state pure-state) qubits &rest parameters)
    (print "dagger gate")
    (apply-matrix-operator
     (magicl-matrix-to-quantum-operator
      (apply #'quil:gate-matrix gate parameters))
     (amplitudes state)
     qubits))
  
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
    (print "density matrix ssstate")
    (let ((sop (single-kraus gate))
          (ghosts (mapcar (alexandria:curry #'+ (num-qubits state)) qubits)))
      (multiple-value-bind (new-density temp-storage)
          ;; parameters gets collected into a list, which we need to unpack:
          (apply #'apply-superoperatorr sop (amplitudes state)
                 (apply #'nat-tuple qubits)
                 (apply #'nat-tuple ghosts)
                 state
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
