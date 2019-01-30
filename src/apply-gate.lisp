;;;; src/apply-gate.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Warm the cache at compile time.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (warm-apply-matrix-operator-cache :max-qubits 30))

;;; Actual application of gates.

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
             wavefunction))

  (:method ((gate compiled-parameterized-gate-application) wavefunction qubits &rest parameters)
    (declare (ignore qubits))
    (assert (not (null parameters)))
    (funcall (compiled-gate-apply-operator gate)
             (quil:constant-value (first parameters))
             wavefunction))
  
  (:method ((gate compiled-permutation-gate-application) wavefunction qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-PERMUTATION-GATE-APPLICATIONs.")
    (funcall (compiled-gate-apply-operator gate) wavefunction)))
