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

  (:method ((gate compiled-matrix-gate) wavefunction qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-MATRIX-GATE.")
    (funcall (compiled-gate-apply-operator gate)
             (compiled-matrix gate)
             wavefunction))

  (:method ((gate compiled-permutation-gate) wavefunction qubits &rest parameters)
    (declare (ignore qubits))
    (assert (null parameters) (parameters) "Parameters don't make sense for a COMPILED-MATRIX-GATE.")
    (funcall (compiled-gate-apply-operator gate) wavefunction)))
