;;;; src/apply-gate.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defun permutation-to-transpositions (permutation)
  "Decompose a permutation PERMUTATION represented as a sequence of non-negative integers into a list of transpositions represented as conses."
  (let ((indices (copy-seq permutation))
        (swaps nil))
    (dotimes (dest (length indices) (nreverse swaps))
      (let ((src (elt indices dest)))
        (loop :while (< src dest) :do
          (setf src (elt indices src)))
        (when (/= src dest)
          (push (cons src dest) swaps))))))

(defun generate-permutation-gate-function (permutation)
  "Generate an efficient unary function which modifies a quantum state according to the permutation PERMUTATION."
  (labels ((generate-code (transpositions)
             (loop :with variable := (gensym "V")
                   :for (left . right) :in transpositions
                   :collect `(rotatef (aref ,variable ,left)
                                      (aref ,variable ,right))
                     :into rotations
                   :finally (return (values variable rotations))))

           (generate-lambda-form (variable code)
             `(lambda (,variable)
                (declare ,*optimize-dangerously-fast*
                         (type quantum-state ,variable)
                         (ignorable ,variable))
                ,@code
                nil)))
    (multiple-value-bind (variable code)
        (generate-code (permutation-to-transpositions permutation))
      (compile nil (generate-lambda-form variable code)))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Gate Application ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; One might ask why we define a generic function to act on a
;;; wavefunction with qubits, as opposed to a state vector. I (Robert)
;;; think the the latter approach would be more mathematically
;;; elegant, but unfortunately, it is much harder to optimize. The
;;; compiler will be unable to inline any of the various chains of
;;; functions if we need to dispatch on every subspace application.

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
    (apply-operator
     (generate-permutation-gate-function
      (quil:permutation-gate-permutation gate))
     wavefunction
     qubits)))

