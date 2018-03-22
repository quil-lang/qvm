;;;; qam.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defclass quantum-abstract-machine ()
  ()
  (:documentation "A class representing implementations of the quantum abstract machine.")
  (:metaclass abstract-class))

(defgeneric run (qam)
  (:documentation "Simulate the quantum abstract machine QAM until completion. Return the QAM in its end state."))

(defgeneric measure (qam q c)
  (:documentation  "Non-deterministically perform a measurement on the qubit addressed by Q in the quantum abstract machine QAM. Store the bit at the classical bit memory address C. If C is instead NIL, don't store.

Return two values:

    1. The resulting QAM.
    2. The measured classical bit."))

(defgeneric measure-all (qam)
  (:documentation "Non-deterministically perform a measurement on all qubits in the quantum abstract machine QAM.
Return two values:

    1. The resulting QAM.
    2. A list of measured bits."))

(defmethod measure-all (qam)
  (loop :for q :upto (number-of-qubits qam)
        :collect (measure qam q nil)))

(defgeneric number-of-qubits (qam)
  (:documentation "Return the number of qubits configured on the quantum abstract machine QAM."))
