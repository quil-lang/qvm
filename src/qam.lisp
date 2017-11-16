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

    1. The resulting QVM.
    2. The measured classical bit."))

(defgeneric measure-all (qam)
  (:documentation "Non-deterministically perform a measurement on the qubit addressed by Q in the quantum abstract machine QAM. Store the bit at the classical bit memory address C. If C is instead NIL, don't store.

Return two values:

    1. The resulting QVM.
    2. The measured classical bit."))
