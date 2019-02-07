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

(defgeneric reset-quantum-state (qam)
  (:documentation "Bring all qubits of the quantum abstract machine QAM to the zero state."))

(defgeneric measure (qam q)
  (:documentation  "Non-deterministically perform a measurement on the qubit addressed by Q in the quantum abstract machine QAM.

Return two values:

    1. The resulting QAM.
    2. The measured classical bit."))

(defgeneric measure-and-store (qam q c)
  (:documentation "Performs a measurement on the qubit addressed by Q in the quantum abstract machine QAM, and stores the measured
bit in the classical bit addressed by C.

Return two values:

    1. The resulting QAM.
    2. The measured classical bit."))

(defmethod measure-and-store ((qam quantum-abstract-machine) q c)
  (check-type c quil:memory-ref)
  (multiple-value-bind (ret-qam cbit)
                  (measure qam q)
    (setf (dereference-mref qam c) cbit)
    (values ret-qam cbit)))

(defgeneric measure-all (qam)
  (:documentation "Non-deterministically perform a measurement on all qubits in the quantum abstract machine QAM.
Return two values:

    1. The resulting QAM.
    2. A list of measured bits."))

(defmethod measure-all (qam)
  (let ((measured-bits nil))
    (loop :for q :from (1- (number-of-qubits qam)) :downto 0
          :do (multiple-value-bind (ret-qam bit)
                  (measure qam q)
                (push bit measured-bits)
                (setf qam ret-qam)))
    (values
     qam
     measured-bits)))

(defgeneric number-of-qubits (qam)
  (:documentation "Return the number of qubits configured on the quantum abstract machine QAM."))
