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
