;;;; src/mixed-state-qvm.lisp
;;;;
;;;; Authors: Robert Smith
;;;;          Erik Davis
;;;;          Sophia Ponte

(in-package #:qvm)

;;; This file implements a qvm that evolves pure and mixed states by
;;; means of a DENSITY-MATRIX-STATE.

;;; General Overview

;;; The MIXED-STATE-QVM is an implementation of a QVM that can evolve
;;; either a pure or mixed state using a DENSITY-MATRIX-STATE. The
;;; MIXED-STATE-QVM inherits most of its core behavior from BASE-QVM,
;;; but provides more specialized functionality for methods like
;;; TRANSITION, MEASURE, and MEASURE-ALL. Since the MIXED-STATE-QVM
;;; uses a DENSITY-MATRIX-STATE, it converts quil gates to
;;; superoperators for gate application. Additional superoperators can
;;; be defined in the SUPEROPERATOR-DEFINITIONS slot of the
;;; MIXED-STATE-QVM.

(defclass mixed-state-qvm (base-qvm)
  ((state :reader state
          :writer %set-state
          :initarg :state
          :type (or null density-matrix-state))) ; XXX: Should we nix
                                                 ; the null option
                                                 ; here?
  (:documentation "A qvm for simulating mixed-state quantum systems."))

(defmethod initialize-instance :after ((qvm mixed-state-qvm) &rest args)
  (declare (ignore args))
  ;; PURE-STATE-QVM does its own allocation, which we don't want, so
  ;; here we make sure that the STATE slot has a vector of the
  ;; right size (e.g. it was constructed by MAKE-MIXED-STATE-QVM).
  (when (or (not (slot-boundp qvm 'state))
            (null (slot-value qvm 'state)))
    (%set-state (make-instance 'density-matrix-state :num-qubits (number-of-qubits qvm))
                qvm)
      (set-to-zero-state (state qvm))))

(defun make-mixed-state-qvm (num-qubits &key (allocation nil) &allow-other-keys)
  "Build a MIXED-STATE-QVM with a DENSITY-MATRIX-STATE representing NUM-QUBITS qubits."
  (check-type num-qubits unsigned-byte)
  (make-instance 'mixed-state-qvm :number-of-qubits num-qubits 
                                  :state (make-density-matrix-state 
                                          num-qubits  
                                          :allocation allocation)))

(defun full-density-number-of-qubits (vec-density)
  "Computes the number of qubits encoded by a vectorized density matrix."
  (1- (integer-length (isqrt (length vec-density)))))


(defun mixed-state-qvm-measurement-probabilities (qvm)
  "Computes the probability distribution of measurement outcomes (a vector)
  associated with the STATE of the DENSITY-QVM."
  (density-matrix-state-measurement-probabilities (state qvm)))

;;; Don't compile things for the mixed-state-qvm.
(defmethod compile-loaded-program ((qvm mixed-state-qvm))
  qvm)

;;; TODO: FIXME: we should be able to compile density operator stuff
;;; just fine.
(defmethod compile-instruction ((qvm mixed-state-qvm) isn)
  (declare (ignore qvm))
  isn)
