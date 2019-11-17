;;;; src/qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file implements a PURE-STATE-QVM.

;;; General Overview

;;; The BASE-QVM is base class that implements the fundamental
;;; functionality that every QVM will use. The BASE-QVM can evolve
;;; both pure state wavefunctions and density matrices by means of its
;;; STATE field, which can be either a PURE-STATE or DENSITY-MATRIX
;;; STATE. Furthermore, the BASE-QVM supports the application of
;;; SUPEROPERATORS, which can be applied to both types of state
;;; representations when added to QUIL programs via the
;;; ADD-KRAUS-PRAGMA.

(defclass base-qvm (classical-memory-mixin)
  ((number-of-qubits 
    :reader number-of-qubits
    :initarg :number-of-qubits
    :type alexandria:non-negative-fixnum
    :initform (error ":NUMBER-OF-QUBITS is a required initarg ~ to BASE-QVM.")
    :documentation "Number of qubits being simulated by the QVM.")
   (state 
    :accessor state
    :initarg :state
    :documentation "The unpermuted wavefunction in standard order.")
   (program-compiled-p 
    :accessor program-compiled-p
    :initform nil
    :documentation "Has the loaded program been compiled?")
   (superoperator-definitions 
    :initarg :superoperator-definitions
    :accessor superoperator-definitions
    :initform (make-hash-table :test 'equalp)
    :documentation "A hash table of gate name to SUPEROPERATOR. These superoperator definitions are meant to override the gate name definitions stored in GATE-DEFINITIONS, such that a superoperator will be applied to the state of the QVM if the current gate name matches an entry in the SUPEROPERATOR-DEFINITIONS."))
  (:metaclass abstract-class))

;;; Fundamental Manipulation of the QVM

(defmethod amplitudes ((qvm base-qvm))
  ;; Read the amplitudes from the state of the QVM.
  (amplitudes (state qvm)))

(defmethod (setf amplitudes) (new-amplitudes (qvm base-qvm))
  ;; Set the amplitudes to the state of the QVM.
  (setf (amplitudes (state qvm)) new-amplitudes))

(defun nth-amplitude (qvm n)
  "Get the Nth amplitude of the quantum virtual machine QVM."
  (aref (amplitudes qvm) n))

(defun (setf nth-amplitude) (new-value qvm n)
  "Set the Nth amplitude of the quantum virtual machine QVM."
  (setf (aref (amplitudes qvm) n) new-value))

(defun map-amplitudes (qvm f)
  "Apply the function F to the amplitudes of the quantum virtual machine QVM in standard order."
  (map nil f (amplitudes qvm))
  (values))

(defmethod reset-quantum-state ((qvm base-qvm))
  ;; It just so happens that the pure, zero state is the same in
  ;; this formalism, i.e., a 1 in the first entry.
  (bring-to-zero-state (amplitudes qvm))
  qvm)

(defmethod set-superoperator ((qvm base-qvm) gate-name qubits kraus-ops)
  ;; Wrap a gate defined by GATE-NAME and QUBITS in a superoperator
  ;; defined by KRAUS-OPS. When the QVM reads an instruction with
  ;; GATE-NAME on QUBITS, the superoperator made from the KRAUS-OPS
  ;; will be applied to the state of the QVM.  Note: if we are
  ;; applying superoperators, we do not want to compile the QVM
  ;; program before running it
  (setf qvm:*compile-before-running* nil)
  (check-kraus-ops kraus-ops)
  (setf (gethash (list gate-name qubits) (superoperator-definitions qvm))
        (kraus-list (mapcar #'lift-matrix-to-superoperator kraus-ops))))

(defmethod run :after ((qvm base-qvm))
  ;; Only copy if we really need to.
  (when (requires-swapping-amps-p (state qvm))
    (swap-internal-amplitude-pointers (state qvm))))

;;;;;;;;;;;;;;;;;;;;;;; PURE STATE QVM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The PURE-STATE-QVM is an implementation of a QVM that evolves pure
;;; state quantum systems. The STATE of the PURE-STATE-QVM is a
;;; PURE-STATE, in which the wavefunction of the quantum system is
;;; represented by a 2n qubit vector of amplitudes. The PURE-STATE-QVM
;;; also supports superoperator applications by stochastically
;;; applying the kraus operators they are represented by.

(defclass pure-state-qvm (base-qvm)
  ((state :accessor state
          :initarg :state
          :type (or null pure-state)
          :documentation "The unpermuted wavefunction in standard order."))
  (:documentation "An pure-state implementation of the Quantum Abstract Machine."))

;;; Creation and Initialization
(defmethod initialize-instance :after ((qvm pure-state-qvm) &rest args)
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qvm)))
    (cond
      ((and (slot-boundp qvm 'state)
            (not (null (slot-value qvm 'state))))
       ;; Check that it represents the number of qubits it should.
       (assert (<= num-qubits (wavefunction-qubits (amplitudes qvm)))
               ()
               "The provided amplitudes to the PURE-STATE-QVM ~A represents ~D qubit~:P, ~
                but the QAM is specified to need ~D qubit~:P."
               qvm
               (wavefunction-qubits (amplitudes (state qvm)))
               num-qubits))
      (t
       ;; If the amplitudes weren't specified, initialize to |...000>.
       ;;
       ;; We explicitly zero out the memory to make sure all pages get
       ;; touched.
       (setf (state qvm) (make-instance 'pure-state :num-qubits (number-of-qubits qvm)))
       (bring-to-zero-state (amplitudes qvm))))))

(defun make-qvm (num-qubits &key (classical-memory-model quil:**empty-memory-model**)
                                 (allocation nil))
  "Make a new quantum virtual machine with NUM-QUBITS number of qubits and a classical memory size of CLASSICAL-MEMORY-SIZE bits.ALLOCATION is an optional argument with the following behavior.
    - If it's NULL (default), then a standard wavefunction in the Lisp heap will be allocated.
    - If it's a STRING, then the wavefunction will be allocated as a shared memory object, accessible by that name.
    - Otherwise, it's assumed to be an object that is compatible with the ALLOCATION-LENGTH and ALLOCATE-VECTOR methods"
  (check-type num-qubits unsigned-byte)
  (check-type classical-memory-model quil:memory-model)
  (make-instance 'pure-state-qvm
                 :number-of-qubits num-qubits
                 :state (make-pure-state num-qubits :allocation allocation)
                 :classical-memory-subsystem
                 (make-instance 'classical-memory-subsystem
                                :classical-memory-model
                                classical-memory-model)))

(defmethod compile-loaded-program ((qvm pure-state-qvm))
  ;; Compile the loaded program on the PURE-STATE-QVM QVM.
  (unless (program-compiled-p qvm)
    (when *fuse-gates-during-compilation*
      (setf (program qvm) (quil::fuse-gates-in-executable-code (program qvm))))
    (when *compile-measure-chains*
      (setf (program qvm) (compile-measure-chains (program qvm) (number-of-qubits qvm))))
    (setf (program qvm)
          (map 'vector (lambda (isn) (compile-instruction qvm isn)) (program qvm)))
    (setf (program-compiled-p qvm) t))
  qvm)

;;; DEPRECATED:

(defun qubit-probability (qvm qubit)
  "DEPRECATED // The probability that the physical qubit addressed by QUBIT is 1."
  (let ((wavefunction (amplitudes qvm)))
    (declare (type quantum-state wavefunction))
    (wavefunction-excited-state-probability wavefunction qubit)))
