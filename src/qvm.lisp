;;;; src/qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file implements a PURE-STATE-QVM.

;;; General Overview

;;; The BASE-QVM is base class that implements the fundamental
;;; functionality that every QVM will use. The BASE-QVM can evolve
;;; both pure state wavefunctions and density matrices by means of its
;;; STATE slot, which can be either a PURE-STATE or DENSITY-MATRIX
;;; STATE or any state matching the protocols defined in MEASURE,
;;; APPLY-GATE, and STATE-REPRESENTATION. Furthermore, the BASE-QVM
;;; supports the application of superoperators, which can be applied
;;; to both types of state representation when added to QUIL programs
;;; via the ADD-KRAUS PRAGMA.

(defclass base-qvm (classical-memory-mixin)
  ((number-of-qubits ; XXX: Should we just compute the number of qubits from the STATE?
    :reader number-of-qubits
    :initarg :number-of-qubits
    :type alexandria:non-negative-fixnum
    :initform (error ":NUMBER-OF-QUBITS is a required initarg to BASE-QVM.")
    :documentation "Number of qubits being simulated by the QVM.")
   (state
    :reader state
    :writer %set-state
    :initarg :state
    :documentation "The state of the quantum system simulated by the QVM.")
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

(defun check-superoperators (superoperators)
  "Convert each SUPEROPERATOR in SUPEROPERATORS to a matrix and call CHECK-ALL-KRAUS-OPS on the resulting list."
  (check-all-kraus-ops (mapcar (lambda (sop)
                                 (alexandria:ensure-list (superoperator-to-component-matrices sop)))
                               superoperators)))

(defmethod initialize-instance :after ((qvm base-qvm) &rest args)
  (declare (ignore args))
  (assert (typep (number-of-qubits qvm) 'alexandria:non-negative-fixnum))
  (check-superoperators (alexandria:hash-table-values (superoperator-definitions qvm))))

;; Note: AMPLITUDES refers to the STATE-ELEMENTS of the STATE of the
;; QVM for both PURE-STATEs and DENSITY-MATRIX-STATEs. The name
;; AMPLITUDES is not technically correct for DMSs, but this name was
;; chosen and implemented throughout the QVM before
;; DENSITY-MATRIX-STATES.

(defmethod amplitudes ((qvm base-qvm))
  ;; Read the STATE-ELEMENTS from the state of the QVM.
  (state-elements (state qvm)))

(defmethod (setf amplitudes) (new-amplitudes (qvm base-qvm))
  ;; Set the STATE-ELEMENTS to the state of the QVM.
  (setf (state-elements (state qvm)) new-amplitudes))

(defun nth-amplitude (qvm n)
  "Get the Nth amplitude of the quantum virtual machine QVM."
  (aref (amplitudes qvm) n))

(defun (setf nth-amplitude) (new-value qvm n)
  "Set the Nth amplitude of the quantum virtual machine QVM."
  (setf (aref (amplitudes qvm) n) new-value))

(defun map-amplitudes (qvm f)
  "Apply the function F to the amplitudes of the QVM in standard order."
  (map nil f (amplitudes qvm))
  (values))

(defmethod reset-quantum-state ((qvm base-qvm))
  ;; Reset the quantum state of a BASE-QVM to the ground state.
  (bring-to-zero-state (amplitudes qvm))
  qvm)

(defmethod set-superoperator ((qvm base-qvm) gate-name qubits kraus-ops)
  ;; Wrap a gate defined by GATE-NAME and QUBITS in a superoperator
  ;; defined by KRAUS-OPS. When the QVM reads an instruction with
  ;; GATE-NAME on QUBITS, the superoperator made from the KRAUS-OPS
  ;; will be applied to the state of the QVM.
  ;;
  ;; Note: if we are applying superoperators, we do not want to
  ;; compile the QVM program before running it.
  (when qvm:*compile-before-running*
    (warn "SUPEROPERATOR operations will not work with compiled programs!"))
  (check-allocate-computation-space (state qvm))
  (check-kraus-ops kraus-ops)
  (setf (gethash (list gate-name qubits) (superoperator-definitions qvm))
        (kraus-list (mapcar #'ensure-superoperator kraus-ops))))

;;;;;;;;;;;;;;;;;;;;;;; PURE STATE QVM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The PURE-STATE-QVM is an implementation of a QVM that evolves pure
;;; state quantum systems. The STATE of the PURE-STATE-QVM is a
;;; PURE-STATE, in which the wavefunction of the quantum system is
;;; represented by a 2^(n qubit) vector of amplitudes. The
;;; PURE-STATE-QVM also supports superoperator applications by
;;; stochastically applying the kraus operators they are represented
;;; by to the STATE.

(defclass pure-state-qvm (base-qvm)
  ((state :reader state
          :writer %set-state
          :initarg :state
          :type (or null pure-state) ; TODO: Should we nix the NULL possibility?
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
               (wavefunction-qubits (amplitudes qvm))
               num-qubits))
      (t
       ;; If the amplitudes weren't specified, initialize to |...000>.
       ;; We explicitly zero out the memory to make sure all pages get
       ;; touched.
       (%set-state (make-instance 'pure-state :num-qubits (number-of-qubits qvm))
                   qvm)
       (bring-to-zero-state (amplitudes qvm))))
    ;; If there are SUPEROPERATOR-DEFINITIONS, allocate the
    ;; %TRIAL-AMPLITUDES of the STATE
    (when (plusp (hash-table-count (superoperator-definitions qvm)))
      (check-allocate-computation-space (state qvm)))))

(defun make-qvm (num-qubits &key (classical-memory-model quil:**empty-memory-model**)
                                 (allocation nil))
  "Make a new quantum virtual machine with NUM-QUBITS number of qubits and a classical memory size of CLASSICAL-MEMORY-SIZE bits. ALLOCATION is an optional argument with the following behavior.
    - If NULL (default), then a standard wavefunction in the Lisp heap will be allocated.
    - If STRING, then the wavefunction will be allocated as a shared memory object, accessible by that name.
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
