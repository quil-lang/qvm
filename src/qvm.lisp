;;;; src/qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defclass pure-state-qvm (quantum-abstract-machine classical-memory-mixin)
  (
   ;; --- Machine state
   (number-of-qubits :reader number-of-qubits
                     :initarg :number-of-qubits
                     :type non-negative-fixnum
                     :initform (error ":NUMBER-OF-QUBITS is a required initarg ~
                                       to PURE-STATE-QVM")
                     :documentation "Number of qubits being simulated.")

   ;; We allow AMPLITUDES to be written to so we can do fancy
   ;; memory-saving things.
   (amplitudes :accessor amplitudes
               :initarg :amplitudes
               :type (or null quantum-state)
               :documentation "The unpermuted wavefunction in standard order.")

   (program-compiled-p :accessor program-compiled-p
                       :initform nil
                       :documentation "Has the loaded program been compiled?"))
  (:documentation "An pure-state implementation of the Quantum Abstract Machine."))

;;; Creation and Initialization

(defmethod initialize-instance :after ((qvm pure-state-qvm) &rest args)
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qvm)))
    (cond
      ((and (slot-boundp qvm 'amplitudes)
            (not (null (slot-value qvm 'amplitudes))))
       ;; Check that amplitudes is of the right type.
       (check-type (amplitudes qvm) quantum-state)
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
       ;;
       ;; We explicitly zero out the memory to make sure all pages get
       ;; touched.
       (setf (slot-value qvm 'amplitudes) (make-lisp-cflonum-vector (expt 2 num-qubits)))
       (bring-to-zero-state (amplitudes qvm))))))


(defun make-qvm (num-qubits &key (classical-memory-model quil:**empty-memory-model**)
                                 (allocation nil))
  "Make a new quantum virtual machine with NUM-QUBITS number of qubits and a classical memory size of CLASSICAL-MEMORY-SIZE bits.

ALLOCATION is an optional argument with the following behavior.

    - If it's NULL (default), then a standard wavefunction in the Lisp heap will be allocated.

    - If it's a STRING, then the wavefunction will be allocated as a shared memory object, accessible by that name.

    - Otherwise, it's assumed to be an object that is compatible with the ALLOCATION-LENGTH and ALLOCATE-VECTOR methods
"
  (check-type num-qubits unsigned-byte)
  (check-type classical-memory-model quil:memory-model)
  (let ((allocation
          (etypecase allocation
            (null
             (make-instance 'lisp-allocation :length (expt 2 num-qubits)))
            (string
             (make-instance 'posix-shared-memory-allocation :length (expt 2 num-qubits)
                                                            :name allocation))
            (t
             (assert (= (allocation-length allocation) (expt 2 num-qubits)))
             allocation))))
    (multiple-value-bind (amplitudes finalizer)
        (allocate-vector allocation)
      ;; Ensure we start in |...000>.
      (bring-to-zero-state amplitudes)
      ;; Make the QVM.
      (let ((qvm (make-instance 'pure-state-qvm
                                :number-of-qubits num-qubits
                                :amplitudes amplitudes
                                :classical-memory-subsystem
                                (make-instance 'classical-memory-subsystem
                                               :classical-memory-model
                                               classical-memory-model))))
        ;; When the QVM disappears, make sure the shared memory gets
        ;; deallocated too. XXX: This could cause problems if someone
        ;; copies out the shared memory. We should enforce the use of
        ;; MAP-AMPLITUDES and AMPLITUDE-REF and the like.
        (tg:finalize qvm finalizer)
        ;; Return the QVM.
        qvm))))

(defmethod compile-loaded-program ((qvm pure-state-qvm))
  "Compile the loaded program on the QVM QVM."
  (unless (program-compiled-p qvm)
    (when *fuse-gates-during-compilation*
      (setf (program qvm) (quil::fuse-gates-in-executable-code (program qvm))))
    (when *compile-measure-chains*
      (setf (program qvm) (compile-measure-chains (program qvm) (number-of-qubits qvm))))
    (setf (program qvm)
          (map 'vector (lambda (isn) (compile-instruction qvm isn)) (program qvm)))
    (setf (program-compiled-p qvm) t))
  qvm)

;;; Fundamental Manipulation of the QVM


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

(defmethod reset-quantum-state ((qvm pure-state-qvm))
  ;; We don't reset the classical state because that memory could be
  ;; shared.
  (bring-to-zero-state (amplitudes qvm))
  qvm)
