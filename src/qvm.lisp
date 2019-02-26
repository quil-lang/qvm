;;;; src/qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defclass pure-state-qvm (quantum-abstract-machine)
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

   (classical-memory-subsystem :initarg :classical-memory-subsystem
                               :reader classical-memory-subsystem
                               :documentation "The classical memory subsystem of the QVM. No memory by default.")

   ;; --- Program and Definitions
   (program-counter :accessor pc
                    :initform 0
                    :documentation "The program counter.")

   (program :accessor program
            :initform #()
            :documentation "The program to be executed.")

   (program-compiled-p :accessor program-compiled-p
                       :initform nil
                       :documentation "Has the loaded program been compiled?")

   (gate-definitions :accessor gate-definitions
                     :initarg :gate-definitions
                     :documentation "A table mapping gate names to their GATE-instance definition."))
  (:default-initargs
   :classical-memory-subsystem (make-instance 'qvm:classical-memory-subsystem)
   ;; XXX FIXME: To be superseded by some notion of environments.
   :gate-definitions (copy-hash-table quil::**default-gate-definitions**))
  (:documentation "An pure-state implementation of the Quantum Abstract Machine."))

(defmethod classical-memory-model ((qvm pure-state-qvm)) ; Convenience
  (classical-memory-model (classical-memory-subsystem qvm)))

(defmethod classical-memories ((qvm pure-state-qvm))     ; Convenience
  (classical-memories (classical-memory-subsystem qvm)))

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
                                 (shared-memory nil))
  "Make a new quantum virtual machine with NUM-QUBITS number of qubits and a classical memory size of CLASSICAL-MEMORY-SIZE bits.

If SHARED-MEMORY is a string, then the wavefunction will be allocated as a shared memory object, accessible by that name."
  (check-type classical-memory-model quil:memory-model)
  (check-type shared-memory (or null string))
  (if (null shared-memory)
      (make-instance 'pure-state-qvm
                     :number-of-qubits num-qubits
                     :classical-memory-subsystem (make-instance
                                                  'classical-memory-subsystem
                                                  :classical-memory-model
                                                  classical-memory-model))
      (%make-shared-qvm shared-memory num-qubits classical-memory-model)))

(defun %make-shared-qvm (name num-qubits classical-memory-model)
  (multiple-value-bind (vec finalizer)
      (make-shared-array name (expt 2 num-qubits) 'cflonum)
    ;; Set it to |...000>
    (setf (aref vec 0) (cflonum 1))
    ;; Create the QVM object.
    (let ((qvm (make-instance 'pure-state-qvm
                              :number-of-qubits num-qubits
                              :amplitudes vec
                              :classical-memory-subsystem (make-instance
                                                           'classical-memory-subsystem
                                                           :classical-memory-model
                                                           classical-memory-model))))
      ;; When the QVM disappears, make sure the shared memory gets
      ;; deallocated too. XXX: This could cause problems if someone
      ;; copies out the shared memory.
      (tg:finalize qvm finalizer)
      ;; Return the QVM.
      qvm)))

(defun install-gates (qvm program)
  "Install the gates specified by the program PROGRAM into the QVM.

This will not clear previously installed gates from the QVM."
  ;; XXX FIXME: This should be improved by some first class definition
  ;; of environments.
  (loop :with gate-table := (gate-definitions qvm)
        :for gate-def :in (quil:parsed-program-gate-definitions program)
        :for gate := (quil:gate-definition-to-gate gate-def)
        :do (setf (gethash (quil:gate-name gate) gate-table) gate)))

;;; TODO: How should we deal with classical memory here?
(defun load-program (qvm program &key (supersede-memory-subsystem nil))
  "Load the program PROGRAM into the quantum virtual machine QVM. If SUPERSEDE-MEMORY-SUBSYSTEM is true (default: NIL), then the memory subsystem will be recomputed for the QVM based off of the program."
  (check-type program quil:parsed-program)

  (let ((qubits-needed (quil:qubits-needed program)))
    (assert (<= qubits-needed (number-of-qubits qvm))
            (qvm program)
            "Trying to load a program that requires ~D qubit~:P onto a ~
             QVM of ~D qubit~:P."
            qubits-needed
            (number-of-qubits qvm)))

  ;; Install the gates.
  (install-gates qvm program)

  ;; Make sure we forget we compiled.
  (setf (program-compiled-p qvm) nil)

  ;; Load the code vector.
  (setf (program qvm) (quil:parsed-program-executable-code program))

  ;; Possibly reset the memory subsystem.
  (when supersede-memory-subsystem
    (setf (slot-value qvm 'classical-memory-subsystem)
          (make-instance
           'classical-memory-subsystem
           :classical-memory-model
           (memory-descriptors-to-qvm-memory-model
            (quil:parsed-program-memory-definitions program)))))

  ;; Return the modified QVM.
  qvm)

(defun compile-loaded-program (qvm)
  "Compile the loaded program on the QVM QVM."
  (unless (program-compiled-p qvm)
    (setf (program qvm)
          (map 'vector (lambda (isn) (compile-instruction qvm isn)) (program qvm)))
    (setf (program-compiled-p qvm) t))
  qvm)

(defun current-instruction (qvm)
  "What is the next instruction to be executed on the QVM?"
  (aref (program qvm) (pc qvm)))

(defun loaded-program-length (qvm)
  "Number of executable instructions in the program loaded into the QVM."
  (length (program qvm)))

;;; Fundamental Manipulation of the QVM

(defun memory-ref (qvm memory-name i)
  "Get the memory named MEMORY-NAME at index I in the QVM."
  (let ((mv (gethash memory-name (classical-memories qvm))))
    (when (null mv) (error "Unknown memory ~S" memory-name))
    (memory-view-ref mv i)))

(defun (setf memory-ref) (new-value qvm memory-name i)
  "Set the memory named MEMORY-NAME at index I in the QVM to NEW-VALUE."
  (let ((mv (gethash memory-name (classical-memories qvm))))
    (when (null mv) (error "Unknown memory ~S" memory-name))
    (setf (memory-view-ref mv i) new-value)))

(defun dereference-mref (qvm mref)
  "Look up the value of the quil:memory-ref MREF in the QVM."
  (check-type mref quil:memory-ref)
  (memory-ref qvm (quil:memory-ref-name mref) (quil:memory-ref-position mref)))

(defun (setf dereference-mref) (new-value qvm mref)
  "Set the value at the quil:memory-ref MREF in the QVM to NEW-VALUE."
  (check-type mref quil:memory-ref)
  (setf (memory-ref qvm (quil:memory-ref-name mref) (quil:memory-ref-position mref))
        new-value))

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

(defun lookup-gate (qvm gate &key error)
  "Look up the definition of the gate named GATE (a symbol or string) within the QVM. Return NIL if not found.

If ERROR is T, then signal an error when the gate wasn't found."
  (let ((name (etypecase gate
                (symbol (symbol-name gate))
                (string gate))))
    (multiple-value-bind (found-gate found?)
        (gethash name (gate-definitions qvm))
      (when (and error (not found?))
        (error "Failed to find the gate named ~S" name))
      found-gate)))

(defmethod reset-quantum-state ((qvm pure-state-qvm))
    ;; We don't reset the classical state because that memory could be
    ;; shared.
    (bring-to-zero-state (amplitudes qvm))
    qvm)

(defun reset-classical-memory (qvm)
  "Zero out all of the classical memory of the qvm QVM."
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (root-memory-view-p v)
               (zero-out-classical-memory (memory-view-root-classical-memory v))))
           (classical-memories qvm))
  qvm)
