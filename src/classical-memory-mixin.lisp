;;;; src/classical-memory-mixin.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; The non-quantum parts of a QVM (the program, the pc, and the
;;; classical memory subsystem) don't really change from QVM to QVM,
;;; so that state is stored in an abstract mixin class.
;;;
;;; "Use composition!" I hear you shout. We use a class so that we can
;;; define the transitions on the classical instructions uniformly.

(defclass classical-memory-mixin ()
  ((classical-memory-subsystem :initarg :classical-memory-subsystem
                               :reader classical-memory-subsystem
                               :documentation "The classical memory subsystem of the QVM. No memory by default.")
   (program-counter :accessor pc
                    :initform 0
                    :documentation "The program counter.")

   (program :accessor program
            :initform #()
            :documentation "The program to be executed.")
   (gate-definitions :accessor gate-definitions
                     :initarg :gate-definitions
                     :documentation "A table mapping gate names to their GATE-instance definition."))
  (:default-initargs
   :classical-memory-subsystem (make-instance 'qvm:classical-memory-subsystem)
   ;; XXX FIXME: To be superseded by some notion of environments.
   :gate-definitions (copy-hash-table quil::**default-gate-definitions**))
  (:metaclass abstract-class)
  (:documentation "A mixin for quantum abstract machines making use of a classical memory subsystem and a program vector."))

(defmethod classical-memory-model ((qvm classical-memory-mixin)) ; Convenience
  (classical-memory-model (classical-memory-subsystem qvm)))

(defmethod classical-memories ((qvm classical-memory-mixin))     ; Convenience
  (classical-memories (classical-memory-subsystem qvm)))


(defun current-instruction (qvm)
  "What is the next instruction to be executed on the QVM?"
  (aref (program qvm) (pc qvm)))

(defun loaded-program-length (qvm)
  "Number of executable instructions in the program loaded into the QVM."
  (length (program qvm)))

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

(defun reset-classical-memory (qvm)
  "Zero out all of the classical memory of the qvm QVM."
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (root-memory-view-p v)
               (zero-out-classical-memory (memory-view-root-classical-memory v))))
           (classical-memories qvm))
  qvm)

(defun install-gates (qvm program)
  "Install the gates specified by the program PROGRAM into the QVM.

This will not clear previously installed gates from the QVM."
  ;; XXX FIXME: This should be improved by some first class definition
  ;; of environments.
  (loop :with gate-table := (gate-definitions qvm)
        :for gate-def :in (quil:parsed-program-gate-definitions program)
        :for gate := (quil:gate-definition-to-gate gate-def)
        :do (setf (gethash (quil:gate-name gate) gate-table) gate)))

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
  ;;
  ;; XXX: This is a hack! This is to accommodate the PURE-STATE-QVM.
  (ignore-errors
   (when (slot-boundp qvm 'program-compiled-p)
     (setf (slot-value qvm 'program-compiled-p) nil)))

  ;; Patch the labels. This unfortunately mutates the program. It is
  ;; what it is. (We have this CONTINUE loop so that requisite
  ;; transforms get applied as needed.)
  (handler-case
      (setf program (quil:transform 'quil::patch-labels program))
    (quil:unsatisfied-transform-dependency (c)
      (declare (ignore c))
      (invoke-restart 'continue)))

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

