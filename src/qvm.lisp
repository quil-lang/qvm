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
                     :documentation "Number of qubits being simulated.")

   (amplitudes :accessor amplitudes
               :initarg :amplitudes
               :type quantum-state
               :documentation "The unpermuted wavefunction.")

   (classical-memory-size :accessor classical-memory-size
                          :initarg :classical-memory-size
                          :type non-negative-fixnum
                          :documentation "The number of bits of classical memory.")

   ;; Currently this is just represented as an integer.
   (classical-memory :accessor classical-memory
                     :initarg :classical-memory
                     :type integer
                     :documentation "An integer representing bits of classical memory.")

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
                     ;; XXX FIXME: To be superseded by some notion of
                     ;; environments.
                     :initform (copy-hash-table quil::**default-gate-definitions**)
                     :documentation "A table mapping gate names to their GATE-instance definition."))

  (:documentation "An pure-state implementation of the Quantum Abstract Machine."))

;;; Creation and Initialization

(defmethod initialize-instance :after ((qvm pure-state-qvm) &rest args)
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qvm))
        (num-bits (classical-memory-size qvm)))
    ;; Allocate the classical memory if needed.
    (unless (slot-boundp qvm 'classical-memory)
      (setf (classical-memory qvm) (make-classical-memory num-bits)))

    ;; If the amplitudes weren't specified, initialize to |...000>.
    ;;
    ;; We explicitly zero out the memory to make sure all pages get
    ;; touched.
    (unless (and (slot-boundp qvm 'amplitudes)
                 (not (null (slot-value qvm 'amplitudes))))
      (setf (amplitudes qvm)
            (make-vector (expt 2 num-qubits)))
      (bring-to-zero-state (amplitudes qvm)))))

(defun make-qvm (num-qubits &key (classical-memory-size 64)
                                 (shared-memory nil))
  "Make a new quantum virtual machine with NUM-QUBITS number of qubits and a classical memory size of CLASSICAL-MEMORY-SIZE bits.

If SHARED-MEMORY is a string, then the wavefunction will be allocated as a shared memory object, accessible by that name."
  (check-type classical-memory-size (integer 0))
  (check-type shared-memory (or null string))
  (if (null shared-memory)
      (make-instance 'pure-state-qvm
                     :number-of-qubits num-qubits
                     :classical-memory-size classical-memory-size)
      (%make-shared-qvm shared-memory num-qubits classical-memory-size)))

(defun %make-shared-qvm (name num-qubits classical-memory-size)
  (multiple-value-bind (vec finalizer)
      (make-shared-array name (expt 2 num-qubits) 'cflonum)
    ;; Set it to |...000>
    (setf (aref vec 0) (cflonum 1))
    ;; Create the QVM object.
    (let ((qvm (make-instance 'pure-state-qvm
                              :number-of-qubits num-qubits
                              :classical-memory-size classical-memory-size
                              :amplitudes vec)))
      ;; When the QVM disappears, make sure the shared memory gets
      ;; deallocated too.
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

(defun load-program (qvm program &key append)
  "Load the program PROGRAM into the quantum virtual machine QVM. If APPEND is T (default: NIL), then the program will be appended to the currently loaded program. Otherwise, whatever program is loaded will be superseded."
  (check-type program quil:parsed-program)

  ;; Install the gates.
  (install-gates qvm program)

  ;; Make sure we forget we compiled.
  (setf (program-compiled-p qvm) nil)

  ;; Load the code vector.
  (let ((code-vector (quil:parsed-program-executable-code program)))
    (cond
      ((null append)
       (setf (program qvm) code-vector))
      (t
       (setf (program qvm)
             (concatenate 'vector (program qvm) code-vector))))
    qvm))

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

(defun check-bit-in-bounds (qvm n)
  (assert (<= 0 n (1- (classical-memory-size qvm)))
          ()
          "Attempting to access classical bit out of the range of ~
           the memory provided, which is ~D bits."
          (classical-memory-size qvm)))

(defun classical-bit (qvm n)
  "Extract the classical bit addressed by N from the quantum virtual machine QVM."
  (check-bit-in-bounds qvm n)
  (peek-bit (classical-memory qvm) n))

(defun (setf classical-bit) (new-value qvm n)
  "Set the classical bit addressed by N in the quantum virtual machine QVM to the value NEW-VALUE."
  (check-type new-value bit)
  (check-bit-in-bounds qvm n)
  (setf (classical-memory qvm)
        (poke-bit (classical-memory qvm) n new-value)))

(defun classical-bit-range (qvm br)
  "Extract the value in the bit range BR from the classical memory of QVM."
  (check-type br bit-range)
  (check-bit-in-bounds qvm (bit-range-left br))
  (check-bit-in-bounds qvm (1- (bit-range-right br)))
  (peek-bits (classical-memory qvm) br))

(defun (setf classical-bit-range) (new-value qvm br)
  "Set the value contained within the bit range BR of the classical memory of QVM to NEW-VALUE."
  (check-type new-value (integer 0))
  (check-type br bit-range)
  (check-bit-in-bounds qvm (bit-range-left br))
  (check-bit-in-bounds qvm (1- (bit-range-right br)))
  (setf (classical-memory qvm)
        (poke-bits (classical-memory qvm) br new-value)))

(defun classical-double-float (qvm br)
  "Get the double precision floating point number from the bits specified by the bit range BR from the classical memory of QVM."
  (assert (= 64 (bit-range-width br)))
  (ieee-floats:decode-float64 (classical-bit-range qvm br)))

(defun (setf classical-double-float) (new-value qvm br)
  "Set the double precision floating point number NEW-VALUE into the bits specified by the bit range BR from the classical memory of QVM."
  (assert (= 64 (bit-range-width br)))
  (check-type new-value double-float)
  (setf (classical-bit-range qvm br)
        (ieee-floats:encode-float64 new-value))
  new-value)

(defun classical-complex-double-float (qvm br)
  "Get the complex double precision floating point number from the bits specified by the bit range BR from the classical memory of QVM."
  (assert (= 128 (bit-range-width br)))
  (let ((left (bit-range-left br))
        (right (bit-range-right br)))
    (complex (classical-double-float qvm (make-bit-range left (+ left 63)))
             (classical-double-float qvm (make-bit-range (+ left 64) right)))))

(defun (setf classical-complex-double-float) (new-value qvm br)
    "Set the complex double precision floating point number NEW-VALUE into the bits specified by the bit range BR from the classical memory of QVM."
  (assert (= 128 (bit-range-width br)))
  (check-type new-value (complex double-float))
  (let ((left (bit-range-left br))
        (right (bit-range-right br))
        (re (realpart new-value))
        (im (imagpart new-value)))
    (setf (classical-double-float qvm (make-bit-range left (+ 63 left)))  re
          (classical-double-float qvm (make-bit-range (+ 64 left) right)) im)
    new-value))

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

(defun reset (qvm)
  "Perform a reset. Bring all qubits to |0>."
  (bring-to-zero-state (amplitudes qvm))
  qvm)

(defun reset-classical-memory (qvm)
  "Zero out the classical memory of the qvm QVM."
  (setf (classical-memory qvm) (make-classical-memory
                                (classical-memory-size qvm)))
  qvm)
