;;;; src/qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defclass pure-state-qvm (quantum-abstract-machine)
  (
   ;; --- Machine state
   (number-of-qubits :accessor number-of-qubits
                     :initarg :number-of-qubits
                     :documentation "Number of qubits being simulated.")

   (qubit-permutation :accessor qubit-permutation
                      :initarg :qubit-permutation
                      :documentation "The permutation on the qubits.")

   (amplitudes :accessor amplitudes
               :initarg :amplitudes
               :documentation "The unpermuted wavefunction.")

   (classical-memory-size :accessor classical-memory-size
                          :initarg :classical-memory-size
                          :documentation "The number of bits of classical memory.")

   ;; Currently this is just represented as an integer.
   (classical-memory :accessor classical-memory
                     :initarg :classical-memory
                     :documentation "An integer representing bits of classical memory.")

   ;; --- Program and Definitions
   (program-counter :accessor pc
                    :initform 0
                    :documentation "The program counter.")

   (program :accessor program
            :initform #()
            :documentation "The program to be executed.")

   (gate-definitions :accessor gate-definitions
                     :initarg :gate-definitions
                     :initform (copy-hash-table *default-gate-definitions*)
                     :documentation "A table mapping gate names to their GATE-instance definition."))

  (:documentation "An pure-state implementation of the Quantum Abstract Machine with SWAP optimization."))

;;; Creation and Initialization

(defmethod initialize-instance :after ((qvm pure-state-qvm) &rest args)
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qvm))
        (num-bits (classical-memory-size qvm)))
    ;; Allocate the classical memory if needed.
    (unless (slot-boundp qvm 'classical-memory)
      (setf (classical-memory qvm) (make-classical-memory num-bits)))

    ;; Initialize the permutation to the identity.
    (unless (slot-boundp qvm 'qubit-permutation)
      (setf (qubit-permutation qvm) (make-identity-permutation num-qubits)))
    
    ;; If the amplitudes weren't specified, initialize to |...000>.
    (unless (slot-boundp qvm 'amplitudes)
      (setf (amplitudes qvm)
            (make-vector (expt 2 num-qubits) 1)))))

(defun make-qvm (num-qubits &key (classical-memory-size 64))
  "Make a new quantum virtual machine with NUM-QUBITS number of qubits and a classical memory size of CLASSICAL-MEMORY-SIZE bits."
  (check-type classical-memory-size (integer 0))
  (make-instance 'pure-state-qvm
                 :number-of-qubits num-qubits
                 :classical-memory-size classical-memory-size))

(defun install-gates (qvm program)
  "Install the gates specified by the program PROGRAM into the QVM.

This will not clear previously installed gates from the QVM."
  (loop :with gate-table := (gate-definitions qvm)
        :for gate-def :in (quil:parsed-program-gate-definitions program)
        :for gate := (gate-definition-to-gate gate-def)
        :do (setf (gethash (gate-name gate) gate-table) gate)))

(defun load-program (qvm program &key append)
  "Load the program PROGRAM into the quantum virtual machine QVM. If APPEND is T (default: NIL), then the program will be appended to the currently loaded program. Otherwise, whatever program is loaded will be superseded."
  (check-type program quil:parsed-program)

  ;; Install the gates.
  (install-gates qvm program)

  ;; Load the code vector.
  (let ((code-vector (quil:parsed-program-executable-code program)))
    (cond
      ((null append)
       ;; TODO: Install gates.
       (setf (program qvm) code-vector))
      (t
       (setf (program qvm)
             (concatenate 'vector (program qvm) code-vector))))
    qvm))

(defun current-instruction (qvm)
  "What is the next instruction to be executed on the QVM?"
  (aref (program qvm) (pc qvm)))

(defun loaded-program-length (qvm)
  "Number of executable instructions in the program loaded into the QVM."
  (length (program qvm)))

;;; Fundamental Manipulation of the QVM

(declaim (inline permuted-qubit))
(defun permuted-qubit (qvm logical-qubit)
  "What physical qubit does the logical qubit QUBIT refer to in the quantum virtual machine QVM?"
  (aref (qubit-permutation qvm) logical-qubit))

(defun swap-qubits (qvm qubit1 qubit2)
  "Swap the logical qubits QUBIT1 and QUBIT2 in the quantum virtual machine QVM."
  (let ((perm (qubit-permutation qvm)))
    (rotatef (aref perm qubit1)
             (aref perm qubit2))))

(defun nth-amplitude (qvm n)
  "Get the Nth amplitude of the quantum virtual machine QVM."
  (aref (amplitudes qvm) n))

(defun (setf nth-amplitude) (new-value qvm n)
  "Set the Nth amplitude of the quantum virtual machine QVM."
  (setf (aref (amplitudes qvm) n) new-value))

(defun map-amplitudes (qvm f)
  "Apply the function F to the amplitudes of the quantum virtual machine QVM in standard order."
  (let ((amps (amplitudes qvm)))
    (map-reordered-amplitudes
     0
     (lambda (i addr)
       (declare (ignore i))
       (funcall f (aref amps addr)))
     (permutation-to-nat-tuple (qubit-permutation qvm))))
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

(defun state-probabilities (qvm qubits)
  "Returns a list of the probabilities for all combinations for the given qubits represented as a tuple. The output will be in binary order."
  (let ((probabilities nil)
        (other-qubits (nat-tuple-complement (number-of-qubits qvm) qubits)))
    (map-reordered-amplitudes
     qvm
     (lambda (combo address)
       (declare (ignore combo address))
       (push (let ((probability 0.0d0))
               (map-reordered-amplitudes
                qvm
                (lambda (combo address)
                  (declare (ignore combo))
                  (incf probability (probability (nth-amplitude qvm address))))
                other-qubits)
               probability)
             probabilities))
     qubits)
    ;; Return the probabilities.
    (nreverse probabilities)))

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
  (fill (amplitudes qvm) (cflonum 0))
  (setf (aref (amplitudes qvm) 0) (cflonum 1))
  (setf (qubit-permutation qvm) (make-identity-permutation
                                 (number-of-qubits qvm)))
  qvm)

(defun reset-classical-memory (qvm)
  "Zero out the classical memory of the qvm QVM."
  (setf (classical-memory qvm) (make-classical-memory
                                (classical-memory-size qvm)))
  qvm)

