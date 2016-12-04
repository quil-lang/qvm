;;;; qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defclass quantum-virtual-machine ()
  (
   ;; --- Machine state
   (number-of-qubits :accessor number-of-qubits
                     :initarg :number-of-qubits
                     :documentation "Number of qubits being simulated.")

   (amplitudes :accessor amplitudes
               :initarg :amplitudes
               :documentation "The (estimated) wavefunction.")

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
                     :initform *default-gate-definitions*
                     :documentation "A table mapping gate names to their GATE-instance definition."))

  (:documentation "An implementation of the Quantum Abstract Machine."))


;;; Creation and Initialization

(defmethod initialize-instance :after ((qvm quantum-virtual-machine) &rest args)
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qvm))
        (num-bits (classical-memory-size qvm)))
    (declare (ignore num-bits))
    ;; Allocate the classical memory if needed. This is represented as
    ;; an integer so we don't need to do an explicit allocation.
    (unless (slot-boundp qvm 'classical-memory)
      (setf (classical-memory qvm) 0))

    ;; If the amplitudes weren't specified, initialize to |0...>.
    (unless (slot-boundp qvm 'amplitudes)
      (setf (amplitudes qvm)
            (make-vector (expt 2 num-qubits) 1))))) ; Pr[|00..>] = 1

(defun make-qvm (num-qubits &key (classical-memory-size 64))
  "Make a new quantum virtual machine with NUM-QUBITS number of qubits and a classical memory size of CLASSICAL-MEMORY-SIZE bits."
  (check-type classical-memory-size (integer 0))
  (make-instance 'quantum-virtual-machine
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

(defun nth-amplitude (qvm n)
  "Get the Nth amplitude of the quantum virtual machine QVM."
  (aref (amplitudes qvm) n))

(defun (setf nth-amplitude) (new-value qvm n)
  "Set the Nth amplitude of the quantum virtual machine QVM."
  (setf (aref (amplitudes qvm) n) new-value))

(defun classical-bit (qvm n)
  "Extract the classical bit addressed by N from the quantum virtual machine QVM."
  (ldb (byte 1 n) (classical-memory qvm)))

(defun (setf classical-bit) (new-value qvm n)
  "Set the classical bit addressed by N in the quantum virtual machine QVM to the value NEW-VALUE."
  (check-type new-value bit)
  (setf (ldb (byte 1 n) (classical-memory qvm)) new-value))

(deftype bit-range ()
  "Representation of a range of bits."
  `(cons (integer 0) (integer 1)))

(defun make-bit-range (a b)
  "Construct a new bit range whose endpoints are A and B."
  (check-type a (integer 0))
  (check-type b (integer 1))
  (assert (< a b))
  (cons a b))

(defun bit-range-left (br)
  "Get the left endpoint of the bit range BR."
  (car br))

(defun bit-range-right (br)
  "Get the right endpoint of the bit range BR."
  (cdr br))

(defun bit-range-width (br)
  "Compute the width of the bit range BR."
  (1+ (- (bit-range-right br) (bit-range-left br))))

(defun classical-bit-range (qvm br)
  "Extract the value in the bit range BR from the classical memory of QVM."
  (check-type br bit-range)
  (ldb (byte (bit-range-width br) (bit-range-left br))
       (classical-memory qvm)))

(defun (setf classical-bit-range) (new-value qvm br)
  "Set the value contained within the bit range BR of the classical memory of QVM to NEW-VALUE."
  (check-type new-value (integer 0))
  (check-type br bit-range)
  (let ((range-width (bit-range-width br))
        (len (integer-length new-value)))
    (assert (<= len range-width) (new-value)
            "The value being set in the bit range [~D-~D] exceeds the width of the range."
            (bit-range-left br)
            (bit-range-right br))
    (setf (ldb (byte range-width (bit-range-left br))
               (classical-memory qvm))
          new-value)))

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
  (map-into (amplitudes qvm) (constantly (cflonum 0)))
  (setf (aref (amplitudes qvm) 0) (cflonum 1))
  qvm)

;;; These are useful for debugging and other classical execution. They
;;; are a particular feature of this implementation, not a part of the
;;; specification of the QAM/QIL.

(defun print-amplitudes (qvm &optional string)
  "Print the amplitudes nicely, prepended with the optional string STRING."
  (let ((amplitudes
          (coerce (amplitudes qvm) 'list)))
    (format t "~@[~A: ~]~{~A~^, ~}~%" string amplitudes)
    ;; Return the qvm.
    qvm))

(defun print-probabilities (qvm &optional string)
  "Print the probabilities nicely, prepended with the optional string STRING."
  (let ((probabilities
          (map 'list #'probability (amplitudes qvm))))
    (format t "~@[~A: ~]~{~5F~^, ~}~%" string probabilities)
    ;; Return the qvm.
    qvm))

