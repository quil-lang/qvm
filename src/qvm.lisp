;;;; qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Quantum Virtual Machine

(defvar *default-gate-definitions* (make-hash-table :test 'equal)
  ;; This is populated later when gates are loaded.
  "A table of default gate definitions.")

(defclass quantum-virtual-machine ()
  (
   ;; --- Machine state
   (number-of-qubits :accessor number-of-qubits
                     :initarg :number-of-qubits
                     :documentation "Number of qubits being simulated.")

   (amplitudes :accessor amplitudes
               :initarg :amplitudes
               :initform nil
               :documentation "The estimated wavefunction.")

   (classical-memory-size :accessor classical-memory-size
                          :initarg :classical-memory-size
                          :documentation "The number of octets of classical memory.")

   ;; Currently this is just represented as a bit vector.
   (classical-memory :accessor classical-memory
                     :initarg :classical-memory
                     :initform nil
                     :documentation "Bit vector of classical memory.")

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
                     :documentation "A table mapping gate names to their GATE-instance definition.")

   ;; --- Bookkeeping
   ;; The following two additional registers are used primarily for
   ;; controlled iteration through the amplitude space.

   (qubit-numbers :accessor qubit-numbers
                  :initform (make-nat-tuple)
                  :documentation "Valid qubit indexes represented as a NAT-TUPLE.")

   (amplitude-address :accessor amplitude-address
                      :documentation "Non-negative integer address into the amplitudes."))

  (:documentation "An implementation of the Quantum Abstract Machine."))


;;; Creation and Initialization

(defmethod initialize-instance :after ((qvm quantum-virtual-machine) &rest args)
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qvm))
        (num-octets (classical-memory-size qvm)))
    (declare (ignore num-octets))
    ;; Allocate the classical memory if needed.
    (when (null (classical-memory qvm))
      (setf (classical-memory qvm) 0))

    ;; If the amplitudes weren't specified, initialize to |0...>.
    (when (null (amplitudes qvm))
      (setf (amplitudes qvm)
            (make-vector (expt 2 num-qubits) 1))) ; Pr[|00..>] = 1

    ;; Initilize list of valid qubit indices.
    (dotimes (qubit num-qubits)
      (setf (qubit-numbers qvm)
            (nat-tuple-add (qubit-numbers qvm) qubit)))

    ;; Initialize address register for amplitudes.
    (setf (amplitude-address qvm) 0)))

(defun make-qvm (num-qubits &key (classical-memory-size 8))
  "Make a new quantum virtual machine with NUM-QUBITS number of qubits and a classical memory size of CLASSICAL-MEMORY-SIZE octets."
  (check-type classical-memory-size (integer 0))
  (make-instance 'quantum-virtual-machine
                 :number-of-qubits num-qubits
                 :classical-memory-size classical-memory-size))

(defun load-program (qvm program &key append)
  "Load the program PROGRAM into the quantum virtual machine QVM. If APPEND is T (default: NIL), then the program will be appended to the currently loaded program. Otherwise, whatever program is loaded will be superseded."
  (check-type program quil:parsed-program)
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
  (length (program qvm)))

;;; Fundamental Manipulation of the QVM

(defun set-qubit-components-of-amplitude-address (qvm flags qubits)
  "Set the amplitude address of QVM to the bits within the non-negative integer FLAGS for the corresponding tuple of qubit indices QUBITS. (Ordered LSB to MSB.)

Note: This is stateful, and will retain any bits untouched by this procedure."
  (declare (type nat-tuple qubits)
           (type non-negative-fixnum flags))
  (let ((x (amplitude-address qvm)))
    (declare (type non-negative-fixnum x))
    (do-nat-tuple (i qubit qubits)
      (if (logbitp i flags)
          (setf x (dpb 1 (byte 1 qubit) x))
          (setf x (dpb 0 (byte 1 qubit) x))))
    (setf (amplitude-address qvm) x)))

(defun map-relevant-amplitudes (qvm function qubits)
  "Iterate through all variations of the currently loaded amplitude address in the QVM, varying the qubits specified by the tuple QUBITS. For each newly loaded address, call the function FUNCTION.

FUNCTION should be a unary function, and will receive an index running from 0 below 2^|qubits|.

N.B. This function does not reset the amplitude address of the QVM."
  (declare (type nat-tuple qubits))
  (let ((number-of-iterations (expt 2 (nat-tuple-cardinality qubits))))
    (dotimes (combo number-of-iterations)
      (set-qubit-components-of-amplitude-address qvm combo qubits)
      (funcall function combo))))

(defun addressed-amplitude (qvm)
  "Returns the amplitude currently addressed by QVM."
  (aref (amplitudes qvm) (amplitude-address qvm)))

(defun (setf addressed-amplitude) (new-value qvm)
  "Sets the amplitude currently addressed by QVM to the complex NEW-VALUE."
  (setf (aref (amplitudes qvm) (amplitude-address qvm))
        new-value))

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

(defun extract-amplitudes (qvm qubits)
  "Returns a column vector of amplitudes represented by the tuple of qubits QUBITS."
  (declare (type nat-tuple qubits))
  (let ((col (make-array (expt 2 (nat-tuple-cardinality qubits)))))
    (map-relevant-amplitudes
     qvm
     (lambda (combo)
       (setf (aref col combo) (addressed-amplitude qvm)))
     qubits)
    col))

(defun insert-amplitudes (qvm column qubits)
  "Given a column vector of amplitudes COLUMN representing the qubits QUBITS, insert this state back into the quantum virtual machine QVM."
  (declare (type nat-tuple qubits))
  (map-relevant-amplitudes
   qvm
   (lambda (combo)
     (setf (addressed-amplitude qvm) (aref column combo)))
   qubits))

;; Equivalent to a Extract -> Multiply -> Insert
;;
;; Uses temporary stack space instead of allocating extra lists.
(defun matrix-multiply-in-place (qvm matrix qubits-to-vary)
  (let* ((matrix-size (array-dimension matrix 0))
         (result (make-array matrix-size)))
    (declare (type (simple-array t (*)) result)
             (type (integer 1 32) matrix-size)
             (dynamic-extent result))
    (dotimes (i matrix-size)
      (let ((element 0.0d0))
        (map-relevant-amplitudes
         qvm
         (lambda (j)
           (incf element (* (aref matrix i j)
                            (addressed-amplitude qvm))))
         qubits-to-vary)
        (setf (aref result i) element)))
    ;; Insert.
    (map-relevant-amplitudes
     qvm
     (lambda (j)
       (setf (addressed-amplitude qvm) (aref result j)))
     qubits-to-vary))
  qvm)

(defun apply-operator (qvm operator qubits)
  "Apply the operator (given as a matrix) OPERATOR to the amplitudes of the QVM specified by the qubits QUBITS."
  (map-relevant-amplitudes
   qvm
   (lambda (combo)
     (declare (ignore combo))
     (let* ((x (extract-amplitudes qvm qubits))
            (Ux (matrix-multiply operator x)))
       (insert-amplitudes qvm Ux qubits)))
   (nat-tuple-difference (qubit-numbers qvm) qubits))
  qvm)

(defun probability (amplitude)
  "Convert an amplitude into a probability."
  (expt (abs amplitude) 2))

(defun state-probabilities (qvm qubits)
  "Returns a list of the probabilities for all combinations for the given qubits represented as a tuple. The output will be in binary order."
  (let ((probabilities nil)
        (other-qubits (nat-tuple-difference (qubit-numbers qvm) qubits)))
    (map-relevant-amplitudes
     qvm
     (lambda (combo)
       (declare (ignore combo))
       (push (let ((probability 0.0d0))
               (map-relevant-amplitudes
                qvm
                (lambda (combo)
                  (declare (ignore combo))
                  (incf probability (probability (addressed-amplitude qvm))))
                other-qubits)
               probability)
             probabilities))
     qubits)
    ;; Return the probabilities.
    (nreverse probabilities)))

(defun lookup-gate (qvm gate)
  "Look up the definition of the gate named GATE (a symbol or string) within the QVM. Return NIL if not found."
  (let ((name (etypecase gate
                (symbol (symbol-name gate))
                (string gate))))
    (values (gethash name (gate-definitions qvm)))))
