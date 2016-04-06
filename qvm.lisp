;;;; qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Quantum Virtual Machine

(defclass quantum-virtual-machine ()
  ((number-of-qubits :accessor number-of-qubits
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

   (program :accessor program
            :initarg :program
            :initform nil
            :documentation "The program to be executed.")

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
    ;; Allocate the classical memory if needed.
    (when (null (classical-memory qvm))
      (setf (classical-memory qvm)
            (make-array (* 8 num-octets)
                        :element-type 'bit
                        :initial-element 0)))

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
  (if (null append)
      (setf (program qvm) program)
      (setf (program qvm) (append (program qvm) program))))


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
  (sbit (classical-memory qvm) n))

(defun (setf classical-bit) (new-value qvm n)
  "Set the classical bit addressed by N in the quantum virtual machine QVM to the value NEW-VALUE."
  (check-type new-value bit)
  (setf (sbit (classical-memory qvm) n) new-value))

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

