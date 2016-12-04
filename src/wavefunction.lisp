;;;; wavefunction.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Wavefunction manipulation

(deftype amplitude-address ()
  "An address into an array of amplitudes."
  'non-negative-fixnum)

(defun probability (amplitude)
  "Convert an amplitude into a probability."
  (expt (abs amplitude) 2))

(defun wavefunction-qubits (wavefunction)
  "The number of qubits represented by the wavefunction WAVEFUNCTION."
  (1- (integer-length (length wavefunction))))

(defun set-qubit-components-of-amplitude-address (address flags qubits)
  "Set the amplitude address to the bits within the non-negative integer FLAGS for the corresponding tuple of qubit indices QUBITS. (Ordered LSB to MSB.)"
  (declare (type nat-tuple qubits)
           (type non-negative-fixnum flags)
           (type amplitude-address address))
  (do-nat-tuple (i qubit qubits)
    (if (logbitp i flags)
        (setf address (dpb 1 (byte 1 qubit) address))
        (setf address (dpb 0 (byte 1 qubit) address))))
  address)

(defun map-reordered-amplitudes (starting-address function qubits)
  "Iterate through all variations of the amplitude address STARTING-ADDRESS, varying the qubits specified by the tuple QUBITS. For each newly loaded address, call the function FUNCTION.

FUNCTION should be a binary function, and will receive (1) an index running from 0 below 2^|qubits|, and (2) the varied amplitude address."
  (declare (type nat-tuple qubits))
  (let ((number-of-iterations (expt 2 (nat-tuple-cardinality qubits))))
    (dotimes (combo number-of-iterations)
      (let ((address (set-qubit-components-of-amplitude-address starting-address combo qubits)))
        (funcall function combo address)))))

(defun extract-amplitudes (wavefunction qubits starting-address)
  "Returns a column vector of amplitudes represented by the tuple of qubits QUBITS."
  (declare (type nat-tuple qubits))
  (let ((col (make-vector (expt 2 (nat-tuple-cardinality qubits)))))
    (map-reordered-amplitudes
     starting-address
     (lambda (combo address)
       (setf (aref col combo) (aref wavefunction address)))
     qubits)
    col))

(defun insert-amplitudes (wavefunction column qubits starting-address)
  "Given a column vector of amplitudes COLUMN representing the qubits QUBITS, insert this state back into WAVEFUNCTION."
  (declare (type nat-tuple qubits))
  (map-reordered-amplitudes
   starting-address
   (lambda (combo address)
     (setf (aref wavefunction address) (aref column combo)))
   qubits))

(defun apply-operator (wavefunction operator qubits)
  "Apply the operator (given as a matrix) OPERATOR to the amplitudes of WAVEFUNCTION specified by the qubits QUBITS."
  (map-reordered-amplitudes
   0
   (lambda (combo address)
     (declare (ignore combo))
     (let* ((x (extract-amplitudes wavefunction qubits address))
            (Ux (matrix-multiply operator x)))
       (insert-amplitudes wavefunction Ux qubits address)))
   (nat-tuple-complement (wavefunction-qubits wavefunction) qubits))
  wavefunction)

(defun normalize-wavefunction (wavefunction)
  "Normalize the wavefunction WAVEFUNCTION, making it a unit vector in the constituent Hilbert space."
  (loop :for amp :across wavefunction
        :sum (probability amp) :into square-norm
        :finally (map-into wavefunction
                           (let ((norm (sqrt square-norm)))
                             (lambda (amp)
                               (/ amp norm)))
                           wavefunction))
  wavefunction)
