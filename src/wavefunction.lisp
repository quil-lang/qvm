;;;; wavefunction.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This is very dangerous, unfortunately. With safety > 0, we slow
;;; down 33%. With none of the optimizations in this file, we slow
;;; down >100%.
(declaim (optimize speed (safety 0) (debug 0) (space 0)))

;;;; Wavefunction manipulation

(deftype amplitude-address ()
  "An address into an array of amplitudes."
  'non-negative-fixnum)

(defun probability (amplitude)
  "Convert an amplitude into a probability."
  (expt (abs amplitude) 2))

(defun wavefunction-qubits (wavefunction)
  "The number of qubits represented by the wavefunction WAVEFUNCTION."
  (declare (type quantum-state wavefunction))
  (1- (integer-length (length wavefunction))))

(declaim (inline set-qubit-components-of-amplitude-address))
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
(declaim (notinline set-qubit-components-of-amplitude-address))

(declaim (inline map-reordered-amplitudes))
(defun map-reordered-amplitudes (starting-address function qubits)
  "Iterate through all variations of the amplitude address STARTING-ADDRESS, varying the qubits specified by the tuple QUBITS. For each newly loaded address, call the function FUNCTION.

FUNCTION should be a binary function, and will receive (1) an index running from 0 below 2^|qubits|, and (2) the varied amplitude address."
  (declare (type nat-tuple qubits)
           (type (function (non-negative-fixnum non-negative-fixnum) *) function)
           (inline set-qubit-components-of-amplitude-address))
  (let ((number-of-iterations (expt 2 (nat-tuple-cardinality qubits))))
    (dotimes (combo number-of-iterations)
      (let ((address (set-qubit-components-of-amplitude-address starting-address combo qubits)))
        (funcall function combo address)))))
(declaim (notinline map-reordered-amplitudes))

(declaim (inline map-reordered-amplitudes-in-parallel))
(defun map-reordered-amplitudes-in-parallel (starting-address function qubits)
  "Parallel version of #'MAP-REORDERED-AMPLITUDES."
  (declare (type nat-tuple qubits)
           (type (function (non-negative-fixnum non-negative-fixnum) *) function)
           (inline set-qubit-components-of-amplitude-address))
  (let ((number-of-iterations (expt 2 (nat-tuple-cardinality qubits))))
    (lparallel:pdotimes (combo number-of-iterations)
      (let ((address (set-qubit-components-of-amplitude-address starting-address combo qubits)))
        (funcall function combo address)))))
(declaim (notinline map-reordered-amplitudes-in-parallel))

(defun extract-amplitudes (wavefunction qubits starting-address)
  "Returns a column vector of amplitudes represented by the tuple of qubits QUBITS."
  (declare (type nat-tuple qubits)
           (type quantum-state wavefunction)
           (inline map-reordered-amplitudes))
  (let ((col (make-vector (expt 2 (nat-tuple-cardinality qubits)))))
    (flet ((extract (combo address)
             (setf (aref col combo) (aref wavefunction address))
             (values)))
      (declare (dynamic-extent #'extract))
      (map-reordered-amplitudes
       starting-address
       #'extract
       qubits))
    col))

(defun insert-amplitudes (wavefunction column qubits starting-address)
  "Given a column vector of amplitudes COLUMN representing the qubits QUBITS, insert this state back into WAVEFUNCTION."
  (declare (type nat-tuple qubits)
           (type quantum-state wavefunction)
           (inline map-reordered-amplitudes))
  (flet ((insert (combo address)
           (setf (aref wavefunction address) (aref column combo))
           (values)))
    (declare (dynamic-extent #'insert))
    (map-reordered-amplitudes
     starting-address
     #'insert
     qubits)))

(defun apply-operator (wavefunction operator qubits)
  "Apply the operator (given as a matrix) OPERATOR to the amplitudes of WAVEFUNCTION specified by the qubits QUBITS."
  (declare (type quantum-state wavefunction)
           (type quantum-operator operator)
           (inline map-reordered-amplitudes-in-parallel))
  ;;(declare (optimize speed (safety 1) (debug 1) (space 0)))
  (flet ((multiply-in (combo address)
           (declare (ignore combo))
           (let* ((x (extract-amplitudes wavefunction qubits address))
                  (Ux (matrix-multiply operator x)))
             (insert-amplitudes wavefunction Ux qubits address))
           (values)))
    (declare (dynamic-extent #'multiply-in))
    (map-reordered-amplitudes-in-parallel
     0
     #'multiply-in
     (nat-tuple-complement (wavefunction-qubits wavefunction) qubits)))
  wavefunction)

(defun normalize-wavefunction (wavefunction)
  "Normalize the wavefunction WAVEFUNCTION, making it a unit vector in the constituent Hilbert space."
  (declare (type quantum-state wavefunction))
  (loop :for amp :across wavefunction
        :sum (probability amp) :into square-norm
        :finally (map-into wavefunction
                           (let ((norm (sqrt square-norm)))
                             (lambda (amp)
                               (/ amp norm)))
                           wavefunction))
  wavefunction)
