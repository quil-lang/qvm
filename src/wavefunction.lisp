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

(declaim (ftype (function (cflonum) flonum) probability))
(defun-inlinable probability (amplitude)
  "Convert an amplitude into a probability."
  (declare (type cflonum amplitude))
  (let ((re (realpart amplitude))
        (im (imagpart amplitude)))
    (declare (type flonum re im))
    (+ (* re re) (* im im))))


(defun wavefunction-qubits (wavefunction)
  "The number of qubits represented by the wavefunction WAVEFUNCTION."
  (declare (type quantum-state wavefunction))
  (1- (integer-length (length wavefunction))))


(defun-inlinable set-qubit-components-of-amplitude-address (address flags qubits)
  "Set the amplitude address to the bits within the non-negative integer FLAGS for the corresponding tuple of qubit indices QUBITS. (Ordered LSB to MSB.)"
  (declare (type nat-tuple qubits)
           (type non-negative-fixnum flags)
           (type amplitude-address address))
  (do-nat-tuple (i qubit qubits)
    (if (logbitp i flags)
        (setf address (dpb 1 (byte 1 qubit) address))
        (setf address (dpb 0 (byte 1 qubit) address))))
  address)


(defun-inlinable map-reordered-amplitudes (starting-address function qubits)
  "Iterate through all variations of the amplitude address STARTING-ADDRESS, varying the qubits specified by the tuple QUBITS. For each newly loaded address, call the function FUNCTION.

FUNCTION should be a binary function, and will receive (1) an index running from 0 below 2^|qubits|, and (2) the varied amplitude address."
  (declare (type nat-tuple qubits)
           (type (function (non-negative-fixnum non-negative-fixnum) *) function)
           (inline set-qubit-components-of-amplitude-address))
  (let ((number-of-iterations (expt 2 (nat-tuple-cardinality qubits))))
    (dotimes (combo number-of-iterations)
      (let ((address (set-qubit-components-of-amplitude-address starting-address combo qubits)))
        (funcall function combo address)))))


(defun-inlinable map-reordered-amplitudes-in-parallel-truly (starting-address function qubits)
  "Parallel version of #'MAP-REORDERED-AMPLITUDES."
  (declare (type nat-tuple qubits)
           (type (function (non-negative-fixnum non-negative-fixnum) *) function)
           (inline set-qubit-components-of-amplitude-address))
  (let ((number-of-iterations (expt 2 (nat-tuple-cardinality qubits))))
    (lparallel:pdotimes (combo number-of-iterations)
      (let ((address (set-qubit-components-of-amplitude-address starting-address combo qubits)))
        (funcall function combo address)))))


(defparameter *qubits-required-for-parallelization* 19
  "The number of qubits required of a quantum state before it gets operated on in parallel.")

(defun-inlinable map-reordered-amplitudes-in-parallel (starting-address function qubits)
  "Parallel version of #'MAP-REORDERED-AMPLITUDES, when the number of qubits is large enough."
  (if (<= *qubits-required-for-parallelization* (nat-tuple-cardinality qubits))
      (map-reordered-amplitudes-in-parallel-truly starting-address function qubits)
      (map-reordered-amplitudes starting-address function qubits)))

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

(defmacro with-modified-amplitudes ((col wavefunction qubits starting-address) &body body)
  "Given a quantum state WAVEFUNCTION, a NAT-TUPLE of qubits QUBITS, and a starting address STARTING-ADDRESS, bind dynamically to the symbol COL the list of amplitudes that correspond to the qubits provided."
  (check-type col symbol)
  (alexandria:with-gensyms (combo address insert extract)
    (alexandria:once-only (wavefunction qubits starting-address)
      `(locally (declare (type nat-tuple ,qubits)
                         (type quantum-state ,wavefunction)
                         (inline map-reordered-amplitudes))
         (let ((,col (make-vector (expt 2 (nat-tuple-cardinality ,qubits)))))
           (declare (type quantum-state ,col)
                    (dynamic-extent ,col))
           (flet ((,extract (,combo ,address)
                    (setf (aref ,col ,combo) (aref ,wavefunction ,address))
                    (values))
                  (,insert (,combo ,address)
                    (setf (aref ,wavefunction ,address) (aref ,col ,combo))
                    (values)))
             (declare (dynamic-extent #',extract #',insert))
             ;; Extract
             (map-reordered-amplitudes
              ,starting-address
              #',extract
              ,qubits)
             ;; Execute BODY
             (progn
               ,@body)
             ;; Insert
             (map-reordered-amplitudes
              ,starting-address
              #',insert
              ,qubits)
             (values)))))))

(defun apply-operator (wavefunction operator qubits)
  "Apply the operator (given as a matrix) OPERATOR to the amplitudes of WAVEFUNCTION specified by the qubits QUBITS."
  (declare (type quantum-state wavefunction)
           (type quantum-operator operator)
           (inline map-reordered-amplitudes-in-parallel))
  (flet ((fast-multiply-in (combo address)
           (declare (ignore combo))
           (with-modified-amplitudes (col wavefunction qubits address)
             (let ((result (make-vector (expt 2 (nat-tuple-cardinality qubits)))))
               (declare (type quantum-state result)
                        (dynamic-extent result))
               (matrix-multiply operator col result)

               #+ccl                    ; CCL bug.
               (loop :for i :below (length col)
                     :do (setf (aref col i) (aref result i)))

               #-ccl
               (replace col result)

               (values))))

         ;; This is the old way of doing it. It is kept for posterity
         ;; and pedagogy.
         (multiply-in (combo address)
           (declare (ignore combo))
           (let* ((x (extract-amplitudes wavefunction qubits address))
                  (Ux (matrix-multiply operator x)))
             (insert-amplitudes wavefunction Ux qubits address))
           (values)))
    (declare (ignore #'multiply-in)
             (dynamic-extent #'multiply-in #'fast-multiply-in))
    (map-reordered-amplitudes-in-parallel
     0
     #'fast-multiply-in
     (nat-tuple-complement (wavefunction-qubits wavefunction) qubits)))
  wavefunction)

(defun normalize-wavefunction (wavefunction)
  "Normalize the wavefunction WAVEFUNCTION, making it a unit vector in the constituent Hilbert space."
  (declare (type quantum-state wavefunction)
           (inline probability)
           (optimize speed (safety 1)))
  ;; Mutate the wavefunction.
  (let ((norm (flonum 0)))
    (declare (type flonum norm))
    ;; Compute the square norm.
    (loop :for x :of-type cflonum :across wavefunction
          :do (incf norm (probability x)))

    ;; Compute the norm.
    (setf norm (sqrt (the (flonum 0) norm)))

    ;; Normalize the wavefunction
    (loop :for i :below (length wavefunction)
          :for x :of-type cflonum :across wavefunction
          :do (setf (aref wavefunction i) (/ x norm)))

    ;; Return the wavefunction.
    wavefunction))
