;;;; src/wavefunction.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This is very dangerous, unfortunately. With safety > 0, we slow
;;; down 33%. With none of the optimizations in this file, we slow
;;; down >100%. It might be worth doing a finer-grained study of the
;;; functions to optimize.
(declaim #.*optimize-dangerously-fast*)

;;;; Wavefunction manipulation

(deftype amplitude-address ()
  "An address into an array of amplitudes."
  `(integer 0 (,(expt 2 +max-nat-tuple-cardinality+))))

(declaim (ftype (function (cflonum) flonum) probability)
         (inline probability))
(defun probability (amplitude)
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
             nil))
      (declare (dynamic-extent #'extract))
      (map-reordered-amplitudes
       starting-address
       #'extract
       qubits))
    col))

(defun insert-amplitudes (wavefunction column qubits starting-address)
  "Given a column vector of amplitudes COLUMN representing the qubits QUBITS, insert this state back into WAVEFUNCTION."
  (declare (type nat-tuple qubits)
           (type quantum-state wavefunction column)
           (inline map-reordered-amplitudes))
  (flet ((insert (combo address)
           (setf (aref wavefunction address) (aref column combo))
           nil))
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
                    nil)
                  (,insert (,combo ,address)
                    (setf (aref ,wavefunction ,address) (aref ,col ,combo))
                    nil))
             (declare (dynamic-extent #',extract #',insert)
                      (inline ,extract ,insert))
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

             nil))))))

(defun-inlinable apply-operator (operator wavefunction qubits)
  "Apply an operator OPERATOR to the amplitudes of WAVEFUNCTION specified by the qubits QUBITS. OPERATOR shall be a unary function taking a QUANTUM-STATE as an argument and modifying it."
  (declare (type quantum-state wavefunction)
           (type (function (quantum-state) *) operator)
           (inline map-reordered-amplitudes-in-parallel))
  (flet ((fast-multiply-in (combo address)
           (declare (ignore combo))
           (with-modified-amplitudes (col wavefunction qubits address)
             (funcall operator col)))

         ;; This is the old way of doing it. It is kept for posterity
         ;; and pedagogy.
         #+#:unused
         (multiply-in (combo address)
           (declare (ignore combo))
           (let* ((x (extract-amplitudes wavefunction qubits address))
                  (Ux (matrix-multiply operator x)))
             (insert-amplitudes wavefunction Ux qubits address))
           nil))
    (declare (dynamic-extent #'fast-multiply-in)
             (inline fast-multiply-in))
    (map-reordered-amplitudes-in-parallel
     0
     #'fast-multiply-in
     (nat-tuple-complement (wavefunction-qubits wavefunction) qubits)))

  ;; Return the wavefunction.
  wavefunction)

(defun-inlinable apply-matrix-operator (matrix wavefunction qubits)
  "Apply the matrix operator OPERATOR to the amplitudes of WAVEFUNCTION specified by the qubits QUBITS."
  (declare (type quantum-state wavefunction)
           (type quantum-operator matrix)
           (inline apply-operator))
  (flet ((multiply-by-operator (column)
           (matrix-multiply matrix column)))
    (declare (dynamic-extent #'multiply-by-operator)
             (inline multiply-by-operator))
    (apply-operator #'multiply-by-operator wavefunction qubits)))

(defun normalize-wavefunction (wavefunction)
  "Normalize the wavefunction WAVEFUNCTION, making it a unit vector in the constituent Hilbert space."
  (declare (type quantum-state wavefunction)
           (inline probability psum))
  ;; Mutate the wavefunction.
  (let ((num-qubits (wavefunction-qubits wavefunction))
        ;; Square norm.
        (norm (psum #'probability wavefunction)))
    (declare (type flonum norm))

    ;; Compute the RECIPROCAL norm.
    (setf norm (/ (sqrt (the (flonum 0) norm)) norm))

    ;; Normalize the wavefunction
    (if (<= *qubits-required-for-parallelization* num-qubits)
        (lparallel:pdotimes (i (length wavefunction))
          (setf (aref wavefunction i) (* norm (aref wavefunction i))))
        (dotimes (i (length wavefunction))
          (setf (aref wavefunction i) (* norm (aref wavefunction i)))))

    ;; Return the wavefunction.
    wavefunction))
