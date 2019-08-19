;;;; src/wavefunction.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; This file mostly deals with the manipulation of a
;;;; wavefunction. This is a relatively barebones interface; the
;;;; intention is that one uses QVM-like objects to do calculations.

;;; This is very dangerous, unfortunately. With safety > 0, we slow
;;; down 33%. With none of the optimizations in this file, we slow
;;; down >100%. It might be worth doing a finer-grained study of the
;;; functions to optimize.
(declaim #.*optimize-dangerously-fast*)

;;;; Wavefunction manipulation

(deftype amplitude-address ()
  "An address into an array of amplitudes."
  `(integer 0 (,(expt 2 +max-nat-tuple-cardinality+))))

(declaim (inline index-to-address))
(defun index-to-address (index qubit state)
  "Given an amplitude index INDEX, find the amplitude address for the INDEX'th basis state with QUBIT in the STATE state.

Specifically, given an integer whose bit string is

    INDEX = LLLLRRRR,

compute the address

    Result = LLLL{0,1}RRRR

which is the index with a {1, 0} injected at the QUBIT'th position."
  (declare (type nat-tuple-element qubit)
           (type amplitude-address index)
           (type bit state)
           #.*optimize-dangerously-fast*)
  (dpb state (byte 1 qubit) (inject-bit index qubit)))

(declaim (ftype (function (cflonum) flonum) probability)
         (inline probability))
(defun probability (amplitude)
  "Convert an amplitude into a probability."
  (declare (type cflonum amplitude))
  (let ((re (realpart amplitude))
        (im (imagpart amplitude)))
    (declare (type flonum re im))
    (+ (* re re) (* im im))))

(declaim (ftype (function (quantum-state nat-tuple-element) flonum)
                wavefunction-ground-state-probability))
(defun-inlinable wavefunction-ground-state-probability (wavefunction qubit)
  "Compute the probability that qubit QUBIT is in the ground state."
  (declare #.*optimize-dangerously-fast*)
  (psum-dotimes (i (half (length wavefunction)))
    (let ((address (inject-bit i qubit)))
      (declare (type amplitude-address address))
      (probability (aref wavefunction address)))))

(declaim (ftype (function (quantum-state nat-tuple-element) flonum)
                wavefunction-excited-state-probability))
(defun-inlinable wavefunction-excited-state-probability (wavefunction qubit)
  "Compute the probability that qubit QUBIT is in the excited state."
  (declare #.*optimize-dangerously-fast*)
  (psum-dotimes (i (half (length wavefunction)))
    (let ((address (index-to-address i qubit 1)))
      (declare (type amplitude-address address))
      (probability (aref wavefunction address)))))

(declaim (ftype (function (quantum-state) nat-tuple-cardinality) wavefunction-qubits)
         (inline wavefunction-qubits))
(defun wavefunction-qubits (wavefunction)
  "The number of qubits represented by the wavefunction WAVEFUNCTION."
  (declare (type quantum-state wavefunction))
  (max 1 (1- (integer-length (length wavefunction)))))

(declaim (ftype (function (quantum-state) quantum-state) bring-to-zero-state))
(defun-inlinable bring-to-zero-state (v)
  "Modify the quantum state V to be |...000>."
  (declare (type quantum-state v)
           #.*optimize-dangerously-fast*)
  (if (< (wavefunction-qubits v) *qubits-required-for-parallelization*)
      (dotimes (i (length v))
        (setf (aref v i) (cflonum 0)))
      (lparallel:pdotimes (i (length v))
        (setf (aref v i) (cflonum 0))))

  (setf (aref v 0) (cflonum 1))
  v)

(defun copy-wavefunction (wf &optional destination)
  "Create a copy of the wavefunction WF. If DESTINATION is NIL, allocate a new vector on the Lisp heap. If DESTINATION is provided, copy the wavefunction WF into the DESTINATION vector. Only copy as many elements as can be copied, namely min(|wf|, |destination|)."
  (declare (type quantum-state wf)
           (type (or null quantum-state) destination))
  (let ((length (if (null destination)
                    (length wf)
                    (min (length wf) (length destination)))))
    (if (<= *qubits-required-for-parallelization* (1- (integer-length length)))
        (let ((copy (if (null destination)
                        (make-lisp-cflonum-vector length) ; Allocate fresh vector.
                        destination)))
          (declare (type quantum-state copy))
          (lparallel:pdotimes (i length copy)
            (setf (aref copy i) (aref wf i))))
        (if (null destination)
            (copy-seq wf)
            (replace destination wf)))))


(defun-inlinable set-qubit-components-of-amplitude-address (address flags qubits)
  "Set the amplitude address to the bits within the non-negative integer FLAGS for the corresponding tuple of qubit indices QUBITS. (Ordered LSB to MSB.)"
  (declare (type nat-tuple qubits)
           (type alexandria:non-negative-fixnum flags)
           (type amplitude-address address))
  (do-nat-tuple (qubit qubits)
    (if (logbitp 0 flags)
        (setf address (dpb 1 (byte 1 qubit) address))
        (setf address (dpb 0 (byte 1 qubit) address)))
    (setf flags (ash flags -1)))
  address)


(defun-inlinable map-reordered-amplitudes (starting-address function qubits)
  "Iterate through all variations of the amplitude address STARTING-ADDRESS, varying the qubits specified by the tuple QUBITS. For each newly loaded address, call the function FUNCTION.

FUNCTION should be a binary function, and will receive (1) an index running from 0 below 2^|qubits|, and (2) the varied amplitude address."
  (declare (type nat-tuple qubits)
           (type (function (alexandria:non-negative-fixnum alexandria:non-negative-fixnum) *) function)
           (inline set-qubit-components-of-amplitude-address))
  (let ((number-of-iterations (expt 2 (nat-tuple-cardinality qubits))))
    (dotimes (combo number-of-iterations)
      (let ((address (set-qubit-components-of-amplitude-address starting-address combo qubits)))
        (funcall function combo address)))))


(defun-inlinable map-reordered-amplitudes-in-parallel-truly (starting-address function qubits)
  "Parallel version of #'MAP-REORDERED-AMPLITUDES."
  (declare (type nat-tuple qubits)
           (type (function (alexandria:non-negative-fixnum alexandria:non-negative-fixnum) *) function)
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


(defun-inlinable map-complement (function n qubits)
  "An optimized call to

    (MAP-REORDERED-AMPLITUDES 0 FUNCTION (NAT-TUPLE-COMPLEMENT QUBITS))

up to amplitude ordering."
  (declare (type nat-tuple qubits)
           (type (function (alexandria:non-negative-fixnum amplitude-address) *) function)
           (type (and (integer 1) nat-tuple-cardinality) n))
  (let ((complement-size (- n (nat-tuple-cardinality qubits))))
    (declare (type nat-tuple-cardinality complement-size))
    (let ((qubits-list (sort (copy-seq qubits) #'<)))
      (declare (type nat-tuple qubits-list))
      (dotimes (i (expt 2 complement-size))
        (declare (type alexandria:non-negative-fixnum i))
        (let ((j i))
          (declare (type alexandria:non-negative-fixnum j))
          (do-nat-tuple (q qubits-list)
            ;; (declare (type nat-tuple-element q))
            (setf j (inject-bit j q)))
          (funcall function i j))))))

(defun-inlinable map-complement-in-parallel-truly (function n qubits)
  "A parallel version of #'MAP-COMPLEMENT."
  (declare (type nat-tuple qubits)
           (type (function (alexandria:non-negative-fixnum amplitude-address) *) function)
           (type (and (integer 1) nat-tuple-cardinality) n))
  (let ((complement-size (- n (nat-tuple-cardinality qubits))))
    (declare (type nat-tuple-cardinality complement-size))
    (let ((qubits-list (sort (copy-seq qubits) #'<)))
      (declare (type nat-tuple qubits-list))
      (lparallel:pdotimes (i (expt 2 complement-size))
        (declare (type alexandria:non-negative-fixnum i))
        (let ((j i))
          (declare (type alexandria:non-negative-fixnum j))
          (do-nat-tuple (q qubits-list)
            ;; (declare (type nat-tuple-element q))
            (setf j (inject-bit j q)))
          (funcall function i j))))))

(defun-inlinable map-complement-in-parallel (function n qubits)
  "A parallel version of #'MAP-COMPLEMENT, when the number of qubits is large enough."
  (declare (type nat-tuple qubits)
           (type (function (alexandria:non-negative-fixnum amplitude-address) *) function)
           (type (and (integer 1) nat-tuple-cardinality) n))
  (if (<= *qubits-required-for-parallelization* n)
      (map-complement-in-parallel-truly function n qubits)
      (map-complement function n qubits)))

(defun extract-amplitudes (wavefunction qubits starting-address)
  "Returns a column vector of amplitudes represented by the tuple of qubits QUBITS."
  (declare (type nat-tuple qubits)
           (type quantum-state wavefunction)
           (inline map-reordered-amplitudes))
  (let ((col (make-lisp-cflonum-vector (expt 2 (nat-tuple-cardinality qubits)))))
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
         (let ((,col (make-lisp-cflonum-vector (expt 2 (nat-tuple-cardinality ,qubits)))))
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

#+(and sbcl avx2)
(defun apply-1q-operator (matrix wavefunction qubits)
  (declare (inline qvm-intrinsics::2x2matrix-to-simd
                   qvm-intrinsics::matmul2-simd)
           (type quantum-state wavefunction)
           (type quantum-operator matrix)
           (type nat-tuple qubits)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((n/2 (half (length wavefunction)))
        (q   (aref qubits 0)))
    ;; Load the matrix into registers.
    (with-parallel-subdivisions (start end n/2)
      (multiple-value-bind (vyr vyi xzr xzi)
          (qvm-intrinsics::2x2matrix-to-simd (aref matrix 0 0)
                                             (aref matrix 0 1)
                                             (aref matrix 1 0)
                                             (aref matrix 1 1))
        (loop :for i :from start :below end :do
          (let* ((ai (inject-bit i q))
                 (bi (dpb 1 (byte 1 q) ai)))
            (multiple-value-bind (p q)
                (qvm-intrinsics::matmul2-simd vyr vyi xzr xzi
                                              (aref wavefunction ai)
                                              (aref wavefunction bi))
              (setf (aref wavefunction ai) p
                    (aref wavefunction bi) q))))
        wavefunction))))

(defun apply-2q-operator (matrix wavefunction qubits)
  (declare (inline qvm-intrinsics::2x4matrix-to-simd
                   qvm-intrinsics::matmul4-simd-half)
           (type quantum-state wavefunction)
           (type quantum-operator matrix)
           (type nat-tuple qubits)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((n/4 (floor (/ (length wavefunction) 4)))
        (q0  (aref qubits 0))
        (q1  (aref qubits 1)))
    (with-parallel-subdivisions (start end n/4)
      (loop :for i :from start :below end :do
           (let* ((ai (inject-bit (inject-bit i q1) q0))
                  (bi (dpb 1 (byte 1 q1) ai))
                  (ci (dpb 1 (byte 1 q0) ai))
                  (di (dpb 1 (byte 1 q0) (dpb 1 (byte 1 q1) ai))))
             (let ((a0 (aref wavefunction ai))
                   (a1 (aref wavefunction bi))
                   (a2 (aref wavefunction ci))
                   (a3 (aref wavefunction di)))
               (let ((m00 (aref matrix 0 0))
                     (m01 (aref matrix 0 1))
                     (m02 (aref matrix 0 2))
                     (m03 (aref matrix 0 3))
                     (m10 (aref matrix 1 0))
                     (m11 (aref matrix 1 1))
                     (m12 (aref matrix 1 2))
                     (m13 (aref matrix 1 3)))
                 (multiple-value-bind (m0r m0i m1r m1i m2r m2i m3r m3i)
                     (qvm-intrinsics::2x4matrix-to-simd m00 m01 m02 m03 m10 m11 m12 m13)
                   (multiple-value-bind (r0 r1)
                       (qvm-intrinsics::matmul4-simd-half m0r m0i m1r m1i m2r m2i m3r m3i a0 a1 a2 a3)
                     (setf (aref wavefunction ai) r0
                           (aref wavefunction bi) r1))))
               (let ((m20 (aref matrix 2 0))
                     (m21 (aref matrix 2 1))
                     (m22 (aref matrix 2 2))
                     (m23 (aref matrix 2 3))
                     (m30 (aref matrix 3 0))
                     (m31 (aref matrix 3 1))
                     (m32 (aref matrix 3 2))
                     (m33 (aref matrix 3 3)))
                 (multiple-value-bind (m4r m4i m5r m5i m6r m6i m7r m7i)
                     (qvm-intrinsics::2x4matrix-to-simd m20 m21 m22 m23 m30 m31 m32 m33)
                   (multiple-value-bind (r2 r3)
                       (qvm-intrinsics::matmul4-simd-half m4r m4i m5r m5i m6r m6i m7r m7i a0 a1 a2 a3)
                     (setf (aref wavefunction ci) r2
                           (aref wavefunction di) r3)))))))
      wavefunction)))

(defun-inlinable apply-operator (operator wavefunction qubits)
  "Apply an operator OPERATOR to the amplitudes of WAVEFUNCTION specified by the qubits QUBITS. OPERATOR shall be a unary function taking a QUANTUM-STATE as an argument and modifying it."
  (declare (type quantum-state wavefunction)
           (type (function (quantum-state) *) operator)
           (inline map-reordered-amplitudes-in-parallel map-complement-in-parallel))
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

    (map-complement-in-parallel
     #'fast-multiply-in
     (wavefunction-qubits wavefunction)
     qubits)

    ;; Again, we show this variation for pedagogical reasons.
    #+unused
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
  (case (nat-tuple-cardinality qubits)
    #+(and sbcl avx2)
    ((1)
     (apply-1q-operator matrix wavefunction qubits))
    #+(and sbcl avx2)
    ((2)
     (apply-2q-operator matrix wavefunction qubits))
    (otherwise
     (flet ((multiply-by-operator (column)
              (matrix-multiply matrix column)))
       (declare (dynamic-extent #'multiply-by-operator)
                (inline multiply-by-operator))
       (apply-operator #'multiply-by-operator wavefunction qubits)))))

(declaim (ftype (function (quantum-state) (flonum 0)) %serial-norm))
(defun-inlinable %serial-norm (wavefunction)
  (loop :with s :of-type flonum := (flonum 0)
        :for i :below (length wavefunction)
        :do (incf s (probability (aref wavefunction i)))
        :finally (return (sqrt (the (flonum 0) s)))))

(declaim (ftype (function (quantum-state) (flonum 0)) norm))
(defun-inlinable norm (wavefunction)
  "Compute the length/L2 norm of the wavefunction WAVEFUNCTION."
  (declare (inline probability psum))
  (if (<= *qubits-required-for-parallelization*
          (wavefunction-qubits wavefunction))
      (sqrt (the (flonum 0) (psum #'probability wavefunction)))
      (%serial-norm wavefunction)))


(defun normalize-wavefunction (wavefunction &key length)
  "Normalize the wavefunction WAVEFUNCTION, making it a unit vector in the constituent Hilbert space.

If the length/norm of WAVEFUNCTION is known, it can be passed as the LENGTH parameter."
  (declare (type quantum-state wavefunction)
           (type (or null real) length)
           (inline norm))
  ;; Mutate the wavefunction.
  (let ((num-qubits (wavefunction-qubits wavefunction))
        (inv-norm (if (null length)
                      (/ (norm wavefunction))
                      (/ (flonum length)))))
    (declare (type (flonum 0) inv-norm))

    ;; Normalize the wavefunction
    (pdotimes (i (length wavefunction) wavefunction)
      (setf (aref wavefunction i) (* inv-norm (aref wavefunction i))))))

;;; This is intended to be a shorthand for testing.
(declaim (ftype (function (number &rest number) quantum-state) wf))
(defun wf (elt &rest elts)
  "Construct a wavefunction from the elements (ELT . ELTS)."
  (declare (dynamic-extent elts))
  (let ((n (1+ (length elts))))
    (assert (power-of-two-p n) () "The number of elements supplied to WF must be a power of two.")
    (let ((psi (make-lisp-cflonum-vector n)))
      (setf (aref psi 0) (cflonum elt))
      (loop :for i :from 1
            :for elt :in elts
            :do (setf (aref psi i) (cflonum elt))
            :finally (return (normalize-wavefunction psi))))))

(declaim (ftype (function (t) (simple-array flonum (*)))
                cumulative-distribution-function))
(defun cumulative-distribution-function (state)
  "Compute the CDF of a quantum state STATE. This is a vector C such that C_i = sum_{0 <= j <= i} Pr(j)."
  (check-type state quantum-state)
  (let* ((n (length state))
         (cdf (make-array (length state) :element-type 'flonum
                                         :initial-element (flonum 0))))
    (declare #.*optimize-dangerously-fast*
             (type alexandria:non-negative-fixnum n)
             (type (simple-array flonum (*)) cdf))
    (loop :with s :of-type flonum := (flonum 0)
          :for i :of-type amplitude-address :below n
          :do (incf s (probability (aref state i)))
              (setf (aref cdf i) s)
          :finally (return cdf))))

(declaim (inline gaussian-random-complex))
(defun gaussian-random-complex ()
  "Return a complex number with Gaussian-random real and imaginary parts."
  (multiple-value-bind (re im)
      (alexandria:gaussian-random)
    (complex re im)))

(defun randomize-wavefunction (wavefunction)
  "Randomize the elements of WAVEFUNCTION resulting in a valid complex unit vector."
  (declare (type quantum-state wavefunction))
  ;; Fill the vector with random Gaussian variates.
  (map-into wavefunction #'gaussian-random-complex)
  ;; Normalize.
  (normalize-wavefunction wavefunction))
