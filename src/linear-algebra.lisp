;;;; linear-algebra.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Quantum State & Operator Representation

(deftype quantum-state (&optional (n '*))
  "A representation of a quantum state. This will have a power-of-2 length."
  `(simple-array cflonum (,n)))

(defun octets-required-for-qubits (n)
  "The number of octets required to represent a state of N qubits."
  (* (expt 2 n) +octets-per-cflonum+))

(defun octets-required-for-quantum-state (state)
  "The number of octets reqquired to represent a given quantum state."
  (check-type state quantum-state)
  (* +octets-per-cflonum+ (length state)))

(deftype quantum-operator (&optional (n '*))
  "A representation of an operator on a quantum state. This will be a unitary square matrix where each dimension is a power-of-two."
  `(simple-array cflonum (,n ,n)))

(declaim (ftype (function (fixnum &rest number) quantum-operator) make-matrix))
(defun make-matrix (size &rest elements)
  "Make a SIZE x SIZE complex matrix whose elements are ELEMENTS. Each of ELEMENTS must be able to be coerced into a CFLONUM."
  (declare (dynamic-extent elements))
  (loop :with matrix := (make-array (list size size)
                                    :element-type 'cflonum
                                    :initial-element (cflonum 0))
        :for i :from 0
        :for raw-element :in elements
        :for element :of-type cflonum := (cflonum raw-element)
        :do (setf (row-major-aref matrix i) element)
        :finally (return matrix)))

(defun magicl-matrix-to-quantum-operator (m)
  "Convert a MAGICL matrix M to a QUANTUM-OPERATOR."
  (check-type m magicl:matrix)
  (let* ((rows (magicl:matrix-rows m))
         (cols (magicl:matrix-cols m))
         (op   (make-matrix rows cols)))
    (dotimes (r rows op)
      (dotimes (c cols)
        (setf (aref op r c) (cflonum (magicl:ref m r c)))))))


;;; Quantum Operator Operations/Manipulations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun matrix-multiply-code (n matrix column result)
    "Generate code to compute the product of the N x N complex matrix (represented as a square array of CFLONUMs) and a length-N complex column vector (represented as a CFLONUM vector)."
    (check-type n unsigned-byte)
    (check-type matrix symbol)
    (check-type column symbol)
    (check-type result symbol)
    `(let ((element (cflonum 0)))
       (declare (type cflonum element))
       ,@(loop :for i :below n
               :append `((setf element (cflonum 0))
                         ,@(loop :for j :below n
                                 :collect `(incf element (* (aref ,matrix ,i ,j)
                                                            (aref ,column ,j))))
                         (setf (aref ,result ,i) element)))
       nil)))

(defmacro define-matmul (name size)
  "Define a matrix multiplier function named NAME for square matrices operating on vectors of length SIZE. The defined function will take three arguments:

    1. MATRIX: An object of type QUANTUM-OPERATOR, the multiplier.
    2. COLUMN: An object of type QUANTUM-STATE, the vector being multipled.
    3. RESULT: An object of type QUANTUM-STATE, where the result is deposited.

The function will just return NIL, and modify the contents of RESULT."
  (check-type name symbol)
  (check-type size unsigned-byte)
  (alexandria:with-gensyms (matrix column result)
    `(progn
       (declaim (ftype (function ((quantum-operator ,size)
                                  (quantum-state ,size)
                                  (quantum-state ,size))
                                 null)
                       ,name))
       (defun-inlinable ,name (,matrix ,column ,result)
         (declare (type (quantum-operator ,size) ,matrix)
                  (type (quantum-state ,size) ,column)
                  (type (quantum-state ,size) ,result)
                  ,*optimize-dangerously-fast*)
         ,(matrix-multiply-code size matrix column result)))))

(define-matmul matmul2 2)
(define-matmul matmul4 4)
(define-matmul matmul8 8)

(defun matrix-multiply (matrix column)
  "Compute the product of the complex matrix (represented as a square array of CFLONUMs) and a complex vector (represented as a CFLONUM vector) in-place."
  (declare (type quantum-operator matrix)
           (type quantum-state column)
           #.*optimize-dangerously-fast*)
  (assert (= (array-dimension matrix 0)
             (array-dimension matrix 1))
          (matrix)
          "The given matrix isn't square.")
  (assert (= (array-dimension matrix 0)
             (length column))
          (matrix column)
          "The given matrix and column vector don't have compatible dimensions.")
  (let* ((matrix-size (array-dimension matrix 0))
         (result (make-lisp-cflonum-vector matrix-size)))
    (declare (type quantum-state result)
             (dynamic-extent result))
    ;; Perform the multiplication.
    (case matrix-size
      ((2) (matmul2 matrix column result))
      ((4) (matmul4 matrix column result))
      ((8) (matmul8 matrix column result))
      (otherwise
       (dotimes (i matrix-size)
         (let ((element (cflonum 0)))
           (declare (type cflonum element))
           (dotimes (j matrix-size)
             (incf element (* (aref matrix i j) (aref column j))))
           (setf (aref result i) element)))))

    ;; Put RESULT back into the column vector.
    (replace column result)

    ;; Return the modified column.
    column))

(defun tr (M)
  "Compute the trace of M."
  (declare (type quantum-operator M))
  (loop :for i :below (array-dimension M 0) :sum (aref M i i)))

(defun dagger (M)
  "Compute the Hermitian transpose of M in-place."
  (declare (type quantum-operator M))
  (loop :for i :below (array-dimension M 0) :do
    (loop :for j :to i :do
      (psetf (aref M i j) (conjugate (aref M j i))
             (aref M j i) (conjugate (aref M i j)))))
  M)

(defun compose-operators (A B)
  "Compute the product of the matrices A and B."
  (declare (type (simple-array cflonum (* *)) A B))
  (destructuring-bind (m n) (array-dimensions A)
    (let* ((l (array-dimension B 1))
           (result (make-array (list m l) :element-type 'cflonum)))
      (loop :for i :below m :do
        (loop :for k :below l :do
          (loop :for j :below n :do
            (incf (aref result i k)
                  (* (aref A i j)
                     (aref B j k))))))
      result)))

(defun outer-multiply (u v)
  "Compute the outer product of two equally-sized column vectors U and V (specifically UV^dagger)."
  (declare (type quantum-state u v))
  (assert (= (length u) (length v)))
  (let* ((len (length u))
         (result (make-matrix len)))
    (dotimes (r len result)
      (dotimes (c len)
        (setf (aref result r c)
              (* (aref u r)
                 (conjugate (aref v c))))))))

(defun kronecker-multiply (A B)
  "Compute the Kronecker product of matrices A and B."
  (declare (type (simple-array cflonum (* *)) A B))
  (destructuring-bind (m n) (array-dimensions A)
    (destructuring-bind (p q) (array-dimensions B)
      (let ((result (make-array (list (* m p) (* n q))
                                :element-type 'cflonum)))
        (dotimes (i m result)
          (dotimes (j n)
            (let ((Aij (aref A i j))
                  (y (* i p))
                  (x (* j q)))
              (loop :for u :below p :do
                (loop :for v :below q :do
                  (setf (aref result (+ y u) (+ x v))
                        (* Aij (aref B u v))))))))))))

(declaim (ftype (function ( (function (cflonum) flonum)
                            quantum-state
                          )
                          flonum)
                psum))
(defun-inlinable psum (f state)
  "Compute the sum of F(X) for X in STATE, in parallel. F should be a unary function mapping CFLONUMs to FLONUMs."
  (declare (type (function (cflonum) flonum) f)
           (type quantum-state state))
  (psum-dotimes (i (length state))
    (funcall f (aref state i))))

;;; Superoperators 

;;; Ordinary gates, as well as user-specified "Kraus operators" in
;;; SUPEROPERATOR-DEFINITIONS, can represented by a SUPEROPERATOR
;;; type. The quil syntax for specifying superoperators is done
;;; through pragmas where a user may specify a
;;; "SUPEROPERATOR-DEFINTION" on a gate and a specific set of
;;; qubits. During the QVM evaluation, such a user defined
;;; SUPEROPERATOR definition will replace the usual gate.
;;;
;;; The primary difference between the DENSITY-MATRIX-STATE's
;;; superoperator application and the PURE-STATE's is that application
;;; of a superoperator to a DENSITY-MATRIX-STATE is completely
;;; deterministic and "folds all of the noisy" into the density
;;; matrix, whereas the application to a PURE-STATE is
;;; nondeterministic and tracks only a specific realization of the
;;; gate noise in a stochastic process.

(adt:defdata superoperator
  "Representation of a linear operator on density operators."
  ;; Let ' mean † aka conjugate transpose.
  ;;
  ;; ρ ↦ U ρ U'
  (single-kraus quil:gate)
  ;; ρ ↦ ∑ᵢ Aᵢ ρ Aᵢ'
  (kraus-list list))

(defun ensure-superoperator (mat)
  "Converts a magicl matrix MAT into a SINGLE-KRAUS SUPEROPERATOR."
  (etypecase mat
    (superoperator mat)
    (quil:gate (single-kraus mat))
    (magicl:matrix (single-kraus 
                    (make-instance 'quil:simple-gate
                                   :name (string (gensym "KRAUS-TEMP"))
                                   :matrix mat)))))

(defun superoperator-to-component-matrices (sop)
  "Convert SOP into a MAGICL:MATRIX or a LIST of them."
  (adt:match superoperator sop
    ((single-kraus U)
     (quil::gate-matrix U))
    ((kraus-list list)
     (mapcar #'superoperator-to-component-matrices list))))

(defgeneric conjugate-entrywise (gate)
  (:documentation "Construct a new gate from GATE with corresponding matrix entries conjugated.")
  (:method ((gate quil:simple-gate))
    (make-instance 'quil:simple-gate
                   :name (concatenate 'string (quil:gate-name gate) "*")
                   :matrix (magicl:conjugate-entrywise (quil:gate-matrix gate))))
  (:method ((gate quil:permutation-gate))
    (make-instance 'quil:permutation-gate
                   :name (concatenate 'string (quil:gate-name gate) "*")
                   :permutation (quil:permutation-gate-permutation gate)))
  (:method ((gate quil:parameterized-gate))
    (make-instance 'quil:parameterized-gate
                   :name (concatenate 'string (quil:gate-name gate) "*")
                   :dimension (quil:gate-dimension gate)
                   :matrix-function #'(lambda (&rest parameters)
                                        (magicl:conjugate-entrywise
                                         (apply #'quil:gate-matrix gate parameters))))))
