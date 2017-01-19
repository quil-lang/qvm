;;;; linear-algebra.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defconstant +octets-per-flonum+ 8)

(deftype flonum (&optional min)
  "The float type used in computations."
  (if (numberp min)
      `(double-float ,(coerce min 'double-float))
      `double-float))

(defconstant +octets-per-cflonum+ (* 2 +octets-per-flonum+))

(deftype cflonum ()
  "The complex float type used in computations. Typically these will represent wavefunction amplitudes."
  `(complex flonum))

(defun flonum (x)
  "Coerce X into a FLONUM."
  (coerce x 'flonum))

(define-compiler-macro flonum (&whole whole &environment env x)
  (if (and (constantp x env)
           (numberp x))
      (coerce x 'flonum)
      whole))

(defun cflonum (x)
  "Coerce X into a CFLONUM."
  (coerce x 'cflonum))

(define-compiler-macro cflonum (&whole whole &environment env x)
  (if (and (constantp x env)
           (numberp x))
      (coerce x 'cflonum)
      whole))

(deftype quantum-state (&optional (n '*))
  "A representation of a quantum state. This will have a power-of-2 length."
  `(simple-array cflonum (,n)))

(defun octets-required-for-qubits (n)
  "The number of octets required to represent a state of N qubits."
  (* (expt 2 n) +octets-per-cflonum+))

(declaim (ftype (function (non-negative-fixnum &rest number) quantum-state) make-vector))
(defun make-vector (size &rest elements)
  "Make a SIZE-length complex vector whose elements are ELEMENTS."
  (let ((vec (make-array size :element-type 'cflonum
                              :initial-element (cflonum 0))))
    (loop :for i :from 0
          :for raw-element :in elements
          :for element :of-type cflonum :=  (cflonum raw-element)
          :do (setf (aref vec i) element)
          :finally (return vec))))

(define-compiler-macro make-vector (&whole form size &rest elements)
  (if (null elements)
      `(make-array ,size :element-type 'cflonum
                         :initial-element (cflonum 0))
      form))

(deftype quantum-operator (&optional (n '*))
  "A representation of an operator on a quantum state. This will be a unitary square matrix where each dimension is a power-of-two."
  `(simple-array cflonum (,n ,n)))

(declaim (ftype (function (fixnum &rest number) quantum-operator) make-matrix))
(defun make-matrix (size &rest elements)
  "Make a SIZE x SIZE complex matrix whose elements are ELEMENTS. Each of ELEMENTS must be able to be coerced into a CFLONUM."
  (declare (dynamic-extent elements))
  (let ((matrix (make-array (list size size)
                            :element-type 'cflonum
                            :initial-element (cflonum 0))))
    (loop :for i :from 0
          :for raw-element :in elements
          :for element :of-type cflonum := (cflonum raw-element)
          :do (setf (row-major-aref matrix i) element)
          :finally (return matrix))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun matrix-multiply-code (n matrix column result)
    "Compute the product of the complex matrix (represented as a square array of CFLONUMs) and a complex column vector (represented as a CFLONUM vector)."
    (check-type n unsigned-byte)
    `(locally
         (declare (type (quantum-operator ,n) ,matrix)
                  (type (quantum-state ,n) ,column)
                  (type (or null (quantum-state ,n)) result)
                  (optimize speed (safety 0) (debug 0) (space 0)))
       (let ((result (or ,result (make-vector ,n)))
             (element (cflonum 0)))
         (declare (type (quantum-state ,n) result)
                  (type cflonum element))
         ,@(loop :for i :below n
                 :append `((setf element (cflonum 0))
                           ,@(loop :for j :below n
                                   :collect `(incf element (* (aref ,matrix ,i ,j)
                                                              (aref ,column ,j))))
                           (setf (aref result ,i) element)))
         result))))

(defmacro define-matmul (name size)
  `(progn
     (declaim (ftype (function ((quantum-operator ,size)
                                (quantum-state ,size)
                                (or null (quantum-state ,size)))
                               (quantum-state ,size))
                     ,name))
     (defun-inlinable ,name (m c result)
       ,(matrix-multiply-code size 'm 'c 'result))))

(define-matmul matmul2 2)
(define-matmul matmul4 4)

(defun matrix-multiply (matrix column &optional result)
  "Compute the product of the complex matrix (represented as a square array of CFLONUMs) and a complex vector (represented as a CFLONUM vector)."
  (declare (type quantum-operator matrix)
           (type quantum-state column)
           (type (or null quantum-state) result)
           (optimize speed (safety 1))
           (inline matmul2 matmul4))
  (assert (= (array-dimension matrix 0)
             (array-dimension matrix 1))
          (matrix)
          "The given matrix isn't square.")
  (assert (= (array-dimension matrix 0)
             (length column))
          (matrix column)
          "The given matrix and column vector don't have compatible dimensions.")
  (let ((matrix-size (array-dimension matrix 0)))
    (case matrix-size
      ((2) (matmul2 matrix column result))
      ((4) (matmul4 matrix column result))
      (otherwise
       (let ((result (or result (make-vector matrix-size))))
         (dotimes (i matrix-size result)
           (let ((element (cflonum 0)))
             (declare (type cflonum element))
             (dotimes (j matrix-size)
               (incf element (* (aref matrix i j) (aref column j))))
             (setf (aref result i) element))))))))
