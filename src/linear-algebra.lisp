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
    "Generate code to compute the product of the complex matrix (represented as a square array of CFLONUMs) and a complex column vector (represented as a CFLONUM vector)."
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

(defun matrix-multiply (matrix column)
  "Compute the product of the complex matrix (represented as a square array of CFLONUMs) and a complex vector (represented as a CFLONUM vector) in-place."
  (declare (type quantum-operator matrix)
           (type quantum-state column)
           #.*optimize-dangerously-fast*
           (inline matmul2 matmul4))
  (assert (= (array-dimension matrix 0)
             (array-dimension matrix 1))
          (matrix)
          "The given matrix isn't square.")
  (assert (= (array-dimension matrix 0)
             (length column))
          (matrix column)
          "The given matrix and column vector don't have compatible dimensions.")
  (let* ((matrix-size (array-dimension matrix 0))
         (result (make-vector matrix-size)))
    (declare (type quantum-state result)
             (dynamic-extent result))
    ;; Perform the multiplication.
    (case matrix-size
      ((2) (matmul2 matrix column result))
      ((4) (matmul4 matrix column result))
      (otherwise
       (dotimes (i matrix-size)
         (let ((element (cflonum 0)))
           (declare (type cflonum element))
           (dotimes (j matrix-size)
             (incf element (* (aref matrix i j) (aref column j))))
           (setf (aref result i) element)))))

    ;; Put RESULT back into the column vector.
    #+ccl                    ; CCL bug.
    (loop :for i :below (length column)
          :do (setf (aref column i) (aref result i)))

    #-ccl
    (replace column result)

    ;; Return the modified column.
    column))

(defmacro psum-dotimes ((i range) &body body)
  "Compute the sum of BODY for I in ranging over 0 <= I < RANGE."
  (alexandria:with-gensyms (sum partial-sum start end ch num-tasks worker-function ranges)
    (alexandria:once-only (range)
      `(if (< ,range (expt 2 *qubits-required-for-parallelization*))
           (locally (declare #.*optimize-dangerously-fast*)
             (loop :with ,sum :of-type flonum := (flonum 0)
                   :for ,i :below ,range
                   :do (incf ,sum (the flonum (progn ,@body)))
                   :finally (return ,sum)))
           (flet ((,worker-function (,start ,end)
                    (declare (type non-negative-fixnum ,start)
                             (type non-negative-fixnum ,end))
                    (locally (declare #.*optimize-dangerously-fast*)
                      (loop :with ,partial-sum :of-type flonum := (flonum 0)
                            :for ,i :of-type non-negative-fixnum :from ,start :below ,end
                            :do (incf ,partial-sum (the flonum (progn ,@body)))
                            :finally (return ,partial-sum)))))
             (declare (dynamic-extent #',worker-function))
             (let* ((,ch (lparallel:make-channel))
                    (,num-tasks (lparallel:kernel-worker-count))
                    (,ranges (subdivide ,range ,num-tasks)))
               (loop :for (,start . ,end) :in ,ranges
                     :do (lparallel:submit-task ,ch #',worker-function ,start ,end))
               (loop :repeat (length ,ranges)
                     :sum (lparallel:receive-result ,ch))))))))

(defun-inlinable psum (f state)
  "Compute the sum of F(X) for X in STATE, in parallel. F should be a unary function mapping CFLONUMs to FLONUMs."
  (declare (type (function (cflonum) flonum) f)
           (type quantum-state state))
  (psum-dotimes (i (length state))
    (funcall f (aref state i))))
