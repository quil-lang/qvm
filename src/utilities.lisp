;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Miscellaneous utilities.

;;; Tuples of Natural Numbers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-nat-tuple-cardinality+ (integer-length most-positive-fixnum)))

(deftype non-negative-fixnum ()
  "A non-negative fixnum."
  `(and fixnum unsigned-byte))

(deftype nat-tuple-element ()
  "The set of valid elements in a nat tuple."
  `(integer 0 (#.+max-nat-tuple-cardinality+)))

(deftype nat-tuple-cardinality ()
  "A representation of the set of cardinalities of a nat tuple."
  `(integer 0 #.+max-nat-tuple-cardinality+))

(declaim (inline make-nat-tuple
                 nat-tuple-add
                 nat-tuple-cardinality))

(defstruct (nat-tuple (:constructor %make-nat-tuple))
  "The NAT-TUPLE type. A \"nat tuple\" represents an ordered list of non-negative integer indexes."
  (list nil :read-only t)
  (membership 0 :read-only t :type non-negative-fixnum))

(defun make-nat-tuple ()
  "Make a new, empty nat tuple."
  (load-time-value
   (%make-nat-tuple :list nil
                    :membership 0)
   t))

(defun nat-tuple-add (nt elt)
  "Add the element ELT (of type NAT-TUPLE-ELEMENT) to the nat tuple NT."
  (declare (type nat-tuple nt)
           (type nat-tuple-element elt))
  (if (logbitp elt (nat-tuple-membership nt))
      nt
      (%make-nat-tuple :list (cons elt (nat-tuple-list nt))
                       :membership (dpb 1 (byte 1 elt) (nat-tuple-membership nt)))))

(defun nat-tuple-cardinality (nt)
  "Compute the number of elements in the nat tuple NT."
  (declare (type nat-tuple nt))
  (length (nat-tuple-list nt)))

(defun nat-tuple (&rest elements)
  "Create a new nat tuple with the elements ELEMENTS (each of type NAT-TUPLE-ELEMENT)."
  (let ((membership 0))
    (declare (type non-negative-fixnum membership))
    (dolist (elt elements)
      (setf membership (dpb 1 (byte 1 elt) membership)))
    (%make-nat-tuple :list (reverse elements)
                     :membership membership)))

(defmacro do-nat-tuple ((i elt nt) &body body)
  "Iterate over the elements of the nat tuple NT in increasing order. The return value is unspecified.

I will be bound to the (zero-indexed) index of the element found.

ELT will be bound to the element itself.

NT should be the bit set."
  (check-type i symbol)
  (check-type elt symbol)
  (let ((g-nt (gensym "NT-")))
    `(loop :with ,g-nt := (nat-tuple-list ,nt)
           :for ,i :from 0
           :for ,elt :of-type nat-tuple-element :in ,g-nt
           :do (progn
                 ,@body))))

(defun nat-tuple-complement (n nt)
  "Compute the complement of the nat tuple NT in a universe of (0 1 2 ... N-1)."
  (let* ((comp-membership (logandc2 (1- (expt 2 n))
                                    (nat-tuple-membership nt)))
         (comp-list (loop :for i :below n
                          :when (logbitp i comp-membership)
                            :collect i)))
    (%make-nat-tuple :list comp-list
                     :membership comp-membership)))

(deftype permutation ()
  "A qubit permutation."
  `(simple-array nat-tuple-elemnt (*)))

(defun make-identity-permutation (n)
  (check-type n nat-tuple-cardinality)
  (loop :with perm := (make-array n :element-type 'nat-tuple-element
                                    :initial-element 0)
            :for i :below n
            :do (setf (aref perm i) i)
            :finally (return perm)))

(defun permutation-to-nat-tuple (perm)
  (apply #'nat-tuple (nreverse (coerce perm 'list))))

;;; Complex Linear Algebra

(deftype flonum ()
  "The float type used in computations."
  `double-float)

(deftype cflonum ()
  "The complex float type used in computations. Typically these will represent wavefunction amplitudes."
  `(complex flonum))

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

(declaim (ftype (function (fixnum &rest number) quantum-state) make-vector))
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
  (defun matrix-multiply-code (n matrix column)
    "Compute the product of the complex matrix (represented as a square array of CFLONUMs) and a complex column vector (represented as a CFLONUM vector)."
    (check-type n unsigned-byte)
    `(locally
         (declare (type (quantum-operator ,n) ,matrix)
                  (type (quantum-state ,n) ,column)
                  (optimize speed (safety 0) (debug 0) (space 0)))
       (let ((result (make-vector ,n))
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
     (declaim (inline ,name))
     (declaim (ftype (function ((quantum-operator ,size)
                                (quantum-state ,size))
                               (quantum-state ,size))
                     ,name))
     (defun ,name (m c)
       ,(matrix-multiply-code size 'm 'c))
     (declaim (notinline ,name))))

(define-matmul matmul2 2)
(define-matmul matmul4 4)

(defun matrix-multiply (matrix column)
  "Compute the product of the complex matrix (represented as a square array of CFLONUMs) and a complex vector (represented as a CFLONUM vector)."
  (declare (type quantum-operator matrix)
           (type quantum-state column)
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
      ((2) (matmul2 matrix column))
      ((4) (matmul4 matrix column))
      (otherwise
       (let ((result (make-vector matrix-size)))
         (dotimes (i matrix-size result)
           (let ((element (cflonum 0)))
             (declare (type cflonum element))
             (dotimes (j matrix-size)
               (incf element (* (aref matrix i j) (aref column j))))
             (setf (aref result i) element))))))))

(defmacro probabilistically (p &body body)
  "Execute BODY with probability 0 <= P <= 1."
  `(when (<= (random 1.0) ,p)
     ,@body))

;;; Some system-level definitions.

#+unix
(cffi:defcfun (sysconf "sysconf") :long
  (name :int))

(defun count-logical-cores ()
  "Compute the number of logical cores on the machine."
  #+unix
  (or (ignore-errors (sysconf $sc-nprocessors-onln)) 1)

  #-unix
  1)

(let ((prepared? nil)
      (workers-allotted nil))
  (defun prepare-for-parallelization (&optional num-workers)
    "Prepare for parallelization with the correct number of workers scaling with the number of logical cores of your machine.

If NUM-WORKERS is provided, it can force the number of workers. If it's greater than 1, then it should be less than the number of logical cores of your machine.

NOTE: This must be done before computations can be done.
"
    (check-type num-workers (or null (integer 1)))
    (let ((num-logical-cores (or num-workers (count-logical-cores))))
      (assert (or (null workers-allotted)
                  (= 1 workers-allotted)
                  (< workers-allotted num-logical-cores))
              ()
              "The number of workers for parallelization exceeds the ~
               number of cores. This could be because ~
               #'QVM:PREPARE-FOR-INITIALIZATION was called too early. ~
               The number of workers is ~D and the number of logical ~
               cores is ~D."
              workers-allotted
              num-logical-cores)
      (unless prepared?
        (let ((num-workers (max 1 num-logical-cores)))
          (setf lparallel:*kernel*
                (lparallel:make-kernel num-workers :name "QVM Worker"))
          (setf workers-allotted num-workers)
          (setf prepared? t))))

    (values)))
