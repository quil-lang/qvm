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

(deftype nat-tuple ()
  "The NAT-TUPLE type. Represents an ordered list of non-negative integer indexes."
  `list)

(deftype nat-tuple-element ()
  "The set of valid elements in a bit set."
  `(integer 0 (#.+max-nat-tuple-cardinality+)))

(deftype nat-tuple-cardinality ()
  "A representation of the set of cardinalities of a bit set."
  `(integer 0 #.+max-nat-tuple-cardinality+))

(declaim (inline make-nat-tuple
                 nat-tuple-add
                 nat-tuple-remove
                 nat-tuple-cardinality
                 nat-tuple-union
                 nat-tuple-difference))

(defun make-nat-tuple ()
  "Make a new (immutable) empty bit set."
  nil)

(defun nat-tuple-add (bs elt)
  "Add the element ELT (of type NAT-TUPLE-ELEMENT) to the bitset BS."
  (declare (type nat-tuple bs)
           (type nat-tuple-element elt))
  (if (find elt bs)
      bs
      (cons elt bs)))

(defun nat-tuple-remove (bs elt)
  "Remove, if it exists, the element ELT (of type NAT-TUPLE-ELEMENT) to the bitset BS."
  (declare (type nat-tuple bs)
           (type nat-tuple-element elt))
  (remove elt bs))

(defun nat-tuple-cardinality (bs)
  "Compute the number of elements in the bit set BS."
  (declare (type nat-tuple bs))
  (length bs))

(defun nat-tuple-union (bs1 bs2)
  "Compute the union of the bit sets BS1 and BS2."
  (declare (type nat-tuple bs1 bs2))
  (union bs1 bs2))

(defun nat-tuple-difference (bs1 bs2)
  "Compute the set difference between the bit sets BS1 and BS2. The resulting bit set will contain all of the elements in BS1 but not in BS2."
  (declare (type nat-tuple bs1 bs2))
  (set-difference bs1 bs2))

(defun nat-tuple (&rest elements)
  "Create a new bit set with the elements ELEMENTS (each of type NAT-TUPLE-ELEMENT)."
  (declare (dynamic-extent elements))
  (let ((bs (make-nat-tuple)))
    (declare (type nat-tuple bs))
    (dolist (elt elements bs)
      (setf bs (nat-tuple-add bs elt)))))

(defmacro do-nat-tuple ((i elt bs) &body body)
  "Iterate over the elements of the bit set BS in increasing order. The return value is unspecified.

I will be bound to the (zero-indexed) index of the element found.

ELT will be bound to the element itself.

BS should be the bit set."
  (check-type i symbol)
  (check-type elt symbol)
  (let ((g-bs (gensym "BS-")))
    `(loop :with ,g-bs := ,bs
           :for ,i :from 0
           :for ,elt :of-type nat-tuple-element :in ,g-bs
           :do (progn
                 ,@body))))

;;; Complex Linear Algebra

(defun make-vector (size &rest elements)
  "Make a SIZE-length complex vector whose elements are ELEMENTS."
  (let ((vec (make-array size :element-type '(complex double-float)
                              :initial-element #C(0.0d0 0.0d0))))
    (loop :for i :from 0
          :for raw-element :in elements
          :for element :of-type (complex double-float) := (coerce raw-element '(complex double-float))
          :do (setf (aref vec i) element)
          :finally (return vec))))

(defun make-matrix (size &rest elements)
  "Make a SIZE x SIZE complex matrix whose elements are ELEMENTS. Each of ELEMENTS must be able to be coerced into a complex double-float."
  (declare (dynamic-extent elements))
  (let ((matrix (make-array (list size size)
                            :element-type '(complex double-float)
                            :initial-element #C(0.0d0 0.0d0))))
    (loop :for i :from 0
          :for raw-element :in elements
          :for element :of-type (complex double-float) := (coerce raw-element '(complex double-float))
          :do (setf (row-major-aref matrix i) element)
          :finally (return matrix))))

(defun matrix-multiply (matrix column)
  "Compute the product of the complex matrix (represented as a square array of complex double-floats) and a complex column vector (represented as a complex double-float vector)."
  (assert (= (array-dimension matrix 0)
             (array-dimension matrix 1))
          (matrix)
          "The given matrix isn't square.")
  (assert (= (array-dimension matrix 0)
             (length column))
          (matrix column)
          "The given matrix and column vector don't have compatible dimensions.")
  (let* ((matrix-size (array-dimension matrix 0))
         (result (make-array matrix-size)))
    (dotimes (i matrix-size result)
      (let ((element #C(0.0d0 0.0d0)))
        (dotimes (j matrix-size)
          (incf element (* (aref matrix i j) (aref column j))))
        (setf (aref result i) element)))))
