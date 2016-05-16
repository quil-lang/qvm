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
  "The NAT-TUPLE type. A \"nat tuple\" represents an ordered list of non-negative integer indexes."
  `list)

(deftype nat-tuple-element ()
  "The set of valid elements in a nat tuple."
  `(integer 0 (#.+max-nat-tuple-cardinality+)))

(deftype nat-tuple-cardinality ()
  "A representation of the set of cardinalities of a nat tuple."
  `(integer 0 #.+max-nat-tuple-cardinality+))

(declaim (inline make-nat-tuple
                 nat-tuple-add
                 nat-tuple-remove
                 nat-tuple-cardinality
                 nat-tuple-union
                 nat-tuple-difference))

(defun make-nat-tuple ()
  "Make a new (immutable) empty nat tuple."
  nil)

(defun nat-tuple-add (nt elt)
  "Add the element ELT (of type NAT-TUPLE-ELEMENT) to the nat tuple NT."
  (declare (type nat-tuple nt)
           (type nat-tuple-element elt))
  (if (find elt nt)
      nt
      (cons elt nt)))

(defun nat-tuple-remove (nt elt)
  "Remove, if it exists, the element ELT (of type NAT-TUPLE-ELEMENT) to the nat tuple NT."
  (declare (type nat-tuple nt)
           (type nat-tuple-element elt))
  (remove elt nt))

(defun nat-tuple-cardinality (nt)
  "Compute the number of elements in the nat tuple NT."
  (declare (type nat-tuple nt))
  (length nt))

(defun nat-tuple-union (nt1 nt2)
  "Compute the union of the nat tuple NT1 and NT2."
  (declare (type nat-tuple nt1 nt2))
  ;; FIXME? This will produce non-deterministic ordering of the
  ;; output.
  (union nt1 nt2))

(defun nat-tuple-difference (nt1 nt2)
  "Compute the set difference between the nat tuples NT1 and NT2. The resulting nat tuple will contain all of the elements in NT1 but not in NT2."
  (declare (type nat-tuple nt1 nt2))
  ;; FIXME? This will produce non-deterministic ordering of the
  ;; output.
  (set-difference nt1 nt2))

(defun nat-tuple (&rest elements)
  "Create a new bit set with the elements ELEMENTS (each of type NAT-TUPLE-ELEMENT)."
  (declare (dynamic-extent elements))
  (let ((nt (make-nat-tuple)))
    (declare (type nat-tuple nt))
    (dolist (elt elements nt)
      (setf nt (nat-tuple-add nt elt)))))

(defmacro do-nat-tuple ((i elt nt) &body body)
  "Iterate over the elements of the nat tuple NT in increasing order. The return value is unspecified.

I will be bound to the (zero-indexed) index of the element found.

ELT will be bound to the element itself.

NT should be the bit set."
  (check-type i symbol)
  (check-type elt symbol)
  (let ((g-nt (gensym "NT-")))
    `(loop :with ,g-nt := ,nt
           :for ,i :from 0
           :for ,elt :of-type nat-tuple-element :in ,g-nt
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
