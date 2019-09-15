;;;; src/offset-arrays.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

;;; We define an offset as an index within the portion of the wavefunction
;;; that exists in a given rank. This notion differs from an address, which
;;; is the index of an amplitude within the whole wavefunction (not just the
;;; particular piece stored a specific rank).
;;;
;;; Example:
;;;
;;;     Consider the following partition of the addresses of the wavefunction
;;;     among two ranks:
;;;
;;;       rank #0 | rank #1
;;;       --------+--------
;;;       0 4 2 6 | 1 5 3 7
;;;
;;;     Addresses 0, 4, 2, and 6 are respectively at offsets 0, 1, 2, and 3
;;;     of rank #0's portion of the wavefunction. Similarly, addresses 1, 5,
;;;     3, and 7 are at offsets 0, 1, 2, and 3 of rank #1's portion of the
;;;     wavefunction.
;;;
;;; We use arrays of offsets to tell MPI which pieces of memory should be
;;; accessed when transferring data.
;;;
;;; This file defines the OFFSET-ARRAYS class. This class encapsulates the
;;; set up of auxiliary data for the MPI_Datatype structures required to
;;; off-load the transmission of amplitudes.

;;; Note that the offsets must be 32-bit integers for compatibility with the
;;; signature of MPI_Type_create_indexed_block. The limitations of the use of
;;; the C type int for sizes in MPI are well-known [1] and can be avoided by
;;; splitting the wavefunction into several static vectors and specifying
;;; offsets with respect to each of those.
;;;
;;; Moreover, support for "big counts" is expected to be introduced in MPI
;;; 4.0, to be ratified by 2020 or 2021. At that stage, we could use offsets
;;; of bigger sizes.
;;;
;;; [1] J. R. Hammond, A. Schafer, and R. Latham, “To INT_MAX... and Beyond!
;;; Exploring Large-Count Support in MPI,” in 2014 Workshop on Exascale MPI
;;; at Supercomputing Conference, New Orleans, LA, USA, 2014.

(defclass offset-arrays ()
  ((dimension
    :type alexandria:non-negative-fixnum
    :initarg :dimension
    :initform (error-missing-initform :dimension)
    :documentation "Upper bound on the number of offsets per rank.")
   (counts
    :type (simple-array alexandria:non-negative-fixnum (*))
    :initarg :counts
    :initform (error-missing-initform :counts)
    :documentation "Array containing the number of offsets currently allocated for each rank.")
   (offsets
    :type (simple-array cffi:foreign-pointer (*))
    :initarg :offsets
    :initform (error-missing-initform :offsets)
    :documentation "Array containing pointers to foreign-allocated arrays of size DIMENSION, indexed by rank. The foreign arrays contain offsets to be used during data transfer."))
  (:documentation "Foreign arrays of offsets for MPI."))

(defun make-offset-arrays (global-addresses)
  "Allocate and initialize an instance of OFFSET-ARRAYS."
  (flet ((get-dimension ()
           "Upper bound on the number of offsets per rank."
           (declare (values alexandria:non-negative-fixnum))

           (let ((blocks-per-process (slot-value global-addresses 'blocks-per-process))
                 (remainder-blocks (slot-value global-addresses 'remainder-blocks))
                 (block-size (slot-value global-addresses 'block-size)))

             (declare (type alexandria:non-negative-fixnum blocks-per-process remainder-blocks block-size))

             (* block-size
                (the alexandria:non-negative-fixnum (+ blocks-per-process remainder-blocks))))))

    (let ((number-of-processes (number-of-processes global-addresses))
          (dimension (get-dimension)))

      (flet ((make-counts ()
               "Allocate array of counters indexed by rank."
               (make-array number-of-processes :element-type 'alexandria:non-negative-fixnum
                                               :initial-element 0 :adjustable nil))

             (make-offsets ()
               "Allocate array of pointers to foreign memory indexed by rank. The foreign memory contains offsets to be used for data transfer."
               (make-array number-of-processes
                           :element-type 'cffi:foreign-pointer
                           :initial-contents (loop :repeat number-of-processes
                                                   :collect (cffi:foreign-alloc :int32 :count dimension))
                           :adjustable nil)))

        (let* ((offsets (make-offsets))
               (offset-arrays (make-instance 'offset-arrays :dimension dimension :counts (make-counts) :offsets offsets)))
          (trivial-garbage:finalize offset-arrays (lambda () (map nil #'cffi:foreign-free offsets)))
          offset-arrays)))))

(defun offset-arrays-push (offset rank offset-arrays)
  "Push OFFSET into the array of OFFSET-ARRAYS corresponding to RANK."
  (declare (type (unsigned-byte 32) offset)
           (type alexandria:non-negative-fixnum rank)
           (type offset-arrays offset-arrays))

  (let* ((dimension (slot-value offset-arrays 'dimension))
         (counts (slot-value offset-arrays 'counts))
         (count (aref counts rank))
         (offsets (slot-value offset-arrays 'offsets))
         (ptr-offsets (aref offsets rank)))

    (declare (type alexandria:non-negative-fixnum dimension)
             (type (simple-array alexandria:non-negative-fixnum) counts)
             (type (simple-array cffi:foreign-pointer) offsets))

    (unless (< count dimension)
      (error "Offset array capacity (~d) exceeded when trying to attach offset ~d to rank ~d."
             dimension offset rank))

    (setf (cffi:mem-aref ptr-offsets :int32 count) offset)
    (incf (aref counts rank))))

(defun-inlinable reset-offset-arrays (offset-arrays)
  "Set all the counters of each object in OFFSET-ARRAYS to zero."
  (declare (type offset-arrays offset-arrays))
  (fill (slot-value offset-arrays 'counts) 0))

(defmethod print-object ((offset-arrays offset-arrays) stream)
  (print-unreadable-object (offset-arrays stream :type t :identity t)
    (terpri stream)
    (loop :for rank :from 0
          :for count :across (slot-value offset-arrays 'counts)
          :for offsets :across (slot-value offset-arrays 'offsets) :do
            (format stream "~t~2D:  ~{~2D~^ ~}~%"
                    rank (loop :for i :below count
                               :collect (cffi:mem-aref offsets :int32 i))))))
