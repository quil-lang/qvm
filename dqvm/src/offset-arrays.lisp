(in-package #:dqvm2)

(defclass offset-arrays ()
  ((dimension
    :type alexandria:non-negative-fixnum
    :initarg :dimension
    :initform (error-missing-initform :dimension)
    :documentation "Upper bound on the number of offsets per rank.")
   (counts
    :type array
    :initarg :counts
    :initform (error-missing-initform :counts)
    :documentation "Array containing the number of offsets currently allocated for each rank.")
   (offsets
    :type array
    :initarg :offsets
    :initform (error-missing-initform :offsets)
    :documentation "Array containing pointers to foreign-allocated arrays of size DIMENSION, indexed by rank. The foreign arrays contain offsets to be used during data transfer."))
  (:documentation "Foreign arrays of offsets for MPI."))

(defun make-offset-arrays (global-addresses)
  "Allocate and initialize an instance of OFFSET-ARRAYS."
  (flet ((get-dimension ()
           "Upper bound on the number of offsets per rank."
           (let ((blocks-per-process (blocks-per-process global-addresses))
                 (remainder-blocks (remainder-blocks global-addresses))
                 (block-size (block-size global-addresses)))
             (* block-size (+ blocks-per-process remainder-blocks)))))

    (let ((number-of-processes (number-of-processes global-addresses))
          (dimension (get-dimension)))

      (flet ((make-counts ()
               "Allocate array of counters indexed by rank."
               (make-array number-of-processes :element-type 'alexandria:positive-fixnum :initial-element 0))

             (make-offsets ()
               "Allocate array of pointers to foreign memory indexed by rank. The foreign memory contains offsets to be used for data transfer."
               (make-array number-of-processes
                           :initial-contents (loop :repeat number-of-processes
                                                   :collect (cffi:foreign-alloc :int32 :count dimension)))))

        (loop :with offset-arrays := (make-instance 'offset-arrays :dimension dimension :counts (make-counts) :offsets (make-offsets))
              :for ptr-offsets :across (slot-value offset-arrays 'offsets) :do
                (trivial-garbage:finalize offset-arrays (lambda () (cffi:foreign-free ptr-offsets)))
              :finally (return offset-arrays))))))

(defun offset-arrays-push (offset rank offset-arrays)
  "Push OFFSET into the array of OFFSET-ARRAYS corresponding to RANK."
  (let ((count (aref (slot-value offset-arrays 'counts) rank))
        (offsets (aref (slot-value offset-arrays 'offsets) rank)))

    (let ((dimension (slot-value offset-arrays 'dimension)))
      (unless (< count dimension)
        (error "Offset array capacity (~d) exceeded when trying to attach offset ~d to rank ~d."
               dimension offset rank)))

    (setf (cffi:mem-aref offsets :int32 count) offset)
    (incf (aref (slot-value offset-arrays 'counts) rank))))

(defun reset-offset-arrays (&rest arrays)
  "Set all the counters of each object in ARRAYS to zero."
  (dolist (offset-arrays arrays)
    (fill (slot-value offset-arrays 'counts) 0)))

(defmethod print-object ((offset-arrays offset-arrays) stream)
  (print-unreadable-object (offset-arrays stream :type t :identity t)
    (terpri stream)
    (loop :for rank :from 0
          :for count :across (slot-value offset-arrays 'counts)
          :for offsets :across (slot-value offset-arrays 'offsets) :do
            (format stream "~t~2D:  ~{~2D~^ ~}~%"
                    rank (loop :for i :below count
                               :collect (cffi:mem-aref offsets :int32 i))))))
