;;;; src/mpi.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(defvar +rank+ nil "MPI rank in the MPI_COMM_WORLD communicator.")

(defconstant +mpi-cflonum+ (ecase qvm::+octets-per-cflonum+
                             (8 mpi::+mpi-complex+)
                             (16 mpi::+mpi-double-complex+))
  "Size (in octets) per amplitude.")

(defun bcast (&key (value 0) (type '(unsigned-byte 64)))
  "Broadcast a single VALUE of type ELEMENT-TYPE from MPI rank zero to all ranks. Returns the broadcasted VALUE."
  (with-static-vector (array 1 :element-type type
                               :initial-element value)
    (mpi-bcast array 0)
    (aref array 0)))

(defun bcast-instructions (&optional (program nil))
  "Broadcast instructions in PROGRAM and return vector of GATE-APPLICATION objects."
  (if program
      (let ((instructions (quil:parsed-program-executable-code program)))
        (map 'vector 'string->instruction ; XXX do we need so many MAPs?
             (mpi-broadcast-anything 0 :object (map 'vector 'instruction->string instructions))))
      (map 'vector 'string->instruction (mpi-broadcast-anything 0))))

(defun barrier (&optional (control-string nil) &rest format-arguments)
  "Enforce an MPI barrier."
  (when control-string
    (apply #'format-log :debug control-string format-arguments))
  (mpi-barrier))

(defmacro with-mpi-type-indexed-block ((new-type count blocklength displacements oldtype) &body body)
  "Create a new MPI data type within the given scope."
  `(mpi::with-foreign-results ((,new-type 'mpi:mpi-datatype))
     (mpi::%mpi-type-create-indexed-block ,count ,blocklength ,displacements ,oldtype ,new-type)
     (mpi::%mpi-type-commit ,new-type)
     (unwind-protect
          (progn
            ,@body)
       (mpi::%mpi-type-free ,new-type))))

(defmacro with-mpi-type-indexed-blocks (bindings &body body)
  (if bindings
      `(with-mpi-type-indexed-block ,(first bindings)
         (with-mpi-type-indexed-blocks ,(rest bindings)
           ,@body))
      `(progn
         ,@body)))

(defun mpi-get-count (status datatype)
  "High level interface to MPI_Get_count."
  (mpi::with-foreign-results ((count :int))
    (mpi::%mpi-get-count status datatype count)))

(defclass requests ()
  ((count
    :reader request-count
    :type alexandria:non-negative-fixnum
    :initform 0
    :documentation "Current number of MPI_Requests in use.")
   (total
    :reader total
    :type alexandria:non-negative-fixnum
    :initarg :total
    :initform (error-missing-initform :total)
    :documentation "Total number of MPI_Requests allocated.")
   (array-ptr
    :reader array-ptr
    :initform nil
    :documentation "Pointer to array of MPI_Requests in foreign memory."))

  (:documentation "Container of MPI_Request types."))

(defmethod initialize-instance :after ((requests requests) &rest initargs)
  (declare (ignore initargs))

  (let ((ptr (cffi:foreign-alloc 'mpi::mpi-request ; XXX use QVM's allocation?
                                 :count (total requests))))
    (setf (slot-value requests 'array-ptr) ptr)
    (trivial-garbage:finalize requests (lambda ()
                                         (cffi:foreign-free ptr)))))

(defmethod reset-requests ((requests requests))
  (setf (slot-value requests 'count) 0))

(defmethod get-next-request ((requests requests))
  (let ((count (request-count requests)))

    (setf (slot-value requests 'count) (1+ count))
    (if (< count (total requests))
        (cffi:mem-aptr (array-ptr requests) 'mpi-request count)
        (error "Number of available MPI_Requests exceeded."))))

(defmethod wait-all ((requests requests))
  (mpi::%mpi-waitall (request-count requests) (array-ptr requests) (cffi:null-pointer))
  (reset-requests requests))
