;;;; allocator.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file manages the allocation of CFLONUM vectors in different
;;; spaces.
;;;
;;; There are two components to this file: allocation descriptions,
;;; and the allocation method.
;;;
;;; Allocation descriptions describe how much memory to allocate and
;;; where. It's all specialized on CFLONUM-VECTORs right now, since
;;; that's mostly all the QVM uses. These are implemented as classes,
;;; that at least have some sort of ALLOCATION-LENGTH method to
;;; indicate how many CFLONUMs should be allocated.
;;;
;;; The allocation generic function is ALLOCATE-VECTOR. This function
;;; is specialized on the aforementioned types. Critically, this
;;; function provides both the allocated data, as well as some
;;; finalizing function to free the memory. Since data might be
;;; allocated somewhere else besides the managed Lisp heap, it's up to
;;; you, the programmer, to call the freeing function. (Save us the
;;; trouble and don't cheat! Even with LISP-ALLOCATION vectors! If you
;;; decide to change your mind later you'll regret it! If you don't
;;; want to use the allocation interface, use
;;; MAKE-LISP-CFLONUM-VECTOR.)
;;;
;;; Note that not all ALLOCATE-VECTOR specializations live in
;;; this file.

(deftype finalizer ()
  "A finalizer thunk. Used for the effect of freeing some memory."
  '(function () nil))

(defun dummy-finalizer ()
  "A \"finalizer\" that does nothing. Used for objects managed by the GC."
  nil)

;;; If we made this declaration on the DEFGENERIC, the type would get
;;; clobbered. We put this type here anyway, albeit commented out, so
;;; you know what the *real* type of this function should be.
#+#:ignore
(declaim (ftype (function (t) (values cflonum-vector finalizer))
                allocate-vector))
(defgeneric allocate-vector (description)
  (:documentation "Allocate a fresh zero-initialized CFLONUM-VECTOR described by DESCRIPTION. Return two values:

    1. The allocated vector (a CFLONUM-VECTOR).

    2. A finalizer thunk of type FINALIZER, which should be called when the memory is OK to be freed.

NOTE: Note that the finalizer may close over the allocated vector."))

;;; If we made this declaration on the DEFGENERIC, the type would get
;;; clobbered. We put this type here anyway, albeit commented out, so
;;; you know what the *real* type of this function should be.
#+#:ignore
(declaim (ftype (function (t) alexandria:non-negative-fixnum) allocation-length))
(defgeneric allocation-length (description)
  (:documentation "The length of memory to be allocated, measured in the element type of the vector."))


;;;;;;;;;;;;;;;;;;;;;;;; Lisp Heap Allocation ;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lisp-allocation ()
  ((length :initarg :length :reader allocation-length))
  (:documentation "A description of an allocation on the Lisp heap."))

(declaim (inline make-lisp-cflonum-vector))
(defun make-lisp-cflonum-vector (length)
  (make-array length
              :element-type 'cflonum
              :initial-element (cflonum 0)))

(defmethod allocate-vector ((descr lisp-allocation))
  (values (make-lisp-cflonum-vector (allocation-length descr))
          #'dummy-finalizer))


;;;;;;;;;;;;;;;;;;;;; Foreign Memory Allocation ;;;;;;;;;;;;;;;;;;;;;;

(defclass c-allocation ()
  ((length :initarg :length :reader allocation-length))
  (:documentation "A description of an allocation in foreign memory."))

(defmethod allocate-vector ((descr c-allocation))
  (let ((vec (static-vectors:make-static-vector (allocation-length descr)
                                                :element-type 'cflonum
                                                :initial-element (cflonum 0))))
    (values vec
            (lambda ()
              (static-vectors:free-static-vector vec)
              nil))))


;;;;;;;;;;;;;;;;;;; POSIX Shared Memory Allocation ;;;;;;;;;;;;;;;;;;;

(defclass posix-shared-memory-allocation ()
  ((length :initarg :length :reader allocation-length)
   (name :initarg :name :reader allocation-name))
  (:documentation "A description of an allocation in POSIX shared memory."))

;;; ALLOCATE-VECTOR for this class is defined in "shm.lisp", which is
;;; conditional on being a UNIX OS.
