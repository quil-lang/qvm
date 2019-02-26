;;;; shm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file contains routines for dealing with shared memory on a
;;; POSIX system.
;;;

;;;
;;; Dear Reader:
;;;
;;;     The goal of these routines is to allow a wavefunction to be
;;; allocated in a place that other processes can access. By default,
;;; Lisp-allocated data lives in the Lisp "heap", a dynamic space
;;; where memory moves around and garbage is collected. Such aspects
;;; are not acceptable to other applications because they cannot keep
;;; track of when and how these movements happen. As such, we ask our
;;; POSIX-compliant operating system kernel to reserve some physical
;;; memory with an agreed-upon name and allow processes to access this
;;; memory by that name. A process accesses this memory by way of a
;;; _memory map_, where a segment of the process's virtual address
;;; space maps to a segment of physical address space (the latter of
;;; which is seen and understood only by the kernel).
;;;
;;; Not all is well, though. Lisp's allocating functions (like CONS or
;;; MAKE-ARRAY) cannot be commanded where to allocate memory. So we
;;; must build these objects ourselves in these spaces by manual
;;; memory poking, according to our Lisp implementation's internal
;;; rules on object memory layout. Since we are only concerned with
;;; specialized SIMPLE-ARRAYs, we re-use internal functionality of the
;;; STATIC-VECTORS package, which deals with allocating Lisp objects
;;; managed by libc malloc.
;;;
;;; Here's the rundown of what we do:
;;;
;;;     1. Allocate memory using POSIX shm_open(2), and map it to our
;;;        process's virtual address space with mmap(2). We allocate
;;;        enough memory for our wavefunction vector plus some room
;;;        for our Lisp array object header. (Lisp needs this header
;;;        data so it can tell what the array is all about: its length
;;;        and element type.)
;;;
;;;        We store all of this data in a POSIX-SHARED-MEMORY
;;;        structure, and remember that we allocated it using a
;;;        weak-valued hash table called **SHARED-MEMORIES**. We
;;;        remember it in a table because of the following important
;;;        fact:
;;;
;;;           SHARED MEMORY HAS KERNEL PERSISTENCY. EVEN IF YOU QUIT
;;;           YOUR APPLICATION, THE MEMORY WILL STILL BE ALLOCATED. IT
;;;           CAN ONLY BE DEALLOCATED BY THE KERNEL IF YOU EXPLICITLY
;;;           TELL THE KERNEL YOU'RE FINISHED WITH IT.
;;;
;;;     2. Make a Lisp object out of this with
;;;        MAKE-VECTOR-FROM-POINTER. The *only* benefit we get by
;;;        doing this is that you can use all of the normal Lisp
;;;        functions to manipulate the vector, like AREF and
;;;        LENGTH. Since the following confused me, I must relay it to
;;;        you loud and clear:
;;;
;;;           DESPITE BEING AN HONEST-TO-GOD LISP OBJECT, LISP WILL
;;;           NOT MANAGE THE DEALLOCATION OF THIS OBJECT BECAUSE IT
;;;           DOES NOT LIVE IN A SPACE THAT IS MANAGED BY THE LISP
;;;           SYSTEM. GARBAGE COLLECTION DOESN'T WORK
;;;           HERE. FINALIZATION DOESN'T WORK HERE.
;;;
;;;     3. Return the Lisp vector along with a thunk to deallocate
;;;        it. The caller has responsibility for deallocating the
;;;        vector by calling this thunk. The thunk can be used as a
;;;        finalizer if the caller so pleases. For the QVM application
;;;        code, we attach a finalizer to the QVM object which owns
;;;        the wavefunction vector.
;;;
;;;     4. Because it's never safe to trust the user, we add an "exit
;;;        hook" that will deallocate all shared memories
;;;        unconditionally upon normal application termination.
;;;
;;; With all that said, here are a few things that *don't* work:
;;;
;;;     - Attaching a finalizer to the POSIX-SHARED-MEMORY
;;;       structure. You _could_ do this, but you really want memory
;;;       deallocation to be associated with the Lisp object.
;;;
;;;     - Attaching a finalizer to the Lisp object. This won't work
;;;       because the finalizer will never be called, because Lisp
;;;       doesn't own this object in its managed heap.
;;;
;;;     - Allocating Lisp memory in SBCL's static/immobile space a la
;;;       SB-INT:MAKE-STATIC-VECTOR. This would be perfect, because
;;;       Lisp manages it, but the static memory space is too small to
;;;       be useful. (If, in the future, the space got larger, then we
;;;       could possibly mmap(2) the shared memory a base address in
;;;       this space using MAP_FIXED.)
;;;
;;;                                     Sincerely Yours,
;;;
;;;                                     Robert Smith

;;;;;;;;;;;;;;;;;;;;;;;; POSIX SHARED MEMORY ;;;;;;;;;;;;;;;;;;;;;;;;;

#+(and unix (not darwin))
(progn
  (cffi:define-foreign-library libposix
    (:unix (:or "libposix.so" "librt.so.1")))

  (cffi:use-foreign-library libposix))

(cffi:defcfun (%strerror "strerror") :string
  (errnum :int))

;;; Bashing an array into shared memory.

(defun simple-array-allocation-size (num-elements element-type)
  ;; Total amount of allocation required in memory to store a Lisp SIMPLE-ARRAY.
  #-(or sbcl ccl)
  (locally (declare (ignore num-elements element-type))
    (error "SIMPLE-ARRAY-ALLOCATION-SIZE (of ~A ~A~:P) unsupported on ~A"
           num-elements element-type
           (lisp-implementation-type)))

  (shm-vector-allocation-size num-elements element-type))

(defun make-vector-from-pointer (pointer num-elements element-type)
  #-(or sbcl ccl)
  (locally (declare (ignore num-elements element-type))
    (error "MAKE-VECTOR-FROM-POINTER unsupported on ~A"
           (lisp-implementation-type)))

  (shm-vector-from-pointer pointer num-elements element-type))

;;; General POSIX routines.

(global-vars:define-global-var **shared-memories** (tg:make-weak-hash-table
                                                    :weakness :value
                                                    :test 'equal)
  "A table mapping shared memory names to the shared memory objects, weakly on value.")

(defstruct (posix-shared-memory (:constructor %make-shared-memory))
  "Representation of some POSIX shared memory."
  name
  size
  pointer)

(cffi:defcfun (shm-open "shm_open") :int
  (name :string)
  (oflag :int)
  &rest)

(cffi:defcfun (shm-unlink "shm_unlink") :int
  (name :string))

(cffi:defcfun getpagesize :int)

(cffi:defcfun (%ftruncate "ftruncate") :int
  (fd :int)
  (offset off_t))

(cffi:defcfun (%mmap "mmap") :pointer
  (addr :pointer)
  (len size_t)
  (prot :int)
  (flags :int)
  (fd :int)
  (offset off_t))

(cffi:defcfun (%munmap "munmap") :int
  (addr :pointer)
  (len size_t))

(cffi:defcfun (%close "close") :int
  (fd :int))

(defun round-to-next-page (size)
  (check-type size (and fixnum unsigned-byte))
  (round-up-to-next-multiple size (getpagesize)))

(defun make-posix-shared-memory (name size)
  "Allocate POSIX shared memory, named by NAME whose size is at least SIZE octets. (It will be rounded to the next multiple of a page.)

Return a POSIX-SHARED-MEMORY object."
  (check-type name string)
  (check-type size (and fixnum unsigned-byte))
  ;; mmap(2) allocates in multiples of pages.
  (let* ((num-bytes (round-to-next-page size))
         (fd (shm-open name (logior
                             ;; Make the memory.
                             $o-creat
                             ;; But only if the name is unique.
                             $o-excl
                             ;; And allow for reading/writing.
                             $o-rdwr)
                       ;; rw-rw-rw-
                       mode_t #o666)))
    (when (minusp fd)
      (error "Error in shm_open. Got return code ~D: ~A"
             fd
             (%strerror %errno)))

    ;; Extend the mapped memory to a particular length.
    (let ((status (%ftruncate fd num-bytes)))
      (unless (zerop status)
        (error "Error in ftruncate. Got return code ~D" status)))

    ;; Map the memory.
    (let ((ptr (%mmap (cffi:null-pointer)
                      num-bytes
                      (logior $prot-read
                              $prot-write)
                      $map-shared
                      fd
                      0)))
      (when (eql (cffi:pointer-address ptr) $map-failed)
        (error "Error in mmap. Got MAP_FAILED return code"))
      ;; Close the fd, we don't need it after we've mmapped.
      (%close fd)

      ;; Package everything up.
      (let ((shm (%make-shared-memory
                  :name name
                  :size num-bytes
                  :pointer ptr)))
        ;; Record the object.
        (setf (gethash name **shared-memories**) shm)

        ;; Return the shared memory.
        shm))))

(defun free-posix-shared-memory (shm)
  "Free the memory associated with the POSIX shared memory object SHM."
  ;; Unmap the memory.
  (let ((status (%munmap (posix-shared-memory-pointer shm)
                         (posix-shared-memory-size shm))))
    (unless (zerop status)
      (error "Error in munmap for shared memory ~A" shm))
    (setf (posix-shared-memory-pointer shm) (cffi:null-pointer)
          (posix-shared-memory-size shm) 0))

  ;; Unlink the shared memory.
  (let ((status (shm-unlink (posix-shared-memory-name shm))))
    (unless (zerop status)
      (error "Error in shm_unlink for shared memory ~A. Error code ~D: ~A"
             shm
             status
             (%strerror %errno))))
  nil)

(defun deallocate-all-shared-memories ()
  "Deallocate all valid shared memories that we've remembered."
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (and (posix-shared-memory-p v)
                        (not (cffi:null-pointer-p
                              (posix-shared-memory-pointer v))))
               (free-posix-shared-memory v)))
           **shared-memories**)
  (clrhash **shared-memories**)
  nil)

;; Make ABSOLUTELY SURE that everything gets freed at the end of the
;; day, even if it means making invalid objects. Shared memories have
;; kernel persistence, which means if we allocate and never free, the
;; user has to pay the price.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (call-at-exit 'deallocate-all-shared-memories))

(declaim (notinline posix-shared-memory-finalizer))
(defun posix-shared-memory-finalizer (shm)
  "Return a thunk suitable to releasing the memory associated with the POSIX-SHARED-MEMORY object SHM."
  (lambda ()
    (unless (cffi:null-pointer-p (posix-shared-memory-pointer shm))
      (free-posix-shared-memory shm))
    ;; Be consistent with the FINALIZER type defined
    ;; elsewhere.
    nil))

(defun make-shared-array (name length element-type)
  "Return an array allocated to shared memory, along with a thunk which releases this memory."
  (let* ((size (simple-array-allocation-size length element-type))
         (shm (make-posix-shared-memory name size))
         (shm-finalizer (posix-shared-memory-finalizer shm)))
    (multiple-value-bind (vec vec-finalizer)
        (make-vector-from-pointer (posix-shared-memory-pointer shm)
                                  length
                                  element-type)
      (values vec 
              (lambda ()
                ;; Order matters!
                (funcall vec-finalizer)
                (funcall shm-finalizer)
                ;; Be consistent with the FINALIZER type defined
                ;; elsewhere.
                nil)))))

;;; Plug in to the allocation interface defined in "allocator.lisp".
(defmethod allocate-vector ((descr posix-shared-memory-allocation))
  (make-shared-array (allocation-name descr)
                     (allocation-length descr)
                     'cflonum))
