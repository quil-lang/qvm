;;;; classical-memory.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;;;;;;;;;; Allocation & Referencing of Classical Memory ;;;;;;;;;;;;

;;; Quil defines what a BIT and OCTET are precisely, hence the
;;; name. What's left unspecified are the structure of INTEGER and
;;; REAL, hence their specific QVM-* name.
;;;
;;; See also the QUIL:QUIL-TYPE ADT.
(deftype quil-bit    () 'bit)
(deftype quil-octet  () '(unsigned-byte 8))
(deftype qvm-integer () '(signed-byte 64))
(deftype qvm-real    () 'double-float)

(defun quil-name (type-name)
  (ecase type-name
    (quil-bit    "BIT")
    (quil-octet  "OCTET")
    (qvm-integer "INTEGER")
    (qvm-real    "REAL")))

(global-vars:define-global-parameter **alignment** 8)

(global-vars:define-global-parameter **real-bits** 64
  "The size of a REAL in bits.")

(global-vars:define-global-parameter **integer-bits** 64
  "The size of an INTEGER in bits.")

(defun size-of (type-name)
  "The size of a type in octets."
  (ecase type-name
    (quil-bit    1/8)
    (quil-octet  1)
    (qvm-integer (/ **integer-bits** 8))
    (qvm-real    (/ **real-bits** 8))))

;;; We allocate CLASSICAL-MEMORY data into the C heap so we can do
;;; aliasing tricks. It also makes the memory amenable to being shared
;;; in the future.

(defstruct (classical-memory (:constructor %make-classical-memory (size pointer))
                             (:conc-name memory-)
                             (:copier nil))
  "A representation of a chunk of allocated classical memory."
  ;; SIZE here is measured in OCTETS!
  (size    0                   :read-only t :type alexandria:non-negative-fixnum)
  (pointer (cffi:null-pointer) :read-only t :type cffi:foreign-pointer))

(define-condition memory-index-out-of-bounds (error)
  ((index :initarg :index
          :reader oob-index)
   (from :initarg :from
         :reader oob-from)
   (to :initarg :to
       :reader oob-to)
   (name :initarg :name
         :initform nil
         :reader oob-name))
  (:report (lambda (condition stream)
             (format stream "The index ~A ~@[of memory ~S ~]is supposed to be within the interval [~A, ~A)."
                     (oob-index condition)
                     (oob-name condition)
                     (oob-from condition)
                     (oob-to condition)))))

(defun assert-in-bounds (i a b &optional name)
  (unless (and (<= a i) (< i b))
    (error 'memory-index-out-of-bounds :index i :from a :to b :name name)))

(defun check-classical-memory-bounds (i type-name mem)
  (let ((actual-index (* i (size-of type-name))))
    (assert-in-bounds actual-index 0 (memory-size mem) (quil-name type-name))))

(defmethod print-object ((obj classical-memory) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "(~DB)" (memory-size obj))
    (write-char #\[ stream)
    (dotimes (i (min 16 (memory-size obj)))
      (format stream "~2,'0X" (cffi:mem-aref (memory-pointer obj) ':uint8 i)))
    (when (< 16 (memory-size obj))
      (write-string "..." stream))
    (write-char #\] stream)))

(defun make-classical-memory (size)
  "Allocate SIZE octets of memory."
  (check-type size unsigned-byte)
  (let* ((pointer (cffi:foreign-alloc ':uint8 :initial-element 0 :count size))
         ;; XXX DANGER WILL ROBINSON DANGER
         ;;
         ;; In the hierarchical classical memory model, we can have
         ;; *aliasing memory*. Here, we are finalizing the
         ;; CLASSICAL-MEMORY object so that the raw C memory will get
         ;; freed when the CLASSICAL-MEMORY becomes
         ;; unreachable. However (!!), if there is a raw pointer
         ;; derived from this CLASSICAL-MEMORY pointer---which can
         ;; happen if we alias memory---and the original
         ;; CLASSICAL-MEMORY becomes unreachable, then the derived
         ;; pointer will be invalid.
         ;;
         ;; We don't universally ameliorate this problem, i.e., if you
         ;; use this function and do pointer mumbo jumbo, you're on
         ;; your own. However, within the QVM, we only create aliasing
         ;; memory in the function ALLOCATE-MEMORY-FOR-MODEL. Memory
         ;; aliases materialize as MEMORY-VIEWs. This *will* create a
         ;; derived pointer, but we hold on to a reference to the
         ;; aliased classical memory in read-only slots. In other
         ;; words, within this file, if a MEMORY-VIEW is created for
         ;; an aliasing CLASSICAL-MEMORY, it'll hold on to a reference
         ;; to the original aliased CLASSICAL-MEMORY as well, to avoid
         ;; the above problem.
         (free-pointer (lambda () (cffi:foreign-free pointer)))
         (obj (%make-classical-memory size pointer)))
    (tg:finalize obj free-pointer)
    obj))

(defun slurp-memory (memory)
  "Slurp all of the memory of MEMORY into a vector."
  (check-type memory classical-memory)
  (let ((v (make-array (memory-size memory) :element-type 'quil-octet
                                            :initial-element 0))
        (p (memory-pointer memory)))
    (dotimes (i (memory-size memory) v)
      (setf (aref v i) (cffi:mem-aref p ':uint8 i)))))

(defun barf-memory (vector pointer)
  "Barf out the memory designated by the vector of QUIL-OCTETS into the pointer POINTER."
  (check-type vector simple-vector)
  (check-type pointer cffi:foreign-pointer)
  (dotimes (i (length vector) pointer)
    (let ((o (aref vector i)))
      (check-type o quil-octet)
      (setf (cffi:mem-aref pointer ':uint8 i) o))))

(defmethod make-load-form ((obj classical-memory) &optional env)
  (declare (ignore env))
  (alexandria:with-gensyms (mem ptr static-elts)
    `(let* ((,mem (make-classical-memory ',(memory-size obj)))
            (,ptr (memory-pointer ,mem))
            (,static-elts ',(slurp-memory obj)))
       (barf-memory ,static-elts ,ptr)
       ,mem)))

;;; All of the functions below are really strictly type checked so we
;;; aren't doing something unsafe at the lowest level.

(defun memory-bit-ref (mem i)
  (check-classical-memory-bounds i 'quil-bit mem)
  (multiple-value-bind (byte bit) (floor i 8)
    (if (logbitp bit (cffi:mem-aref (memory-pointer mem) ':uint8 byte))
        1
        0)))

(defun (setf memory-bit-ref) (new-bit mem i)
  (check-type new-bit quil-bit)
  (check-classical-memory-bounds i 'quil-bit mem)
  (multiple-value-bind (byte bit) (floor i 8)
    (let* ((old-byte (cffi:mem-aref (memory-pointer mem) ':uint8 byte))
           (new-byte (dpb new-bit (byte 1 bit) old-byte)))
      (setf (cffi:mem-aref (memory-pointer mem) ':uint8 byte) new-byte)
      new-bit)))

(defun memory-octet-ref (mem i)
  (check-classical-memory-bounds i 'quil-octet mem)
  (cffi:mem-aref (memory-pointer mem) ':uint8 i))

(defun (setf memory-octet-ref) (new-oct mem i)
  (check-type new-oct quil-octet)
  (check-classical-memory-bounds i 'quil-octet mem)
  (setf (cffi:mem-aref (memory-pointer mem) ':uint8 i) new-oct))

(defun memory-integer-ref (mem i)
  (check-classical-memory-bounds i 'qvm-integer mem)
  (cffi:mem-aref (memory-pointer mem) ':int64 i))

(defun (setf memory-integer-ref) (new-int mem i)
  (check-type new-int qvm-integer)
  (check-classical-memory-bounds i 'qvm-integer mem)
  (setf (cffi:mem-aref (memory-pointer mem) ':int64 i) new-int))

(defun memory-real-ref (mem i)
  (check-classical-memory-bounds i 'qvm-real mem)
  (cffi:mem-aref (memory-pointer mem) ':double i))

(defun (setf memory-real-ref) (new-real mem i)
  (check-type new-real qvm-real)
  (check-classical-memory-bounds i 'qvm-real mem)
  (setf (cffi:mem-aref (memory-pointer mem) ':double i) new-real))

(defun zero-out-classical-memory (mem)
  "Zero out the classical memory MEM."
  ;; XXX: We could just memset this.
  (dotimes (i (memory-size mem) mem)
    (setf (memory-octet-ref mem i) 0)))

(defun typed-reader/writer (ty)
  (adt:match quil:quil-type ty
    (quil:quil-bit     (values #'memory-bit-ref     #'(setf memory-bit-ref)))
    (quil:quil-octet   (values #'memory-octet-ref   #'(setf memory-octet-ref)))
    (quil:quil-integer (values #'memory-integer-ref #'(setf memory-integer-ref)))
    (quil:quil-real    (values #'memory-real-ref    #'(setf memory-real-ref)))))

;;;;;;;;;;;;;;;;; Classical Memory in Quil Programs ;;;;;;;;;;;;;;;;;;

(defstruct (memory-view (:constructor memory-view
                            (root-classical-memory
                             &key length
                                  reader
                                  writer
                                  ((:aliasing-memory aliasing-classical-memory)))))
  "A \"view\" into a chunk of (possibly aliased) classical memory. The view constitutes the memory itself along with functions to read and write that memory."
  (length (error "You need to provide the length.")
          :read-only t
          :type unsigned-byte)
  ;; We keep the root classical memory around because we need the
  ;; reference. If ROOT-* dies, then ALIASING-* will point to invalid
  ;; memory. This is also why these slots are read-only, so there's no
  ;; funny business of snipped references.
  ;;
  ;; A chunk of classical memory that's read from/written to by the
  ;; READER and WRITER respectively.
  (root-classical-memory (error "You need a root classical memory.")
                         :read-only t
                         :type classical-memory)
  ;; Memory that aliases the root memory.
  (aliasing-classical-memory nil :read-only t :type (or null classical-memory))
  ;; A function
  ;;
  ;;     CLASSICAL-MEMORY * INDEX -> VALUE
  (reader nil :read-only t :type function)
  ;; A function
  ;;
  ;;     NEW-VALUE * CLASSICAL-MEMORY * INDEX -> NEW-VALUE
  (writer nil :read-only t :type function))

(defun root-memory-view-p (mv)
  "Is the MEMORY-VIEW MV a root memory?"
  (null (memory-view-aliasing-classical-memory mv)))

(defun memory-view-classical-memory (mv)
  "Return the CLASSICAL-MEMORY that can be read to/written from as viewed by MEMORY-VIEW."
  (or (memory-view-aliasing-classical-memory mv)
      (memory-view-root-classical-memory mv)))

(defmethod print-object ((obj memory-view) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "of ~DB"
            (memory-size (memory-view-classical-memory obj)))))

(defun memory-view-ref (mv i)
  (funcall (memory-view-reader mv) (memory-view-classical-memory mv) i))

(defun (setf memory-view-ref) (new-value mv i)
  (funcall (memory-view-writer mv) new-value (memory-view-classical-memory mv) i))

(defun memory-descriptors-to-qvm-memory-model (descrs)
  "Given a list of memory descriptors DESCRS (as if by QUIL:PARSED-PROGRAM-MEMORY-DEFINITIONS), produce a memory model object suitable for creating a CLASSICAL-MEMORY-SUBSYSTEM."
  (quil:memory-descriptors-to-model descrs :alignment **alignment**
                                           :real-bits **real-bits**
                                           :integer-bits **integer-bits**))

(defun allocate-memory-for-model (model &key bypass-size-limit)
  "Allocate memory for a given MEMORY-MODEL MODEL.

Return a hash table mapping memory names to their MEMORY-VIEWs which contain allocated CLASSICAL-MEMORYs.

If BYPASS-SIZE-LIMIT is T (default: NIL), then the size limit dictated by **CLASSICAL-MEMORY-SIZE-LIMIT** will be ignored."
  (let ((memories (make-hash-table :test 'equal)))
    ;; First we allocate all of the roots.
    (loop :with size-left := **classical-memory-size-limit**
          :for root :in (quil:memory-model-roots model)
          :for name := (quil:memory-descriptor-name root)
          :for length := (quil:memory-descriptor-length root)
          :for size := (/
                        ;; This bit is measured in bits.
                        (round-up-to-next-multiple
                           (* length
                              (funcall (quil:memory-model-sizeof model)
                                       (quil:memory-descriptor-type root)))
                           (quil:memory-model-alignment model))
                        ;; So we divide to get number of octets. It
                        ;; should be an even division if our memory
                        ;; model is right.
                        8)
          :do
             (check-type size (integer 1) "The SIZE wasn't a multiple of 8.")
             ;; Record this allocation
             (decf size-left size)
             ;; Check we haven't overflowed.
             (when (and (not bypass-size-limit)
                        (minusp size-left))
               (error "Trying to allocate an additional ~D octets of classical ~
                       memory for the region ~S but the limit ~D has been exhausted."
                      size
                      name
                      **classical-memory-size-limit**))
             ;; Allocate the memory, and create readers and writers.
             (multiple-value-bind (reader writer)
                 (typed-reader/writer (quil:memory-descriptor-type root))
               (let ((name name)        ; Re-bind these for closure
                     (length length))   ; capture below.
                 (setf (gethash name memories)
                       (memory-view (make-classical-memory size)
                                    :length length
                                    :reader  (lambda (mem i)
                                               (assert-in-bounds i 0 length name)
                                               (funcall reader mem i))
                                    :writer  (lambda (new-value mem i)
                                               (assert-in-bounds i 0 length name)
                                               (funcall writer new-value mem i)))))))
    ;; All of the roots are allocated, and thus we have no additional
    ;; memory to allocate. We do need to make views onto the aliased
    ;; data though, which means the construction of a bunch of
    ;; closures and MEMORY-VIEWs.
    (dolist (alias (quil:memory-model-aliases model) memories)
      (let* ((name (quil:memory-alias-name alias))
             (type (quil:memory-alias-type alias))
             (root (quil:memory-alias-root-memory alias))
             (root-classical-memory
               (memory-view-root-classical-memory
                (gethash (quil:memory-descriptor-name root) memories)))
             (start (quil:memory-alias-starting-bit alias))
             (offset-octets (floor start 8))
             (addl-offset-bits (mod start 8)) ; This can *only* be non-zero for bits!
             (size  (quil:memory-alias-size-in-bits alias))
             (length (quil:memory-alias-length alias))
             ;; Computation of a new CLASSICAL-MEMORY which aliases
             ;; ROOT-CLASSICAL-MEMORY.
             (aliasing-memory (%make-classical-memory
                               (ceiling size 8) ; We can always safely
                                                ; round this up if
                                                ; everything
                                                ; previously
                                                ; bounds-checked fine.
                               (cffi:inc-pointer
                                (memory-pointer root-classical-memory)
                                offset-octets))))
        ;; Sanity check for non-aligned access. This should get caught
        ;; in CL-QUIL earlier.
        (adt:match quil:quil-type type
          (quil:quil-bit nil)
          (_ (assert (zerop addl-offset-bits) () "A non-BIT type starts at an ~
                                                  unaligned address. This ~
                                                  shouldn't happen!")))
        ;; Produce the readers and writers.
        (multiple-value-bind (base-reader base-writer)
            (typed-reader/writer (quil:memory-alias-type alias))
          ;; XXX FIXME: This is where we would deal with bit offsets
          ;; and the like.
          (flet ((reader (mem i)
                   (assert-in-bounds i 0 length name) ; Special sub-bounds check.
                   (funcall base-reader mem (+ i addl-offset-bits)))
                 (writer (new-value mem i)
                   (assert-in-bounds i 0 length name) ; Special sub-bounds check.
                   (funcall base-writer new-value mem (+ i addl-offset-bits))))
            (setf (gethash name memories)
                  (memory-view root-classical-memory
                               :aliasing-memory aliasing-memory
                               :length length
                               :reader #'reader
                               :writer #'writer))))))))


;;; Classical Memory Subsystem

(defclass classical-memory-subsystem ()
  ((classical-memory-model :initarg :classical-memory-model
                           :reader classical-memory-model
                           :type quil:memory-model
                           :documentation "A description of the structure of the memory of the abstract machine.")

   ;; This will get allocated automatically in INITIALIZE-INSTANCE.
   (classical-memories :reader classical-memories
                       ;; Map between name -> MEMORY-VIEWs
                       :type hash-table
                       :documentation "A table mapping names to allocated memories."))
  (:default-initargs :classical-memory-model quil:**empty-memory-model**)
  (:documentation "The standard Quil memory subsystem."))

(defmethod initialize-instance :after ((instance classical-memory-subsystem) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value instance 'classical-memories)
        (allocate-memory-for-model (classical-memory-model instance))))
