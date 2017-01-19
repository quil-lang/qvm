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
    `(loop :with ,g-nt :of-type list := (nat-tuple-list ,nt)
           :for ,i :of-type non-negative-fixnum :from 0
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

(defun copy-hash-table (hash-table)
  "Copy the hash table HASH-TABLE.

NOTE: This will not copy any multiprocessing aspects."
  (check-type hash-table hash-table)
  (let ((ht (make-hash-table
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop :for key :being :each :hash-key :of hash-table
            :using (hash-value value)
          :do (setf (gethash key ht) value)
          :finally (return ht))))

(defmacro probabilistically (p &body body)
  "Execute BODY with probability 0 <= P <= 1."
  `(when (< (random 1.0) ,p)
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
                  (<= workers-allotted num-logical-cores))
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
