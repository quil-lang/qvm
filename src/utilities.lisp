;;;; src/utilities.lisp
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

(declaim (inline %make-nat-tuple
                 nat-tuple-cardinality))

(deftype nat-tuple ()
  "The NAT-TUPLE type. A \"nat tuple\" represents an ordered list of non-negative integer indexes."
  `(simple-array nat-tuple-element (*)))

(declaim (ftype (function (nat-tuple-cardinality) nat-tuple) %make-nat-tuple))
(defun %make-nat-tuple (n)
  (declare (type nat-tuple-cardinality n))
  (make-array n :element-type 'nat-tuple-element
                :initial-element 0))

(declaim (ftype (function (nat-tuple) nat-tuple-cardinality) nat-tuple-cardinality))
(defun nat-tuple-cardinality (nt)
  "Compute the number of elements in the nat tuple NT."
  (declare (type nat-tuple nt))
  (length nt))

(defun nat-tuple (&rest elements)
  "Create a new nat tuple with the elements ELEMENTS (each of type NAT-TUPLE-ELEMENT)."
  (declare (dynamic-extent elements))
  (let* ((n (length elements))
         (nt (%make-nat-tuple n)))
    (loop :for x :in elements
          :for i :from (1- n) :downto 0
          :do (setf (aref nt i) x))
    nt))

(defmacro do-nat-tuple ((elt nt) &body body)
  "Iterate over the elements of the nat tuple NT in increasing order. The return value is unspecified.

ELT will be bound to the element itself."
  (check-type elt symbol)
  (let ((g-nt (gensym "NT-")))
    `(let ((,g-nt ,nt))
       (declare (type nat-tuple ,g-nt))
       (loop :for ,elt :of-type nat-tuple-element :across ,g-nt
             :do (progn
                   ,@body)))))

(defun nat-tuple-complement (n nt)
  "Compute the complement of the nat tuple NT in a universe of (0 1 2 ... N-1)."
  (let* ((nt-len (nat-tuple-cardinality nt))
         (sorted-nt (sort (copy-seq nt) #'<)))
    (assert (< nt-len n))
    (let* ((compl-len (- n nt-len))
           (compl (%make-nat-tuple compl-len))
           (nt-ptr 0)
           (compl-ptr 0))
      (declare (type nat-tuple-element nt-ptr compl-ptr)) ; indexes
      (dotimes (i n compl)
        (declare (type nat-tuple-element i))
        (cond
          ((= compl-ptr compl-len)
           (return-from nat-tuple-complement compl))
          ((and (< nt-ptr nt-len)
                (= i (aref sorted-nt nt-ptr)))
           (incf nt-ptr))
          (t
           (setf (aref compl compl-ptr) i)
           (incf compl-ptr)))))))

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

(declaim (inline half))
(defun half (x)
  "Compute the floor of half of the real number X."
  (floor x 2))

(declaim (inline boolean-bit))
(defun boolean-bit (boolean)
  "Convert a generalized boolean BOOLEAN into a good ol' BIT."
  (if boolean 1 0))


(defmacro probabilistically (p &body body)
  "Execute BODY with probability 0 <= P <= 1."
  `(when (< (random 1.0) ,p)
     ,@body))

(defmacro defun-inlinable (name lambda-list &body body)
  "Define an INLINE-able function."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)
     (declaim (notinline ,name))))

(declaim (ftype (function (non-negative-fixnum non-negative-fixnum) list) subdivide))
(defun subdivide (total num-ranges)
  "Subdivide TOTAL (probably representing a vector length) into NUM-RANGES as-equal-as-possible ranges.

The result will be a list of cons cells representing half-open intervals (on the right) whose union is [0, total)."
  (let ((size (floor total num-ranges)))
    (if (zerop size)
        (list (cons 0 total))
        (loop :for worker :from 1 :to num-ranges
              :for start :from 0 :by size
              :if (= worker num-ranges)
                :collect (cons start total)
              :else
                :collect (cons start (+ start size))))))

(defmacro measuring-gc ((time-var bytes-var) &body body)
  "Execute BODY setting TIME-VAR to the number of milliseconds spent garbage collecting, and BYTES-VAR to roughly the number of bytes allocated."
  #-sbcl
  `(multiple-value-prog1 (progn ,@body)
     (setf ,bytes-var -1)
     (setf ,time-var -1))
  #+sbcl
  (alexandria:with-gensyms (bytes-start)
    `(let ((sb-ext:*gc-run-time* 0)
           (,bytes-start (sb-ext:get-bytes-consed)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,bytes-var (- (sb-ext:get-bytes-consed) ,bytes-start))
         (setf ,time-var (round (* (/ 1000 internal-time-units-per-second)
                                   sb-ext:*gc-run-time*)))))))

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

;;; Bit Injection/Ejection

(declaim (inline inject-bit))
(defun inject-bit (x n)
  "Inject a 0 at the nth position. Example:

    (INJECT-BIT #b1111 1) => #b11101
"
  (declare #.*optimize-dangerously-fast*
           (type non-negative-fixnum x)
           (type nat-tuple-element n))
  (let ((left (ash (dpb 0 (byte n 0) x) 1))
        (right (ldb (byte n 0) x)))
    (declare (type non-negative-fixnum left right))
    (the non-negative-fixnum (logior left right))))

(defun inject-bit-code (n)
  (check-type n nat-tuple-element)
  `(lambda (x)
     (declare #.*optimize-dangerously-fast*
              (type non-negative-fixnum x))
     (let ((left (ash (dpb 0 (byte ,n 0) x) 1))
           (right (ldb (byte ,n 0) x)))
       (declare (type non-negative-fixnum left right))
       (the non-negative-fixnum (logior left right)))))

(declaim (inline eject-bit))
(defun eject-bit (x n)
  "Remove the nth bit. Example:

    (EJECT-BIT #b1101 1) => #b111
"
  (declare #.*optimize-dangerously-fast*
           (type non-negative-fixnum x)
           (type nat-tuple-element n))
  (let ((left (dpb 0 (byte n 0) (ash x -1)))
        (right (ldb (byte n 0) x)))
    (declare (type non-negative-fixnum left right))
    (the non-negative-fixnum (logior left right))))

(defun eject-bit-code (n)
  (check-type n nat-tuple-element)
  `(lambda (x)
     (declare #.*optimize-dangerously-fast*
              (type non-negative-fixnum x))
     (let ((left (dpb 0 (byte ,n 0) (ash x -1)))
           (right (ldb (byte ,n 0) x)))
       (declare (type non-negative-fixnum left right))
       (the non-negative-fixnum (logior left right)))))

(defun seeded-random-state (seed)
  "Return an MT19937 random state that has been initialized from SEED,
which should be either NIL (meaning use a fresh and irreproducible
random state), or an integer or a specialized vector of (unsigned-byte
32), which will result in a reproducible random state."
  (check-type seed
              (or null
                  integer
                  (array (unsigned-byte 32) (*))))
  ;; MAKE-RANDOM-OBJECT is not exported, but it the recommended
  ;; function to use for seeding in the MT19937 sources.
  (etypecase seed
    (null
     (mt19937:make-random-state t))
    (array
     (mt19937::make-random-object :state (mt19937:init-random-state seed)))
    (integer
     ;; Integer seeds must be non-zero for MT19937
     (when (zerop seed)
       (setf seed 1))
     (mt19937::make-random-object :state (mt19937:init-random-state seed)))))

(defmacro with-random-state ((state) &body body)
  `(let ((mt19937:*random-state* ,state))
     ,@body))

(defun round-up-to-next-multiple (x multiple)
  "Round X up to the next multiple of MULTIPLE."
  (check-type x (integer 0))
  (check-type multiple (integer 1))
  (* multiple (ceiling x multiple)))


;;; Macros for parallel processing

(defmacro pdotimes ((i n &optional ret) &body body)
  "Selectively perform DOTIMES or LPARALLEL:DOTIMES, depending on
whether the number of iterations N exceeds the threshold set by
*QUBITS-REQUIRED-FOR-PARALLELIZATION*."
  (alexandria:once-only (n)      
    `(if (> ,n (expt 2  *qubits-required-for-parallelization*))
         (lparallel:pdotimes (,i ,n ,ret)
           ,@body)
         (dotimes (,i ,n ,ret)
           ,@body))))


(defmacro psum-dotimes ((i range) &body body)
  "Compute the sum of BODY for I in ranging over 0 <= I < RANGE. RANGE must be a non-negative fixnum."
  (alexandria:with-gensyms (sum partial-sum start end ch num-tasks worker-function range-once ranges)
    `(let ((,range-once ,range))
       (declare (type non-negative-fixnum ,range-once))
       (if (< ,range-once (expt 2 *qubits-required-for-parallelization*))
           (locally (declare #.*optimize-dangerously-fast*)
             (loop :with ,sum :of-type flonum := (flonum 0)
                   :for ,i :of-type non-negative-fixnum :below ,range-once
                   :do (incf ,sum (the flonum (progn ,@body)))
                   :finally (return ,sum)))
           (flet ((,worker-function (,start ,end)
                    (declare (type non-negative-fixnum ,start)
                             (type non-negative-fixnum ,end))
                    (locally (declare #.*optimize-dangerously-fast*)
                      (loop :with ,partial-sum :of-type flonum := (flonum 0)
                            :for ,i :of-type non-negative-fixnum :from ,start :below ,end
                            :do (incf ,partial-sum (the flonum (progn ,@body)))
                            :finally (return ,partial-sum)))))
             (declare (dynamic-extent #',worker-function)
                      (ftype (function (non-negative-fixnum non-negative-fixnum) flonum) ,worker-function))
             (let* ((,ch (lparallel:make-channel))
                    (,num-tasks (lparallel:kernel-worker-count))
                    (,ranges (subdivide ,range-once ,num-tasks)))
               (declare (type lparallel:channel ,ch)
                        (type non-negative-fixnum ,num-tasks)
                        (type list ,ranges))
               (loop :for (,start . ,end) :in ,ranges
                     :do (lparallel:submit-task ,ch #',worker-function ,start ,end))
               (loop :repeat (length ,ranges)
                     :sum (the flonum (lparallel:receive-result ,ch)) :of-type flonum)))))))
