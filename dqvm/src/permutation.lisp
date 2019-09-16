;;;; src/permutation.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

;;; Permutation classes for permuting sets of qubits.
;;;
;;; The value NIL represents the identity permutation. General permutations
;;; are embodied by the PERMUTATION-GENERAL class. Permutations involving a
;;; single transposition swapping 0 with another qubit are represented by the
;;; PERMUTATION-TRANSPOSITION class.
;;;
;;; The generic function APPLY-QUBIT-PERMUTATION does the heavy lifting of
;;; permuting addresses and the class hierarchy laid out here allows us to
;;; accomplish significant speed-ups (applying a PERMUTATION-TRANSPOSITION is
;;; more than three times faster than the equivalent application of a
;;; PERMUTATION-GENERAL object).

(defconstant +qubit-index-length+ (ceiling (log qvm::+max-nat-tuple-cardinality+ 2))
  "Number of bits required to represent a qubit index.")

(defconstant +max-number-of-transpositions+ (* 2 #.qvm::+max-nat-tuple-cardinality+)
  "Upper bound on the number of transpositions defining an arbitrary permutation.")

(deftype qubit-index ()
  '(unsigned-byte #.+qubit-index-length+))

(deftype transposition ()
  '(or null (cons qubit-index qubit-index)))

(defclass permutation ()
  ((size
    :type (integer 0 +qubit-index-length+)
    :reader permutation-size
    :initarg :size
    :documentation "Maximum number of bits on which the permutation acts."))
  (:default-initargs
   :size 0)
  (:documentation "Permutation of qubits."))

(defclass permutation-general (permutation)
  ((number-of-transpositions
    :type alexandria:non-negative-integer
    :initarg :number-of-transpositions
    :documentation "Number of transpositions defining the permutation.")
   (transpositions
    :type list
    :initarg :transpositions
    :reader permutation-transpositions
    :documentation "Bijective map determined by transpositions, stored as an association list sorted by CAR."))
  (:default-initargs
   :number-of-transpositions 0
   :transpositions nil)
  (:documentation "Arbitrary permutation acting on sets of qubit indices."))

(defclass permutation-transposition (permutation)
  ((tau
    :type qubit-index
    :initarg :tau
    :initform (error-missing-initform :tau)
    :documentation "Positive value of τ in π = (0 τ)."))
  (:documentation "Specialized permutation involving a single transposition of the form π = (0 τ) where τ ≠ 0."))

(defmethod permutation-transpositions ((permutation permutation-transposition))
  (let ((tau (slot-value permutation 'tau)))
    (list (cons 0 tau) (cons tau 0))))

(defmethod print-object ((permutation permutation-general) stream)
  (print-unreadable-object (permutation stream :type t :identity t)
    (let ((transpositions (permutation-transpositions permutation)))
      (format stream "~:[~:A~;~{~A~^ ~}~]" transpositions transpositions))))

(defmethod print-object ((permutation permutation-transposition) stream)
  (print-unreadable-object (permutation stream :type t :identity t)
    (let ((tau (slot-value permutation 'tau)))
      (format stream "(0 . ~D) (~D . 0)" tau tau))))

(defun-inlinable make-permutation (&optional transpositions)
  "Allocate a permutation defined by TRANSPOSITIONS.

Transpositions are extended if necessary to fully characterize the permutation.

Example
-------

DQVM2> (make-permutation '((2 . 1) (1 . 0)))
#<permutation (0 . 2) (1 . 0) (2 . 1) {10086BB8B3}>

Note that in the example above, the transposition (0 2) was automatically added."
  (declare #.qvm::*optimize-dangerously-fast*
           (type list transpositions))

  (let ((transpositions* nil)
        (domain nil)
        (codomain nil)
        (max-index 0))

    (declare (type qubit-index max-index))

    (flet ((check-transposition (a b)
             (declare (type qubit-index a b))
             (let ((x (assoc a transpositions*))
                   (y (rassoc b transpositions*)))
               (alexandria:when-let ((z (or x y)))
                 (error "Malformed permutation. A mapping ~D ↦ ~D already existed."
                        (first z) (rest z))))))

      (declare (inline check-transposition))

      (loop :for (a . b) :of-type qubit-index :in transpositions
            :maximize (max a b) :into max :do
              (check-transposition a b)
              (unless (= a b)
                (pushnew (cons a b) transpositions*)
                (pushnew a domain)
                (pushnew b codomain))
            :finally (setf max-index max)))

    (loop :for a :of-type qubit-index :in (set-difference codomain domain)
          :for b :of-type qubit-index :in (set-difference domain codomain)
          :unless (= a b) :do
            (pushnew (cons a b) transpositions* :test #'equal))

    (cond
      ((and (null domain) (null codomain)) nil)
      ((and (= 1 (length domain))
            (zerop (min (the qvm:amplitude-address (first domain))
                        (the qvm:amplitude-address (first codomain)))))
       (make-instance 'permutation-transposition :tau max-index :size (1+ max-index)))
      ((and (= 2 (length domain))
            (null (set-difference domain codomain))
            (zerop (the qvm:amplitude-address (alexandria:extremum domain #'<))))
       (make-instance 'permutation-transposition :tau max-index :size (1+ max-index)))
      (t
       (make-instance 'permutation-general :number-of-transpositions (length transpositions*)
                                           :transpositions (sort transpositions* #'< :key #'first)
                                           :size (1+ max-index))))))

(defgeneric inverse-permutation (permutation)
  (:documentation "Return the inverse of PERMUTATION.")
  (declare #.qvm::*optimize-dangerously-fast*))

(defmethod inverse-permutation ((permutation null))
  nil)

(defmethod inverse-permutation ((permutation permutation-transposition))
  permutation)

(defmethod inverse-permutation ((permutation permutation-general))
  (make-instance 'permutation-general
                 :transpositions (loop :for (a . b) :in (permutation-transpositions permutation) :collect (cons b a))
                 :number-of-transpositions (slot-value permutation 'number-of-transpositions)))

(defgeneric identity-permutation-p (permutation)
  (:documentation "Return T if PERMUTATION is the identity, NIL otherwise."))

(defmethod identity-permutation-p ((permutation null))
  t)

(defmethod identity-permutation-p ((permutation permutation-transposition))
  nil) ; By construction PERMUTATION-TRANSPOSITION objects cannot be the identity.

(defmethod identity-permutation-p ((permutation permutation-general))
  (null (permutation-transpositions permutation)))

(defun compose-permutations (&rest permutations)
  "Return a new permutation that is the composition of PERMUTATIONS.

If PERMUTATIONS is the list π₁, π₂, ..., πₛ, then the result is the composition π₁ ∘ π₂ ∘ ... ∘ πₛ. In other words, the composition starts from right to left as in standard mathematical notation."
  (let ((transpositions nil)
        (domain nil))

    ;; Aggregate the domain of the composed permutation to get a list of
    ;; all possible relevant inputs.
    (loop :for permutation :in permutations :when permutation :do
      (loop :for transposition :of-type transposition :in (permutation-transpositions permutation) :do
        (let ((a (first transposition)))
          (declare (type qubit-index a))
          (pushnew a domain))))

    ;; Now map each domain element to obtain transpositions.
    (loop :with codomain := (coerce domain 'vector)
          :for permutation :in (nreverse permutations) :when permutation :do
            (loop :for i :from 0 :for b :across codomain :do
              (setf (aref codomain i)
                    (apply-permutation permutation (aref codomain i))))
          :finally
             (loop :for a :of-type qubit-index :in domain
                   :for b :of-type qubit-index :across codomain
                   :unless (= a b) :do
                     (pushnew (cons a b) transpositions :test #'equal)))

    (make-permutation transpositions)))

(defgeneric apply-qubit-permutation (permutation address)
  (:documentation
   "Apply PERMUTATION to an index ADDRESS within a wavefunction.

Examples
--------

DQVM2> (apply-qubit-permutation (make-permutation '((2 . 0))) #b100)
1

DQVM2> (write (apply-qubit-permutation (make-permutation '((2 . 0))) #b001) :base 2)
100
4")
  (declare #.qvm::*optimize-dangerously-fast*))

(defmethod apply-qubit-permutation ((permutation null) address)
  address)

(defmethod apply-qubit-permutation ((permutation permutation-transposition) address)
  (declare #.qvm::*optimize-dangerously-fast*
           (type permutation-transposition)
           (type qvm:amplitude-address address)
           (values qvm:amplitude-address))

  (let ((tau (slot-value permutation 'tau)))
    (declare (type qubit-index tau))

    ;; Swap bits 0 and TAU in ADDRESS.
    (let ((x (logxor (logand address 1) (logand (ash address (- tau)) 1))))
      (logxor address (logior x (ash x tau))))))

(defmethod apply-qubit-permutation ((permutation permutation-general) address)
  ;; Alternatively, in-place permutations could be implemented following:
  ;;
  ;; F. Fich, J. Munro, and P. Poblete, “Permuting in Place,” SIAM
  ;; J. Comput., vol. 24, no. 2, pp. 266–278, Apr. 1995.

  (declare #.qvm::*optimize-dangerously-fast*
           (type permutation-general permutation)
           (type qvm:amplitude-address address)
           (values qvm:amplitude-address))

  (let* ((transpositions (slot-value permutation 'transpositions))
         (number-of-transpositions (slot-value permutation 'number-of-transpositions))
         (bit-vector (make-array number-of-transpositions :element-type 'bit)))
    (declare (type (integer 0 #.+max-number-of-transpositions+) number-of-transpositions)
             (dynamic-extent bit-vector))

    (loop :for index :from 0
          :for transposition :of-type transposition :in transpositions :do
            (setf (sbit bit-vector index) (ldb (byte 1 (first transposition))
                                               address)))

    (loop :for index :from 0
          :for transposition :of-type transposition :in transpositions :do
            (setf address (the qvm:amplitude-address
                               (dpb (sbit bit-vector index)
                                    (byte 1 (the qubit-index (rest transposition)))
                                    address)))
          :finally (return address))))

(defgeneric generate-qubit-permutation-code (permutation)
  (:documentation "Generate lambda function equivalent to APPLY-QUBIT-PERMUTATION suitable to be compiled.")
  (declare #.qvm::*optimize-dangerously-fast*))

(defmethod generate-qubit-permutation-code ((permutation null))
  (let ((address (gensym "ADDRESS-")))
    `(lambda (,address)
       (declare #.qvm::*optimize-dangerously-fast*
                (type qvm:amplitude-address ,address)
                (values qvm:amplitude-address))
       ,address)))

(defmethod generate-qubit-permutation-code ((permutation permutation-transposition))
  (let* ((address (gensym "ADDRESS-"))
         (tau (slot-value permutation 'tau))
         (minus-tau (- tau)))
    `(lambda (,address)
       (declare #.qvm::*optimize-dangerously-fast*
                (type qvm:amplitude-address ,address)
                (values qvm:amplitude-address))

       ;; Swap bits 0 and TAU in ADDRESS.
       (let ((x (logxor (logand ,address 1) (logand (ash ,address ,minus-tau) 1))))
         (logxor ,address (logior x (ash x ,tau)))))))

(defmethod generate-qubit-permutation-code ((permutation permutation-general))
  (let ((address (gensym "ADDRESS-"))
        (transpositions (slot-value permutation 'transpositions))
        (number-of-transpositions (slot-value permutation 'number-of-transpositions)))
    `(lambda (,address)
       (declare #.qvm::*optimize-dangerously-fast*
                (type qvm:amplitude-address ,address)
                (values qvm:amplitude-address))

       (let ((bit-vector (make-array ,number-of-transpositions :element-type 'bit)))
         (declare (dynamic-extent bit-vector))

         ,@(loop :for index :from 0
                 :for transposition :of-type transposition :in transpositions
                 :collect `(setf (sbit bit-vector ,index) (ldb (byte 1 ,(first transposition))
                                                               ,address)))

         ,@(loop :for index :from 0
                 :for transposition :of-type transposition :in transpositions
                 :collect `(setf ,address (the qvm:amplitude-address
                                               (dpb (sbit bit-vector ,index)
                                                    (byte 1 (the qubit-index ,(rest transposition)))
                                                    ,address))))
         ,address))))

(defmethod generate-qubit-permutation-code-with-look-up-table ((permutation null))
  (generate-qubit-permutation-code permutation))

(defmethod generate-qubit-permutation-code-with-look-up-table ((permutation permutation))
  (let* ((num-bits (slot-value permutation 'size))
         (num-entries (expt 2 num-bits))
         (table (make-array num-entries :element-type `(unsigned-byte ,+qubit-index-length+)
                                        :initial-contents (loop :for i :below num-entries :collect (apply-qubit-permutation permutation i)))))
    (let ((address (gensym "ADDRESS-")))
      `(lambda (,address)
         (declare #.qvm::*optimize-dangerously-fast*
                  (type qvm:amplitude-address ,address)
                  (values qvm:amplitude-address))
         (dpb (aref ,table (ldb (byte ,num-bits 0) ,address))
              (byte ,num-bits 0)
              ,address)))))

(defun compile-qubit-permutation (permutation)
  "Compile PERMUTATION and return a compiled function equivalent to (LAMBDA (ADDRESS) (APPLY-QUBIT-PERMUTATION PERMUTATION ADDRESS))."
  (declare #.qvm::*optimize-dangerously-fast*
           (type (or null permutation) permutation))
  ;; Try the fastest method first. Namely, compilation with a look-up
  ;; table. If that fails, fall back to single transposition or loop
  ;; unrolling.
  (let* ((size (if permutation
                   (slot-value permutation 'size)
                   0))
         (function (if (<= size +qubit-index-length+)
                       (generate-qubit-permutation-code-with-look-up-table permutation)
                       (generate-qubit-permutation-code permutation))))
    (declare (type qubit-index size))
    (qvm::compile-lambda function)))

(defun-inlinable apply-inverse-qubit-permutation (permutation address)
  (apply-qubit-permutation (inverse-permutation permutation) address))

(defun print-qubit-permutation (permutation &optional number-of-qubits
                                              (stream *standard-output*))
  "Print the address permutation induced by PERMUTATION (possibly using up to NUMBER-OF-QUBITS) in STREAM."
  (let* ((n (or number-of-qubits (permutation-size permutation)))
         (max-value (expt 2 n))
         (aux-control-string (format nil "~~~DD |~~~D,'0B>"
                                     (ceiling (log max-value 10)) n))
         (control-string (concatenate 'string aux-control-string "  " aux-control-string "~%")))

    (dotimes (i1 max-value (values))
      (let ((i2 (apply-qubit-permutation permutation i1)))
        (format stream control-string i1 i1 i2 i2)))))

(defun-inlinable apply-permutation (permutation item)
  "Apply PERMUTATION to ITEM.

Examples
--------

DQVM2> (apply-permutation (make-permutation) 42)
42

DQVM2> (apply-permutation (make-permutation '((2 . 0))) 2)
0

DQVM2> (apply-permutation (make-permutation '((2 . 1) (1 . 0))) 2)
1"
  (declare #.qvm::*optimize-dangerously-fast*
           (type (or null permutation) permutation)
           (type alexandria:non-negative-fixnum item)
           (values alexandria:non-negative-fixnum))

  (if permutation
      (alexandria:if-let ((transposition (assoc item (permutation-transpositions permutation))))
        (rest transposition)
        item)
      item))

(defun-inlinable apply-inverse-permutation (permutation item)
  "Apply PERMUTATION⁻¹ to ITEM."
  (apply-permutation (inverse-permutation permutation) item))
