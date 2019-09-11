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

(deftype transposition ()
  '(or null (cons alexandria:non-negative-fixnum
             alexandria:non-negative-fixnum)))

(defclass permutation ()
  ()
  (:documentation "Base class for permutations."))

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
   :transpositions nil)
  (:documentation "Arbitrary permutation acting on sets of qubit indices."))

(defclass permutation-transposition ()
  ((tau
    :type (unsigned-byte 6)            ; Implies a maximum of 2⁶ = 64 qubits.
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
        (codomain nil))

    (flet ((check-transposition (a b)
             (declare (type alexandria:non-negative-fixnum a b))
             (let ((x (assoc a transpositions*))
                   (y (rassoc b transpositions*)))
               (alexandria:when-let ((z (or x y)))
                 (error "Malformed permutation. A mapping ~D ↦ ~D already existed."
                        (first z) (rest z))))))

      (declare (inline check-transposition))

      (loop :for (a . b) :in transpositions :do
        (check-transposition a b)
        (unless (= a b)
          (pushnew (cons a b) transpositions*)
          (pushnew a domain)
          (pushnew b codomain))))

    (loop :for a :of-type alexandria:non-negative-fixnum
            :in (set-difference codomain domain)
          :for b :of-type alexandria:non-negative-fixnum
            :in (set-difference domain codomain)
          :unless (= a b) :do
            (pushnew (cons a b) transpositions* :test #'equal))

    (cond
      ((and (null domain) (null codomain)) nil)
      ((and (= 1 (length domain))
            (zerop (min (the qvm:amplitude-address (first domain))
                        (the qvm:amplitude-address (first codomain)))))
       (make-instance 'permutation-transposition
                      :tau (max (the qvm:amplitude-address (first domain))
                                (the qvm:amplitude-address (first codomain)))))
      ((and (= 2 (length domain))
            (null (set-difference domain codomain))
            (zerop (the qvm:amplitude-address (apply #'min domain))))
       (make-instance 'permutation-transposition :tau (apply #'max domain)))
      (t
       (make-instance 'permutation-general :number-of-transpositions (length transpositions*)
                                           :transpositions (sort transpositions* #'< :key #'first))))))

(defgeneric inverse-permutation (permutation)
  (:documentation "Return the inverse of PERMUTATION.")
  (declare #.qvm::*optimize-dangerously-fast*))

(defmethod inverse-permutation ((permutation (eql nil)))
  nil)

(defmethod inverse-permutation ((permutation permutation-transposition))
  permutation)

(defmethod inverse-permutation ((permutation permutation-general))
  (make-instance 'permutation-general
                 :transpositions (loop :for (a . b) :in (permutation-transpositions permutation) :collect (cons b a))
                 :number-of-transpositions (slot-value permutation 'number-of-transpositions)))

(defgeneric is-identity-permutation-p (permutation)
  (:documentation "Return T if PERMUTATION is the identity, NIL otherwise."))

(defmethod is-identity-permutation-p ((permutation (eql nil)))
  t)

(defmethod is-identity-permutation-p ((permutation permutation-transposition))
  nil) ; By construction PERMUTATION-TRANSPOSITION objects cannot be the identity.

(defmethod is-identity-permutation-p ((permutation permutation-general))
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
          (declare (type alexandria:non-negative-fixnum a))
          (pushnew a domain))))

    ;; Now map each domain element to obtain transpositions.
    (loop :with codomain := (coerce domain 'vector)
          :for permutation :in (nreverse permutations) :when permutation :do
            (loop :for i :from 0 :for b :across codomain :do
              (setf (aref codomain i)
                    (apply-permutation permutation (aref codomain i))))
          :finally
             (loop :for a :of-type alexandria:non-negative-fixnum :in domain
                   :for b :of-type alexandria:non-negative-fixnum :across codomain
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

(defmethod apply-qubit-permutation ((permutation (eql nil)) address)
  address)

(defmethod apply-qubit-permutation ((permutation permutation-transposition) address)
  (declare #.qvm::*optimize-dangerously-fast*
           (type (or null permutation) permutation)
           ;; (type qvm:amplitude-address address)
           (type (unsigned-byte 64) address) ; Imposed maximum number of qubits.
           (values qvm:amplitude-address))

  (let ((tau (slot-value permutation 'tau)))
    (declare (type (unsigned-byte 6) tau))

    (rotatef (ldb (byte 1 0) address) (ldb (byte 1 tau) address))
    address))

(defmethod apply-qubit-permutation ((permutation permutation) address)
  ;; Alternatively, in-place permutations could be implemented following:
  ;;
  ;; F. Fich, J. Munro, and P. Poblete, “Permuting in Place,” SIAM
  ;; J. Comput., vol. 24, no. 2, pp. 266–278, Apr. 1995.

  (declare #.qvm::*optimize-dangerously-fast*
           (type (or null permutation) permutation)
           ;; (type qvm:amplitude-address address)
           (type (unsigned-byte 64) address) ; Imposed maximum number of qubits.
           (values qvm:amplitude-address))

  (let* ((transpositions (slot-value permutation 'transpositions))
         (number-of-transpositions (slot-value permutation 'number-of-transpositions))
         (bit-vector (make-array number-of-transpositions :element-type 'bit)))
    (declare (type (integer 0 128) number-of-transpositions)
             (dynamic-extent bit-vector))

    (loop :for index :from 0
          :for transposition :in transpositions :do
            (setf (bit bit-vector index) (ldb (byte 1 (first transposition))
                                              address)))

    (loop :for index :from 0
          :for transposition :of-type transposition :in transpositions :do
            (setf address (the qvm:amplitude-address
                               (dpb (bit bit-vector index)
                                    (byte 1 (the (unsigned-byte 6) (rest transposition))) ; Enable this for speed (assumes a maximum of 64 qubits).
                                    ;; (byte 1 (rest transposition))
                                    address)))
          :finally (return address))))

(defun-inlinable apply-inverse-qubit-permutation (permutation address)
  (apply-qubit-permutation (inverse-permutation permutation) address))

(defun print-qubit-permutation (permutation &optional number-of-qubits
                                              (stream *standard-output*))
  "Print the address permutation induced by PERMUTATION (possibly using up to NUMBER-OF-QUBITS) in STREAM."
  (let* ((n (or number-of-qubits
                (1+ (loop :for (a . b) :in (permutation-transpositions permutation)
                          :maximizing (max a b)))))
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
