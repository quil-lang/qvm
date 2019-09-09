;;;; src/permutation.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

;;; A simple implementation of a permutation data structure.

;;; Note that (make-permutation) and NIL both represent the identity.

(deftype transposition ()
  '(or null (cons alexandria:non-negative-fixnum
             alexandria:non-negative-fixnum)))

(defclass permutation ()
  ((number-of-transpositions
    :initarg :number-of-transpositions
    :type alexandria:non-negative-integer
    :documentation "Number of transpositions defining the permutation.")
   (transpositions
    :initarg :transpositions
    :type list
    :reader permutation-transpositions
    :documentation "Bijective map determined by transpositions, stored as an association list sorted by CAR."))
  (:default-initargs
   :transpositions nil)
  (:documentation "Permutation acting on sets of qubit indices."))

(defmethod print-object ((permutation permutation) stream)
  (print-unreadable-object (permutation stream :type t :identity t)
    (let ((transpositions (permutation-transpositions permutation)))
      (format stream "~:[~:A~;~{~A~^ ~}~]" transpositions transpositions))))

(defun-inlinable make-permutation (&optional transpositions)
  "Allocate a permutation defined by TRANSPOSITIONS.

Transpositions are extended if necessary to fully characterize the permutation.

Example
-------

DQVM2> (make-permutation '((2 . 1) (1 . 0)))
#<permutation (0 . 2) (1 . 0) (2 . 1) {10086BB8B3}>

Note that in the example above, the transposition (0 2) was automatically added."
  (declare (optimize (speed 3) (safety 0))
           (type list transpositions))

  (let ((permutation (make-instance 'permutation))
        (transpositions* nil)
        (domain nil)
        (codomain nil))

    (flet ((check-transposition (a b)
             (declare (type alexandria:non-negative-fixnum a b))
             (let ((x (assoc a transpositions*))
                   (y (rassoc b transpositions*)))
               (alexandria:when-let ((z (or x y)))
                 (error "Malformed permutation. A mapping ~D ↦ ~D already existed."
                        (first z) (rest z))))))

      (loop :for (a . b) :in transpositions :do
        (check-transposition a b)
        (unless (= a b)
          (pushnew (cons a b) transpositions*)
          (pushnew a domain)
          (pushnew b codomain))))

    (loop :for a :of-type alexandria:non-negative-fixnum
            :in (set-difference codomain domain)
          :for b :of-type alexandria:non-negative-fixnum
            :in (nset-difference domain codomain)
          :unless (= a b) :do
            (pushnew (cons a b) transpositions* :test #'equal))

    (setf (slot-value permutation 'number-of-transpositions) (length transpositions*)
          (slot-value permutation 'transpositions) (sort transpositions* #'< :key #'first))

    permutation))

(defun-inlinable inverse-permutation (permutation)
  "Return the inverse of PERMUTATION."
  (declare (optimize (speed 3) (safety 0)))
  (when permutation

    (let ((inverse-permutation (make-instance 'permutation))
          (transpositions (permutation-transpositions permutation)))

      (setf (slot-value inverse-permutation 'transpositions) (loop :for (a . b) :in transpositions :collect (cons b a))
            (slot-value inverse-permutation 'number-of-transpositions) (slot-value permutation 'number-of-transpositions))

      inverse-permutation)))

(defun is-identity-permutation-p (permutation)
  "Return T if PERMUTATION is the identity, NIL otherwise."
  (if (or (null permutation) (null (permutation-transpositions permutation)))
      t
      nil))

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
  (declare (optimize (speed 3) (safety 0))
           (type (or null permutation) permutation)
           (type alexandria:non-negative-fixnum item))
  (the alexandria:non-negative-fixnum
       (if permutation
           (alexandria:if-let ((transposition (assoc item (permutation-transpositions permutation))))
             (rest transposition)
             item)
           item)))

(defun-inlinable apply-inverse-permutation (permutation item)
  "Apply PERMUTATION⁻¹ to ITEM."
  (apply-permutation (inverse-permutation permutation) item))

(defun compose-permutations (&rest permutations)
  "Return a new permutation that is the composition of PERMUTATIONS.

If PERMUTATIONS is the list π₁, π₂, ..., πₛ, then the result is the composition π₁ ∘ π₂ ∘ ... ∘ πₛ. In other words, the composition starts from right to left as in standard mathematical notation."
  (let (transpositions)

    (let (domain)
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
                       (pushnew (cons a b) transpositions :test #'equal))))

    (make-permutation transpositions)))

(defun-inlinable apply-qubit-permutation (permutation address)
  "Apply PERMUTATION to an index ADDRESS within a wavefunction.

Examples
--------

DQVM2> (apply-qubit-permutation (make-permutation '((2 . 0))) #b100)
1

DQVM2> (write (apply-qubit-permutation (make-permutation '((2 . 0))) #b001) :base 2)
100
4"
  ;; Alternatively, in-place permutations could be implemented following:
  ;;
  ;; F. Fich, J. Munro, and P. Poblete, “Permuting in Place,” SIAM
  ;; J. Comput., vol. 24, no. 2, pp. 266–278, Apr. 1995.

  (declare (optimize (speed 3) (safety 0))
           (type (or null permutation) permutation)
           (type qvm:amplitude-address address))

  (the qvm:amplitude-address
       (if permutation
           (let* ((transpositions (slot-value permutation 'transpositions))
                  (number-of-transpositions (slot-value permutation 'number-of-transpositions))
                  (bit-vector (make-array number-of-transpositions :element-type 'bit)))
             ;; (declare (dynamic-extent bit-vector))

             (loop :for index :from 0
                   :for transposition :in transpositions :do
                     (setf (bit bit-vector index) (ldb (byte 1 (first transposition))
                                                       address)))

             (loop :for index :from 0
                   :for transposition :of-type transposition :in transpositions :do
                     (setf address (dpb (bit bit-vector index)
                                        ;; (byte 1 (the (unsigned-byte 6) (rest transposition))) ; Enable this for speed (assumes a maximum of 64 qubits).
                                        (byte 1 (rest transposition))
                                        address))
                   :finally (return address)))
           address)))

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
