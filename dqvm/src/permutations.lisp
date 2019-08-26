;;;; src/permutations.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(deftype permutation () 'list)

(defun-inlinable permute (permutation address)
  (loop :for (i . j) :in permutation
        :do (rotatef (ldb (byte 1 i) address) (ldb (byte 1 j) address))
        :finally (return address)))

(defun-inlinable apply-qubit-permutation (qubit-permutation addresses &key from-end)
  "Apply QUBIT-PERMUTATION to each amplitude address in ADDRESSES. Returns the permuted ADDRESSES. The inverse permutation is applied if FROM-END is not NIL.

Permutations of qubits are represented as lists of pairs of qubit addresses (e.g., '((0 1) (1 2)) means \"swap qubits 0 and 1, then swap qubits 1 and 2\") with NIL representing the identity.

Composing qubit permutations amounts to concatenating qubit pair lists in the order in which they are applied (as opposed to the usual order in mathematical notation).

Example:

  DQVM2> (print-qubit-permutation '((0 . 1) (1 . 2)) 3)
   0 |000>    0 |000>
   1 |001>    4 |100>
   2 |010>    1 |001>
   3 |011>    5 |101>
   4 |100>    2 |010>
   5 |101>    6 |110>
   6 |110>    3 |011>
   7 |111>    7 |111>

  DQVM2> (apply-qubit-permutation '((0 . 1) (1 . 2)) #(1 3 5) 3)
  #(4 5 6)

  DQVM2> (apply-qubit-permutation '((0 . 1) (1 . 2)) 1 3)
  4"
  (let ((permutation (if from-end
                         (reverse qubit-permutation)
                         qubit-permutation)))
    (etypecase addresses
      (alexandria:non-negative-fixnum (permute permutation addresses))
      (array (loop :for i :from 0
                   :for address :across addresses
                   :do (setf (aref addresses i)
                             (permute permutation address))
                   :finally (return addresses))))))

(defun apply-inverse-qubit-permutation (qubit-permutation addresses)
  "Apply inverse of QUBIT-PERMUTATION to ADDRESSES."
  (apply-qubit-permutation qubit-permutation addresses :from-end t))

(defun print-qubit-permutation (qubit-permutation number-of-qubits
                                &optional (stream *standard-output*))
  "Print all transpositions in QUBIT-PERMUTATION involving up to NUMBER-OF-QUBITS."
  (dotimes (i1 (expt 2 number-of-qubits) (values))
    (let* ((i2 (apply-qubit-permutation qubit-permutation i1))
           (aux (format nil "~~4D~~T|~~~D,'0B>" number-of-qubits))
           (control-string (concatenate 'string aux "~T" aux "~%")))
      (format stream control-string i1 i1 i2 i2))))
