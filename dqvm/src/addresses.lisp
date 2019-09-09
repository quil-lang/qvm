;;;; src/addresses.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

;;; This file implements an address table for the amplitudes in the wave
;;; function. The table does not explicitly contain the addresses, instead it
;;; computes them on the fly by keeping track of the current qubit
;;; permutation. The rationale for this approach is so that every MPI rank
;;; can determine which addresses are held by any other MPI rank with a
;;; minimal memory footprint.
;;;
;;; Motivation:
;;;
;;;     Let I and X be the Pauli gates with matrix representations:
;;;
;;;             [ 1  0 ]              [ 0  1 ]
;;;         I = [      ],   and   X = [      ]
;;;             [ 0  1 ]              [ 1  0 ]
;;;
;;;     Now consider a quantum computer with two qubits indexed by 0 and
;;;     1. The corresponding wavefunction is:
;;;
;;;        ψ = [ a₀ a₁ a₂ a₃ ]ᵀ = a₀ |00> + a₁ |01> + a₂ |10> + a₃ |11>
;;;
;;;     Suppose we want to execute the instruction X on qubit 0. This amounts
;;;     to evaluating ψ′ = (I ⊗ X) ψ, where ⊗ denotes the Kronecker product.
;;;     The matrix I ⊗ X is:
;;;
;;;                     [ 0  1  0  0 ]
;;;                     [            ]
;;;        [ X  0 ]     [ 1  0  0  0 ]
;;;        [      ]  =  [            ]
;;;        [ 0  X ]     [ 0  0  0  1 ]
;;;                     [            ]
;;;                     [ 0  0  1  0 ]
;;;
;;;     Notice that the matrix I ⊗ X is block-diagonal, and ψ′ becomes:
;;;
;;;             [ X ϕ₀ ]
;;;        ψ′ = [      ], where ϕ₀ = [ ψ₀ ψ₁ ]ᵀ and ϕ₁ = [ ψ₂ ψ₃ ]ᵀ.
;;;             [ X ϕ₁ ]
;;;
;;;     In other words, we split the wave function into blocks and then we
;;;     apply X to each block. This means that if we have two MPI ranks, the
;;;     first one can receive ϕ₀ and compute X ϕ₀ while the second receives
;;;     ϕ₁ and computes X ϕ₁. At this point, the resulting wavefunction ψ′ is
;;;     distributed among the two MPI ranks. Note that the block-diagonal
;;;     structure of I ⊗ X above allowed us to compute the result with
;;;     minimal communication among the ranks.
;;;
;;;     Now we would like to execute X on qubit 1 but the corresponding
;;;     Kronecker product does not have the same block-diagonal structure
;;;     that we exploited in the previous case. This implies that, in
;;;     general, we have to let the MPI ranks communicate more data during a
;;;     gate application (this would be perhaps more apparent had we
;;;     considered a Hadamard gate instead of X). However, we can compute a
;;;     transposition matrix P associated to the permutation π = (1 0) that
;;;     maps qubit 1 to qubit 0 and write
;;;
;;;        ψ′ = (X ⊗ I) ψ = Pᵀ (I ⊗ X) P ψ.
;;;
;;;     The above can be interpreted as meaning that, after a permutation,
;;;     all gate matrices are block-diagonal. In this case, P coincides with
;;;     the representation of the SWAP operator and it is straight-forward to
;;;     see that the above identity holds. A worked out example is shown at
;;;     the end of this comment.
;;;
;;; To illustrate the mechanism used to manage addresses in local and remote
;;; ranks, we consider the following example program:
;;;
;;;     X 1
;;;     Z 2
;;;
;;; There are 3 qubits (0, 1, and 2) and 2^3 = 8 addresses and amplitudes
;;; involved in the above program. If we have 2 ranks available, then each
;;; one of them keeps 2³ / 2 = 4 address/amplitude pairs and the the data is
;;; split among them as shown below:
;;;
;;;       rank #0 | rank #1
;;;       --------+--------
;;;       0 1 2 3 | 4 5 6 7
;;;
;;; Suppose we have already executed "X 1" and we are about to execute "Z
;;; 2". Moreover, imagine we are sitting at rank #0, then we must answer the
;;; following questions:
;;;
;;;   1. Which address/amplitude pairs are going to be sent and where are
;;;   they going (i.e., which rank and which place within the receiving
;;;   rank)?
;;;
;;;   2. Which addresses and amplitudes is rank #0 going to be receiving, and
;;;   from which source?
;;;
;;; To answer the questions above, it is convenient to keep the following
;;; diagram in mind:
;;;
;;;       rank #0 | rank #1
;;;       --------+--------
;;;   π₀  0 1 2 3 | 4 5 6 7
;;;   π₁  0 2 1 3 | 4 6 5 7  <--- Addresses after executing "X 1"
;;;   π₂  0 4 2 6 | 1 5 3 7  <--- Addresses after executing "Z 2"
;;;
;;; where πₖ is the permutation associated with the k-th instruction and π₀
;;; is the identity.
;;;
;;; Remark:
;;;
;;;     Note that the permutations considered above are independent of the
;;;     gate being executed. It does not matter whether the gate is I, X, Y,
;;;     Z, CNOT, etc. What matters is that if U is an arbitrary instruction
;;;     of arity m operating on qubits (q₀, ..., q_{m-1}) then the
;;;     permutation π associated to it will be of the form
;;;         π = (q₀ 0) (q₁ 1) ... (q_{m-1} m-1).
;;;     See the section titled Motivation above for further details.
;;;
;;; To see where addresses 0, 2, 1, and 3 are migrating to, we compute
;;;
;;;   π₂⁻¹(0, 2, 1, 3) = (0, 2, 4, 6) = (0, 0, 1, 1) mod 2³/2
;;;
;;; which tells us that addresses/amplitudes 0 and 2 remain in rank #0 (just
;;; take the remainder of the division by 2³/2 to see it) and, similarly,
;;; that 1 and 3 are to be sent to rank #1. Note that we also know the order
;;; into rank #1's address space (addresses 1 and 3 appear at the 4th and 6th
;;; global address or, equivalently, at offsets 0 and 2 of rank #1's address
;;; space).
;;;
;;; Now it remains to find out the addresses that rank #0 will be
;;; receiving. This follows by applying
;;;
;;;   π₂ ∘ π₁⁻¹(0, 2, 1, 3) = (0, 4, 2, 6),
;;;
;;; and then
;;;
;;;   π₁⁻¹(0, 4, 2, 6) = (0, 4, 1, 5) = (0, 1, 0, 1) mod 2³/2
;;;
;;; tells us that addresses 4 and 6 will come from rank #1 (and in which
;;; order).
;;;
;;; Example:
;;;
;;;     We are going to compute X 1 step by step on the wavefunction:
;;;
;;;       ψ = a₀ |00> + a₁ |01> + a₂ |10> + a₃ |11>
;;;
;;;     The permutation π = (1 0) brings qubit 1 to qubit 0, and so
;;;
;;;       P ψ = a₀ |00> + a₂ |01> + a₁ |10> + a₃ |11>,
;;;
;;;     where P is the permutation matrix associated to π. Now,
;;;
;;;       (I ⊗ X) P ψ = a₂ |00> + a₀ |01> + a₃ |10> + a₁ |11>,
;;;
;;;     The advantage of the previous computation is that we were able to
;;;     apply the same 2×2 matrix X in parallel to the vectors:
;;;
;;;       [ a₀ a₂ ]ᵀ   and   [ a₁ a₃ ]ᵀ.
;;;
;;;     Finally, we undo the permutation π by multiplying the previous result
;;;     by Pᵀ to get:
;;;
;;;       ψ′ = a₂ |00> + a₃ |01> + a₀ |10> + a₁ |11>.

(defvar *print-addresses* nil
  "Print address numbers when printing objects of class ADDRESSES.")

(defclass addresses ()
  ((rank
    :reader rank
    :type alexandria:non-negative-fixnum
    :initarg :rank
    :documentation "MPI rank in charge of this instance.")
   (global-addresses
    :reader global-addresses
    :type global-addresses
    :initarg :global-addresses
    :initform (error-missing-initform :global-addresses)
    :documentation "Reference to the GLOBAL-ADDRESSES object associated with this table.")
   (number-of-addresses
    :reader number-of-addresses
    :type alexandria:non-negative-fixnum
    :documentation "Length of the subvector of the wavefunction indexed by this table."))

  (:default-initargs :rank (mpi-comm-rank))
  (:documentation "Table of addresses handled by a single rank of a distributed QVM. The addresses within the table are computed on demand, rather than being explicitly stored."))

(defmethod initialize-instance :after ((addresses addresses) &rest initargs)
  (declare (ignore initargs))

  (let ((rank (rank addresses)))
    (assert (not (minusp rank)))
    (assert (< rank (number-of-processes addresses)))
    (setf (slot-value addresses 'number-of-addresses) (%number-of-addresses addresses))))

(defun make-addresses (&rest args)
  "Convenience function to instantiate ADDRESS classes."
  (flet ((popf (place indicator &optional default)
           (let ((value (getf place indicator default)))
             (remf place indicator)
             (values value place))))

    (multiple-value-bind (rank args)
        (popf args :rank (mpi-comm-rank))

      (make-instance 'addresses :rank rank
                                :global-addresses (apply #'make-instance 'global-addresses args)))))

(defmethod make-addresses-like ((addresses addresses) &key (rank nil rank-p) (permutation nil permutation-p))
  "Create a copy of ADDRESSES with rank equal to RANK and, possibly permutation set to PERMUTATION."
  (let ((global-addresses (if permutation-p
                              (let ((global-addresses (copy-global-addresses (global-addresses addresses))))
                                (update-permutation permutation global-addresses)
                                global-addresses)
                              (global-addresses addresses))))

    (make-instance 'addresses :rank (if rank-p
                                        rank
                                        (rank addresses))
                              :global-addresses global-addresses)))

(defmethod number-of-qubits ((addresses addresses))
  (number-of-qubits (global-addresses addresses)))

(defmethod number-of-processes ((addresses addresses))
  (number-of-processes (global-addresses addresses)))

(defmethod block-size ((addresses addresses))
  (block-size (global-addresses addresses)))

(defmethod blocks-per-process ((addresses addresses))
  (blocks-per-process (global-addresses addresses)))

(defmethod remainder-blocks ((addresses addresses))
  (remainder-blocks (global-addresses addresses)))

(defmethod permutation ((addresses addresses))
  (permutation (global-addresses addresses)))

(defmethod update-permutation (next-permutation (addresses addresses))
  ;; XXX remove at some point.
  (update-permutation next-permutation (global-addresses addresses)))

(defmethod get-effective-permutation ((addresses addresses) next-permutation)
  "Get the next permutation needed to apply PERMUTATION on ADDRESSES.

Returns the effective permutation (i.e., π₂ ∘ π₁⁻¹) as well as the next permutation (i.e., π₂). These permutations are in a format ready to be passed to APPLY-QUBIT-PERMUTATION."
  (compose-permutations next-permutation (inverse-permutation (permutation addresses))))

(defmethod number-of-blocks ((addresses addresses))
  "Number of addresses handled by the table ADDRESSES."
  (+ (blocks-per-process addresses)
     (boolean-bit (< (rank addresses) (remainder-blocks addresses)))))

(defmethod %number-of-addresses ((addresses addresses))
  "Number of addresses handled by the table ADDRESSES."
  (* (block-size addresses) (number-of-blocks addresses)))

(defmethod address-member (address (addresses addresses))
  "Return T if ADDRESS is in the table ADDRESSES, NIL otherwise."
  (= (get-rank-by-address (global-addresses addresses) address)
     (rank addresses)))

(defmethod block-member (block-index (addresses addresses))
  "Return T if BLOCK-INDEX is in the table ADDRESSES, NIL otherwise."
  (= (get-rank-by-block (global-addresses addresses) block-index)
     (rank addresses)))

(defmethod offset ((addresses addresses) address)
  "Find the index (offset) of the amplitude corresponding to ADDRESS. Return NIL if address is not in ADDRESSES."
  ;; XXX blend address-memberp with this function to avoid redundant work.
  (when (address-member address addresses)
    (let* ((rank (rank addresses))

           (global-addresses (global-addresses addresses))
           (block-size (block-size global-addresses))
           (blocks-per-process (blocks-per-process global-addresses))
           (remainder-blocks (remainder-blocks global-addresses))
           (number-of-blocks (number-of-blocks global-addresses))

           (initial-address (get-initial-address (global-addresses addresses) address))
           (start-of-remainder-addresses (* block-size (- number-of-blocks remainder-blocks))))

      (if (< initial-address start-of-remainder-addresses)
          (- initial-address (* rank blocks-per-process block-size))
          (+ (* blocks-per-process block-size)
             (- initial-address start-of-remainder-addresses (* rank block-size)))))))

(defmethod get-address-by-offset ((addresses addresses) offset)
  "Return the address located at OFFSET within ADDRESSES or NIL if the offset exceeds the number of addresses held."
  (check-type offset (unsigned-byte 64))

  (let* ((rank (rank addresses))
         (global-addresses (global-addresses addresses))
         (number-of-addresses (number-of-addresses addresses))

         (number-of-processes (number-of-processes global-addresses))
         (block-size (block-size global-addresses))
         (blocks-per-process (blocks-per-process global-addresses))
         (permutation (permutation global-addresses)))

    (assert (not (minusp offset)))

    (when (< offset number-of-addresses)
      (let ((base-address (if (< offset (* blocks-per-process block-size))
                              (* rank blocks-per-process block-size)
                              (* block-size
                                 (+ rank
                                    (* blocks-per-process
                                       (1- number-of-processes)))))))
        (apply-qubit-permutation permutation (+ offset base-address))))))

(defun %get-address-ranges (addresses)
  (let* ((rank (rank addresses))
         (number-of-processes (number-of-processes addresses))
         (block-size (block-size addresses))
         (blocks-per-process (blocks-per-process addresses))
         (remainder-blocks (remainder-blocks addresses))
         (k (* block-size blocks-per-process))
         (from-0 (* k rank))
         (end-0 (* k (1+ rank))))

    (if (< rank remainder-blocks)
        (let* ((n-times-k (* k number-of-processes))
               (from-1 (+ n-times-k (* block-size rank)))
               (end-1 (+ n-times-k (* block-size (1+ rank)))))
          (values from-0 end-0 from-1 end-1))

        (values from-0 end-0))))

(defmacro do-addresses ((var addresses &optional result) &body body)
  "Iterate over ADDRESSES using VAR.

The addresses are generated on the fly based on the rank of ADDRESSES and the permutation."
  (alexandria:once-only (addresses)
    (alexandria:with-gensyms (start-0 stop-0 start-1 stop-1 %body permutation index)
      `(multiple-value-bind (,start-0 ,stop-0 ,start-1 ,stop-1)
           (%get-address-ranges ,addresses)

         (flet ((,%body (,var)          ; XXX (declaim (inline ,%body)) ?
                  ,@body))

           (let ((,permutation (permutation ,addresses)))

             (loop :for ,index :from ,start-0 :below ,stop-0
                   :for ,var := (apply-qubit-permutation ,permutation ,index)
                   :do (,%body ,var))

             (when (and ,start-1 ,stop-1)
               (loop :for ,index :from ,start-1 :below ,stop-1
                     :for ,var := (apply-qubit-permutation ,permutation ,index)
                     :do (,%body ,var)))

             ,result))))))

(defmethod print-object ((addresses addresses) stream)
  (let ((*print-readably* nil)
        (*print-pretty* nil))

    (print-unreadable-object (addresses stream :type t :identity t)

      (format stream "~@{~S ~S~^ ~}"
              :rank (rank addresses)
              :global-addresses (global-addresses addresses))

      (when *print-addresses*
        (let (address-list)
          (do-addresses (address addresses)
            (push address address-list))
          (when address-list
            (format stream " ~S (~{~D~^ ~})"
                    :addresses (nreverse address-list))))))))
