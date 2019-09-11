;;;; tests/distributed-qvm-tests.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2-tests)

(deftest test-distributed-qvm-addresses ()
  (let* ((num-qubits 8)
         (number-of-processes 4)
         (rank 1)
         (qvm (make-distributed-qvm :rank rank
                                    :number-of-processes number-of-processes
                                    :number-of-qubits num-qubits))
         (block-length (/ (expt 2 num-qubits) 4))
         (expected-addresses (loop :for i :from (* block-length rank)
                                     :below (* block-length (1+ rank))
                                   :collect i))
         (actual-addresses (let (x)
                             (do-addresses (address (addresses qvm) (nreverse x))
                               (push address x)))))

    (is (equalp actual-addresses expected-addresses))))

(defun find-next-addresses (addresses next-permutation)
  "Find amplitude addresses to exchange when applying NEXT-PERMUTATION and the rank where the amplitudes are located.

Returns four sequences: current addresses, new addresses, and the source and target addresses."
  (let ((permutation (permutation addresses))
        (effective-permutation
          (dqvm2::get-effective-permutation addresses next-permutation))
        (orig-addresses nil)
        (next-addresses nil)
        (target-addresses nil)
        (source-addresses nil))

    (do-addresses (address addresses (values (nreverse orig-addresses)
                                             (nreverse next-addresses)
                                             (nreverse target-addresses)
                                             (nreverse source-addresses)))
      ;; First, find the next address $y = π₂ ∘ π₁⁻¹(x)$, where $x$ is the current address. Next, compute $z = π₁^{-1}(y)$.
      (let* ((next-address (apply-qubit-permutation effective-permutation address))
             (target-address (apply-inverse-qubit-permutation next-permutation address))
             (source-address (apply-inverse-qubit-permutation permutation next-address)))

        (push address orig-addresses)
        (push next-address next-addresses)
        (push target-address target-addresses)
        (push source-address source-addresses)))))

(deftest test-distributed-qvm-consistency ()
  (labels ((make-qvm (r p n)
             (make-distributed-qvm :rank r :number-of-processes p :number-of-qubits n))

           (get-addresses (qvm)
             "Get a list of addresses contained in the address table of DQVM."
             (let (addresses)
               (do-addresses (address (addresses qvm) (nreverse addresses))
                 (push address addresses))))

           (test-consistency (rank number-of-processes number-of-qubits)
             "Check the consistency of the distributed QVM throughout applying permutations to its address table."
             (let* ((qvm-a (make-qvm rank number-of-processes number-of-qubits))
                    (qvm-b (make-qvm rank number-of-processes number-of-qubits))
                    (program (quil:parse-quil "X 1; Z 2; I 1; CNOT 1 2; Y 2"))
                    (instructions (coerce (quil:parsed-program-executable-code program) 'list))
                    (permutations (mapcar #'qubit-permutation instructions)))

               ;; Simulate transitions one instruction at a time and check that the updated qvm is consistent with the applied permutations.
               (dolist (instruction instructions)
                 (let* ((addresses (addresses qvm-a))

                        (next-permutation (qubit-permutation instruction))
                        (next-addresses (find-next-addresses addresses next-permutation)))

                   (is (equalp (get-addresses qvm-a) next-addresses))
                   (update-permutation next-permutation addresses)))

               ;; Make sure the outcome from the previous step is the same as applying the permutations.
               (let ((addresses-b
                       (loop :with xs := (coerce (get-addresses qvm-b) 'vector)
                             :for pi-1 :in (cons nil permutations)
                             :for pi-2 :in permutations
                             :for next-permutation := (compose-permutations pi-2 (inverse-permutation pi-1))
                             :do (setf xs (map 'vector (lambda (x) (apply-qubit-permutation next-permutation x)) xs))
                             :finally (return (coerce xs 'list)))))
                 (is (equalp (get-addresses qvm-a) addresses-b))))))

    (test-consistency 0 4 8)
    (test-consistency 1 4 8)
    (test-consistency 2 4 8)
    (test-consistency 3 4 8)

    (test-consistency 0 5 8)
    (test-consistency 1 5 8)
    (test-consistency 2 5 8)
    (test-consistency 3 5 8)))
