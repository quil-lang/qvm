;;; tests/suite.lisp
;;;
;;; Author: Juan M. Bello-Rivas
;;;         Robert Smith

(in-package #:dqvm2-tests)

(defun run-dqvm2-tests (&key (headless nil))
  "Run all QVM tests. If HEADLESS is T, disable interactive debugging and quit on completion."
  ;; Bug in Fiasco commit fe89c0e924c22c667cc11c6fc6e79419fc7c1a8b
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream
                                            *standard-output*))
  (qvm:prepare-for-parallelization)
  (cond
    ((null headless)
     (run-package-tests :package ':dqvm2-tests
                        :verbose nil
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':dqvm2-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (qvm::boolean-bit (not successp)))))))

(deftest test-addresses ()
  (signals error (make-instance 'addresses))
  (signals error (make-addresses :rank -1 :number-of-qubits 1))
  (signals error (make-addresses :rank 0 :number-of-processes -1 :number-of-qubits 1))
  (signals error (make-addresses :rank 1 :number-of-processes 1 :number-of-qubits 1))

  ;; Test basic facilities.
  (let* ((a-0 (make-addresses :rank 0 :number-of-processes 3 :number-of-qubits 5))
         (a-1 (make-addresses :rank 1 :number-of-processes 3 :number-of-qubits 5))
         (a-2 (make-addresses :rank 2 :number-of-processes 3 :number-of-qubits 5))
         (block-size dqvm2::*default-block-size*)
         (blocks-per-process (floor (dqvm2::number-of-blocks (global-addresses a-0))
                                    (dqvm2::number-of-processes (global-addresses a-0)))))

    (is (= (dqvm2::number-of-addresses (global-addresses a-0)))
        (expt 2 (number-of-qubits a-0)))

    (is (= (number-of-addresses a-0) (* (1+ blocks-per-process) block-size)))
    (is (= (number-of-addresses a-1) (* (1+ blocks-per-process) block-size)))
    (is (= (number-of-addresses a-2) (* blocks-per-process block-size)))

    (is (address-member 0 a-0))
    (is (not (address-member 31 a-0)))
    (is (address-member 31 a-1))

    (is (null (offset a-0 (1+ (* blocks-per-process block-size)))))
    (is (= (offset a-0 0) 0))
    (is (= (offset a-0 24) 8))
    (is (= (offset a-1 31) 11)))

  ;; Test get address by offset.
  (flet ((test-get-address-by-offset (addresses)
           (let ((offset 0))
             (do-addresses (address addresses)
               (is (= (get-address-by-offset addresses offset) address))
               (incf offset)))))

    (test-get-address-by-offset (make-addresses :rank 0 :number-of-processes 3 :number-of-qubits 6))
    (test-get-address-by-offset (make-addresses :rank 1 :number-of-processes 3 :number-of-qubits 6))
    (test-get-address-by-offset (make-addresses :rank 2 :number-of-processes 3 :number-of-qubits 6)))

  ;; Test consistency.
  (flet ((test-rank (p n)
           "Verify that the addresses in a table are consistent with its prescribed rank."
           (let ((addrs-list (loop :for r :from 0 :below p
                                   :collect (make-addresses :rank r :number-of-processes p :number-of-qubits n))))
             (loop :with global-addrs := (global-addresses (first addrs-list))
                   :for addrs :in addrs-list :do
                     (do-addresses (a addrs)
                       (is (= (dqvm2::get-rank-by-address global-addrs a) (rank addrs))))))))

    (test-rank 1 5)
    (test-rank 2 5)
    (test-rank 3 5)
    (test-rank 4 5)
    (test-rank 5 5)))

(deftest test-apply-qubit-permutation ()
  (let* ((n 3)
         (perm '((0 . 1) (1 . 2) (0 . 2)))
         (addresses (coerce (loop :for i :from 0 :below (expt 2 n) :collect i) 'vector)))
    (is (equalp addresses (apply-qubit-permutation nil (copy-seq addresses))))
    (is (equalp #(0 1 4 5 2 3 6 7) (apply-qubit-permutation perm (copy-seq addresses))))
    (is (equalp addresses (apply-qubit-permutation perm (apply-qubit-permutation perm (copy-seq addresses)))))
    (is (equalp addresses (apply-qubit-permutation (append perm perm) (copy-seq addresses))))))

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
  (check-type next-permutation permutation)

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
                    (program (quil:parse-quil (format nil "X 1~%Z 2~%I 1~%CNOT 1 2~%Y 2~%")))
                    (instructions (coerce (quil:parsed-program-executable-code program) 'list))
                    (permutations (mapcar #'dqvm2::qubit-permutation instructions)))

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
                             :for next-permutation := (revappend pi-1 pi-2)
                             :do (apply-qubit-permutation next-permutation xs)
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

(deftest test-do-addresses-in-block ()
  (labels ((get-addresses-in-block (addresses block-index)
             (let (address-list)
               (do-addresses-in-block (address addresses block-index (nreverse address-list))
                 (push address address-list))))

           (test-block (addresses block)
             (loop :for address :in block
                   :for block-index := (get-block-by-address (global-addresses addresses) address) :do
                     (is (equalp (get-addresses-in-block addresses block-index) block)))))

    (let* ((permutation '((0 . 1)))
           (global-addresses (make-instance 'global-addresses :number-of-processes 3
                                                              :number-of-qubits 4
                                                              :block-size 2
                                                              :permutation permutation))
           (a-0 (make-instance 'addresses :rank 0 :global-addresses global-addresses))
           (a-1 (dqvm2::make-addresses-like a-0 :rank 1))
           (a-2 (dqvm2::make-addresses-like a-0 :rank 2)))
      (test-block a-0 '(0 2))
      (test-block a-0 '(1 3))
      (test-block a-0 '(12 14))
      (test-block a-1 '(4 6))
      (test-block a-1 '(5 7))
      (test-block a-1 '(13 15))
      (test-block a-2 '(8 10))
      (test-block a-2 '(9 11))
      (is (null (get-addresses-in-block a-0 128))))

    (let* ((permutation '((0 . 1)))
           (global-addresses (make-instance 'global-addresses :number-of-processes 3
                                                              :number-of-qubits 4
                                                              :block-size 4
                                                              :permutation permutation))
           (a-0 (make-instance 'addresses :rank 0 :global-addresses global-addresses))
           (a-1 (dqvm2::make-addresses-like a-0 :rank 1))
           (a-2 (dqvm2::make-addresses-like a-0 :rank 2)))
      (test-block a-0 '(0 2 1 3))
      (test-block a-0 '(12 14 13 15))
      (test-block a-1 '(4 6 5 7))
      (test-block a-2 '(8 10 9 11))
      (is (null (get-addresses-in-block a-0 128))))))
