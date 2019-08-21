;;; test-suite.lisp
;;;
;;; Author: Juan M. Bello-Rivas

(fiasco:define-test-package #:dqvm2-test
  (:use #:dqvm2))

(cl:in-package #:dqvm2-test)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defun make-addresses (rank number-of-processes number-of-qubits)
  (make-instance 'addresses :rank rank
                            :number-of-processes number-of-processes
                            :number-of-qubits number-of-qubits))

(deftest test-addresses ()
  (signals error (make-instance 'addresses))
  (signals error (make-instance 'addresses :rank -1 :number-of-qubits 1))
  (signals error (make-instance 'addresses :rank 0 :number-of-processes -1 :number-of-qubits 1))
  (signals error (make-instance 'addresses :rank 1 :number-of-processes 1 :number-of-qubits 1))

  ;; Test basic facilities.
  (let* ((a-0 (make-addresses 0 3 5))
         (a-1 (make-addresses 1 3 5))
         (a-2 (make-addresses 2 3 5))
         (block-size dqvm2::*default-block-size*)
         (blocks-per-process (floor (dqvm2::get-total-blocks a-0)
                                    (number-of-processes a-0))))

    (is (= (dqvm2::total-number-of-addresses a-0))
        (ash 1 (number-of-qubits a-0)))

    (is (= (number-of-addresses a-0) (* (1+ blocks-per-process) block-size)))
    (is (= (number-of-addresses a-1) (* (1+ blocks-per-process) block-size)))
    (is (= (number-of-addresses a-2) (* blocks-per-process block-size)))

    (is (member-p a-0 0))
    (is (not (member-p a-0 31)))
    (is (member-p a-1 31))

    (is (eq nil (offset-of a-0 (1+ (* blocks-per-process block-size)))))
    (is (= (offset-of a-0 0) 0))
    (is (= (offset-of a-0 24) 8))
    (is (= (offset-of a-1 31) 11)))

  ;; Test get address by offset.
  (flet ((test-get-address-by-offset (addresses)
           (let ((offset 0))
             (do-addresses (address addresses)
               (is (= (get-address-by-offset addresses offset) address))
               (incf offset)))))

    (test-get-address-by-offset (make-addresses 0 3 6))
    (test-get-address-by-offset (make-addresses 1 3 6))
    (test-get-address-by-offset (make-addresses 2 3 6)))

  ;; Test consistency.
  (flet ((test-rank (p n)
           "Verify that the addresses in a table are consistent with its prescribed rank."
           (let ((addrs-list (loop :for r :from 0 :below p
                                   :collect (make-addresses r p n))))
             (loop :for addrs :in addrs-list :do
               (do-addresses (a addrs)
                 (is (= (get-rank addrs a) (rank addrs))))))))

    (test-rank 1 5)
    (test-rank 2 5)
    (test-rank 3 5)
    (test-rank 4 5)
    (test-rank 5 5)))

(deftest test-apply-qubit-permutation ()
  (let* ((n 3)
         (perm '((0 1) (1 2) (0 2)))
         (addresses (coerce (loop :for i :from 0 :below (ash 1 n) :collect i) 'vector)))
    (is (equalp addresses (apply-qubit-permutation nil (copy-seq addresses))))
    (is (equalp #(0 1 4 5 2 3 6 7) (apply-qubit-permutation perm (copy-seq addresses))))
    (is (equalp addresses (apply-qubit-permutation perm (apply-qubit-permutation perm (copy-seq addresses)))))
    (is (equalp addresses (apply-qubit-permutation (append perm perm) (copy-seq addresses))))))

(deftest test-distributed-qvm-addresses ()
  (let* ((num-qubits 8)
         (number-of-processes 4)
         (rank 1)
         (qvm (make-instance 'distributed-qvm :rank rank
                                              :number-of-processes number-of-processes
                                              :number-of-qubits num-qubits))
         (block-length (/ (ash 1 num-qubits) 4))
         (expected-addresses (loop :for i :from (* block-length rank)
                                   :below (* block-length (1+ rank))
                                   :collect i))
         (actual-addresses (let (x)
                             (do-addresses (address (addresses qvm) (nreverse x))
                               (push address x)))))

    (is (equalp actual-addresses expected-addresses))))

(deftest test-distributed-qvm-consistency ()
  (labels ((make-qvm (r p n)
             (make-instance 'distributed-qvm :rank r :number-of-processes p :number-of-qubits n))

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
                 (with-slots (addresses)
                     qvm-a
                   (let* ((next-permutation (qubit-permutation instruction))
                          (next-addresses (find-next-addresses addresses next-permutation)))
                     (is (equalp (get-addresses qvm-a) next-addresses))
                     (update-permutation addresses next-permutation))))

               ;; Make sure the outcome from the previous step is the same as applying the permutations.
               (let ((addresses-b
                       (loop :with xs := (coerce (get-addresses qvm-b) 'vector)
                             :for pi-1 :in (cons nil permutations)
                             :for pi-2 :in permutations
                             :for next-permutation := (append (reverse pi-1) pi-2)
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
