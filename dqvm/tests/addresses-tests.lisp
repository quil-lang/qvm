;;;; tests/addresses-tests.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2-tests)

(deftest test-addresses ()
  (signals error (make-instance 'addresses))
  (signals error (make-addresses :rank -1 :number-of-qubits 1))
  (signals error (make-addresses :rank 0 :number-of-processes -1 :number-of-qubits 1))
  (signals error (make-addresses :rank 1 :number-of-processes 1 :number-of-qubits 1))

  ;; Test basic facilities.
  (let* ((block-size 4)
         (a-0 (make-addresses :rank 0 :number-of-processes 3 :number-of-qubits 5 :block-size block-size))
         (a-1 (make-addresses :rank 1 :number-of-processes 3 :number-of-qubits 5 :block-size block-size))
         (a-2 (make-addresses :rank 2 :number-of-processes 3 :number-of-qubits 5 :block-size block-size))
         (blocks-per-process (floor (dqvm2::number-of-blocks (global-addresses a-0))
                                    (dqvm2::number-of-processes (global-addresses a-0)))))

    (is (= (dqvm2::number-of-addresses (global-addresses a-0)))
        (expt 2 (number-of-qubits a-0)))

    (is (<= (number-of-addresses a-0) (* (1+ blocks-per-process) block-size)))
    (is (<= (number-of-addresses a-1) (* (1+ blocks-per-process) block-size)))
    (is (<= (number-of-addresses a-2) (* blocks-per-process block-size)))

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

(deftest test-do-addresses-in-block ()
  (labels ((get-addresses-in-block (global-addresses block-index)
             (let (address-list)
               (do-addresses-in-block (address global-addresses block-index (nreverse address-list))
                 (push address address-list))))

           (test-block (addresses block)
             (loop :with global-addresses := (global-addresses addresses)
                   :for address :in block
                   :for block-index := (get-block-by-address global-addresses address) :do
                     (is (equalp (get-addresses-in-block global-addresses block-index) block)))))

    (let* ((permutation (dqvm2::make-permutation '((0 . 1))))
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

    (let* ((permutation (dqvm2::make-permutation '((0 . 1))))
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
