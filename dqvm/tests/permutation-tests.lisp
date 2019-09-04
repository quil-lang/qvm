;;;; tests/permutation-tests.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2-tests)

(deftest test-permutation ()
  (signals error (make-permutation '((1 . 0) (1 . 2))))
  (signals error (make-permutation '((1 . 2) (3 . 2))))

  (let ((permutation '()))
    (is (is-identity-permutation-p permutation))

    (is (= (apply-permutation permutation 0) 0))
    (is (= (apply-permutation permutation 1) 1))
    (is (= (apply-permutation permutation 2) 2)))

  (let ((permutation (make-permutation)))
    (is (is-identity-permutation-p permutation))

    (is (= (apply-permutation permutation 0) 0))
    (is (= (apply-permutation permutation 1) 1))
    (is (= (apply-permutation permutation 2) 2))

    (is (= (apply-inverse-permutation permutation 0) 0))
    (is (= (apply-inverse-permutation permutation 1) 1))
    (is (= (apply-inverse-permutation permutation 2) 2)))

  (let ((permutation (make-permutation '((2 . 1) (1 . 0)))))
    (is (not (is-identity-permutation-p permutation)))

    (is (= (apply-permutation permutation 2) 1))
    (is (= (apply-permutation permutation 1) 0))
    (is (= (apply-permutation permutation 0) 2))

    (is (= (apply-inverse-permutation permutation 0) 1))
    (is (= (apply-inverse-permutation permutation 1) 2))
    (is (= (apply-inverse-permutation permutation 2) 0))

    (is (= (apply-qubit-permutation permutation #b000) #b000))
    (is (= (apply-qubit-permutation permutation #b001) #b100))
    (is (= (apply-qubit-permutation permutation #b010) #b001))
    (is (= (apply-qubit-permutation permutation #b011) #b101))
    (is (= (apply-qubit-permutation permutation #b100) #b010))
    (is (= (apply-qubit-permutation permutation #b101) #b110))
    (is (= (apply-qubit-permutation permutation #b110) #b011))
    (is (= (apply-qubit-permutation permutation #b111) #b111))

    (is (= (apply-inverse-qubit-permutation permutation #b000) #b000))
    (is (= (apply-inverse-qubit-permutation permutation #b001) #b010))
    (is (= (apply-inverse-qubit-permutation permutation #b010) #b100))
    (is (= (apply-inverse-qubit-permutation permutation #b011) #b110))
    (is (= (apply-inverse-qubit-permutation permutation #b100) #b001))
    (is (= (apply-inverse-qubit-permutation permutation #b101) #b011))
    (is (= (apply-inverse-qubit-permutation permutation #b110) #b101))
    (is (= (apply-inverse-qubit-permutation permutation #b111) #b111)))

  (let* ((permutation1 (make-permutation '((2 . 1))))
         (permutation2 (make-permutation '((0 . 1) (1 . 2))))
         (composition (compose-permutations permutation1 permutation2)))
    (is (not (is-identity-permutation-p permutation1)))
    (is (not (is-identity-permutation-p permutation2)))

    (is (is-identity-permutation-p
         (compose-permutations permutation1
                               (inverse-permutation permutation1))))

    (is (is-identity-permutation-p
         (compose-permutations permutation2
                               (inverse-permutation permutation2))))

    (is (= (apply-permutation composition 0) 2))
    (is (= (apply-permutation composition 1) 1))
    (is (= (apply-permutation composition 2) 0))))
