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

  (let ((permutation (make-permutation '((2 . 1)))))
    (is (eq (type-of permutation) 'dqvm2::permutation-general)))

  (let ((permutation (make-permutation '((2 . 0)))))
    (is (eq (type-of permutation) 'dqvm2::permutation-transposition))
    (is (eq permutation (inverse-permutation permutation))))

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

(deftest benchmark-apply-qubit-permutation ()
  (labels ((get-elapsed-time-in-seconds (start stop)
             "Compute elapsed time in seconds between START and STOP."
             (float (/ (- stop start) internal-time-units-per-second)))

           (time-apply-qubit-permutation (permutation number-of-qubits)
             "Measure the time taken by calls to APPLY-QUBIT-PERMUTATION on addresses from 0 to 2^NUMBER-OF-QUBITS."
             (let ((start (get-internal-real-time)))
               (dotimes (x (expt 2 number-of-qubits))
                 (let ((y (apply-qubit-permutation permutation x)))
                   (values x y)))
               (get-elapsed-time-in-seconds start (get-internal-real-time))))

           ;; (time-map-reordered-amplitudes (permutation number-of-qubits)
           ;;   "Measure the time taken by calls to MAP-REORDERED-AMPLITUDES on addresses from 0 to 2^NUMBER-OF-QUBITS."
           ;;   (let ((nat-tuple (apply #'qvm::nat-tuple
           ;;                           (mapcar (lambda (x)
           ;;                                     (apply-permutation permutation x))
           ;;                                   (loop :for i :below number-of-qubits :collect i))))
           ;;         (start (get-internal-real-time)))
           ;;     (qvm::map-reordered-amplitudes 0 (lambda (x y) (values x y)) nat-tuple)
           ;;     (get-elapsed-time-in-seconds start (get-internal-real-time))))
           )

    (let* ((tau 4)
           (number-of-qubits 24)
           (permutation-0 (make-instance 'dqvm2::permutation-transposition :tau tau))
           (permutation-1 (make-instance 'dqvm2::permutation-general
                                         :number-of-transpositions 2
                                         :transpositions (list (cons 0 tau) (cons tau 0)))))

      (loop :for x :below (expt 2 (1+ tau)) :do
        (is (= (apply-qubit-permutation permutation-0 x)
               (apply-qubit-permutation permutation-1 x))))

      (is (> (/ (time-apply-qubit-permutation permutation-1 number-of-qubits)
                (time-apply-qubit-permutation permutation-0 number-of-qubits))
             3))

      ;; (is (> (/ (time-map-reordered-amplitudes permutation-0 number-of-qubits)
      ;;           (time-apply-qubit-permutation permutation-0 number-of-qubits))
      ;;        7))
      )))
