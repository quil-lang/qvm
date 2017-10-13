;;;; noisy-qvm-tests.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Nikolas Tezak

(in-package #:qvm-tests)

(deftest test-simple-gate-noise ()
  "Test that the noisy gates behave as expected."
  (let ((p (with-output-to-quil
             (write-line "X 0")))
        (tries 500)
        (results-desired
          (list
           (qvm::make-vector 2 0 -1)
           (qvm::make-vector 2 1 0)
           (qvm::make-vector 2 (complex 0 -1) 0)))
        (qvm (make-instance 'qvm:depolarizing-qvm
                            :x 1/3
                            :y 1/3
                            :z 1/3
                            :number-of-qubits 1
                            :classical-memory-size 64)))
    (qvm:load-program qvm p)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((psi (qvm::amplitudes (progn
                                            (qvm::reset qvm)
                                            (qvm:run qvm)))))
                (setf results-desired
                      (remove psi results-desired :test #'equalp))))
    (is (plusp tries))))

(deftest test-simple-measurement-noise ()
  "Test that the noisy gates behave as expected."
  (let ((p (with-output-to-quil
             (write-line "MEASURE 0 [0]")))
        (tries 500)
        (results-desired (list 0 1))
        (qvm (make-instance 'qvm:depolarizing-qvm
                            :measure-x 1/5
                            :number-of-qubits 1
                            :classical-memory-size 64)))
    (qvm:load-program qvm p)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((result (qvm::classical-bit (progn
                                                  (qvm::reset qvm)
                                                  (qvm::reset-classical-memory qvm)
                                                  (qvm:run qvm))
                                                0)))
                (setf results-desired
                      (remove result results-desired :test #'=))))
    (is (plusp tries))))


(deftest test-noisy-x-gate ()
  "Test that the noisy gates behave as expected."
  (let ((p (with-output-to-quil
             (write-line "X 0")))
        (tries 500)
        (results-desired
          (list
           (qvm::make-vector 2 0 1)
           (qvm::make-vector 2 1 0)
           (qvm::make-vector 2 (complex 0 -1) 0)))
        (qvm (make-instance 'qvm:noisy-qvm :number-of-qubits 1
                                           :classical-memory-size 64)))
    (qvm:load-program qvm p)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((psi (qvm::amplitudes (progn
                                            (qvm::reset qvm)
                                            (qvm:set-noisy-gate qvm "X" '(0) (qvm::make-pauli-perturbed-1q-gate "X" 1/4 1/4 1/4))
                                            (qvm:run qvm)))))
                (setf results-desired
                      (remove psi results-desired :test #'equalp))))
    (is (plusp tries))))
