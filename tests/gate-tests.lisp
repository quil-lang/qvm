;;;; tests/gate-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-hadamard ()
  "Test Hadamard initialization on several qubits."
  (flet ((test-size (size)
           (let* ((qil (loop :for q :below size :collect `(H ,q)))
                  (qvm (run-program size qil))
                  (expected-probability (expt 2 (- size))))
             (every (lambda (z)
                      (double-float= expected-probability (probability z) 1/10000))
                    (qvm::amplitudes qvm)))))
    (dotimes (i 7)
      (is (test-size i)))))

(deftest test-inversion ()
  "Test |000> -> |111> inversion."
  (let* ((qil '((NOT 0) (NOT 1) (NOT 2)))
         (qvm (run-program 3 qil)))
    (is (double-float= 1 (abs (aref (qvm::amplitudes qvm) (1- (expt 2 3)))) 1/10000))))
