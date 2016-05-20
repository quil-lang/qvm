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
                      (is (double-float= expected-probability (probability z) 1/10000)))
                    (qvm::amplitudes qvm)))))
    (dotimes (i 7)
      (test-size i))))
