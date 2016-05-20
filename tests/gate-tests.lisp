;;;; tests/gate-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defun vector-first (a)
  (aref a 0))

(defun vector-last (a)
  (aref a (1- (length a))))

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
    (is (double-float= 1 (probability (vector-last (qvm::amplitudes qvm))) 1/10000))))

(deftest test-bell ()
  "Test the construction of a Bell pair."
  (labels ((bell-state (n)
             "Construct an N-qubit Bell state."
             (cons
              '(H 0)
              (loop :for i :from 1 :below n
                    :collect `(cnot 0 ,i))))
           (run-bell (i)
             (qvm::amplitudes (run-program i (bell-state i)))))
    (loop :for i :from 1 :to 7 
          :for amps := (run-bell i)
          :do (is (double-float= 1/2 (probability (vector-first amps)) 1/10000))
              (is (double-float= 1/2 (probability (vector-last amps)) 1/10000)))))
