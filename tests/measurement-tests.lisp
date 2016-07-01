;;;; measurement-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-index-to-address ()
  "Test the INDEX-TO-ADDRESS function."
  (let ((index        #b1111)
        (test-cases '(#b11110
                      #b11101
                      #b11011
                      #b10111
                      #b01111)))
    (loop :for i :below (length test-cases)
          :for address :in test-cases
          :do (is (= address (qvm::index-to-address index i))))))

(deftest test-simple-measurements ()
  "Test that some simple measurements work."
  (let ((p (with-output-to-quil (s)
             (write-line "X 0" s)
             (write-line "X 2" s)
             (write-line "MEASURE 0 [0]" s)
             (write-line "MEASURE 1 [1]" s)
             (write-line "MEASURE 2 [2]" s))))
    (let ((qvm (qvm:run-program (cl-quil:qubits-needed p) p)))
      (is (= 1 (qvm::classical-bit qvm 0)))
      (is (= 0 (qvm::classical-bit qvm 1)))
      (is (= 1 (qvm::classical-bit qvm 2))))))
