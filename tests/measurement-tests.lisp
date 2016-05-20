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
