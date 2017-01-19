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
  (let ((p (with-output-to-quil
             "X 0"
             "X 2"
             "MEASURE 0 [0]"
             "MEASURE 1 [1]"
             "MEASURE 2 [2]")))
    (let ((qvm (qvm:run-program (cl-quil:qubits-needed p) p)))
      (is (= 1 (qvm::classical-bit qvm 0)))
      (is (= 0 (qvm::classical-bit qvm 1)))
      (is (= 1 (qvm::classical-bit qvm 2))))))

(deftest test-simple-measurements-with-swap ()
  "Test that some simple measurements work with SWAP."
  (let ((p (with-output-to-quil
             "X 0"
             "X 2"
             "SWAP 0 1"
             "SWAP 0 2"
             "MEASURE 0 [0]"
             "MEASURE 1 [1]"
             "MEASURE 2 [2]")))
    (let ((qvm (qvm:run-program (cl-quil:qubits-needed p) p)))
      (is (= 1 (qvm::classical-bit qvm 0)))
      (is (= 1 (qvm::classical-bit qvm 1)))
      (is (= 0 (qvm::classical-bit qvm 2))))))

(defun test-range (program repetitions percent-zeros percent-ones &key (tolerance 0.05))
  (let ((q (qvm:make-qvm (cl-quil:qubits-needed program)))
        (counts (vector 0 0)))
    (loop :repeat repetitions
          :do (setf (qvm::classical-memory q) 0)
              (qvm:load-program q program)
              (qvm:run q)
              (incf (aref counts (classical-bit q 0))))
    (let ((got-percent-zeros (float (/ (aref counts 0) repetitions)))
          (got-percent-ones  (float (/ (aref counts 1) repetitions))))
      (is (<= (max 0.0 (- percent-zeros tolerance))
              got-percent-zeros
              (min 1.0 (+ percent-zeros tolerance))))
      (is (<= (max 0.0 (- percent-ones tolerance))
              got-percent-ones
              (min 1.0 (+ percent-ones tolerance)))))))

(deftest test-hadamard-measurements ()
  (let ((n 100000)
        (p (with-output-to-quil
             "I 1"
             "H 0"
             "MEASURE 0 [0]"
             "RESET")))
    (test-range p n 0.5 0.5)))

(deftest test-quarter-rotation-measurements ()
  (let* ((n 100000)
         (one (expt (sin (/ pi 4 2)) 2))
         (zero (- 1 one))
         (p (with-output-to-quil
              "I 1"
              "RX(pi/4) 0"
              "MEASURE 0 [0]"
              "RESET")))
    (test-range p n zero one)))

(deftest test-three-quarter-rotation-measurements ()
  (let* ((n 100000)
         (zero (expt (sin (/ pi 4 2)) 2))
         (one (- 1 zero))
         (p (with-output-to-quil
              "I 1"
              "RX(3*pi/4) 0"
              "MEASURE 0 [0]"
              "RESET")))
    (test-range p n zero one)))

(deftest test-eighth-rotation-measurements ()
  (let* ((n 100000)
         (one (expt (sin (/ pi 8 2)) 2))
         (zero (- 1 one))
         (p (with-output-to-quil
              "I 1"
              "RX(pi/8) 0"
              "MEASURE 0 [0]"
              "RESET")))
    (test-range p n zero one)))
