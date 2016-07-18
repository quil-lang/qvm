;;;; instruction-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-defgate ()
  (let* ((p (with-output-to-quil
              (write-line "DEFGATE TEST:")
              (write-line "    0, 1")
              (write-line "    1, 0")
              (write-line "TEST 0")
              (write-line "MEASURE 0 [0]")))
         (q (run-program 1 p)))
    (is (= 1 (classical-bit q 0)))))

(deftest test-nop ()
  (let* ((p (with-output-to-quil
              (dotimes (i 5)
                (write-line "NOP"))))
         (q (run-program 1 p)))
    (is (double-float= 0 (qubit-probability q 0)))))

(deftest test-jump ()
  (let* ((p (with-output-to-quil
              (write-line "X 0")
              (write-line "JUMP @SKIP")
              (write-line "X 0")
              (write-line "LABEL @SKIP")))
         (q (run-program 1 p)))
    (is (double-float= 1 (qubit-probability q 0)))))

(deftest test-jump-when ()
  (let* ((p (with-output-to-quil
              (write-line "X 0")
              (write-line "MEASURE 0 [0]")
              (write-line "JUMP-WHEN @SKIP [0]")
              (write-line "X 0")
              (write-line "LABEL @SKIP")))
         (q (run-program 1 p)))
    (is (double-float= 1 (qubit-probability q 0)))))

(deftest test-jump-unless ()
  (let* ((p (with-output-to-quil
              (write-line "X 0")
              (write-line "LABEL @REDO")
              (write-line "MEASURE 0 [0]")
              (write-line "JUMP-UNLESS @SKIP [0]")
              (write-line "X 0")
              (write-line "JUMP @REDO")
              (write-line "LABEL @SKIP")))
         (q (run-program 1 p)))
    (is (double-float= 0 (qubit-probability q 0)))))

(deftest test-reset ()
  (let* ((p (with-output-to-quil
              (write-line "X 0")
              (write-line "X 1")
              (write-line "RESET")))
         (q (run-program 2 p)))
    (is (double-float= 0 (qubit-probability q 0)))
    (is (double-float= 0 (qubit-probability q 1)))))

(deftest test-halt ()
  (let* ((p (with-output-to-quil
              (write-line "X 0")
              (write-line "HALT")
              (write-line "X 1")))
         (q (run-program 2 p)))
    (is (double-float= 1 (qubit-probability q 0)))
    (is (double-float= 0 (qubit-probability q 1)))))

#+ignore
(deftest test-wait ()
  nil)
