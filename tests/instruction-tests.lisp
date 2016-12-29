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

(deftest test-wait ()
  (let ((p (with-output-to-quil
             "WAIT")))
    (not-signals error
      (run-program 1 p))))

(deftest test-pragma ()
  (let ((p (with-output-to-quil
             "PRAGMA hello"
             "PRAGMA hello world"
             "PRAGMA hello world \"foo\"")))
    (not-signals error
      (run-program 1 p))))

(deftest test-classical-unaries ()
  "Test the classical unary instructions TRUE, FALSE, and NOT."
  (let* ((p (with-output-to-quil
              "TRUE [0]"
              "TRUE [1]"
              "FALSE [1]"
              "NOT [2]"
              "TRUE [3]"
              "NOT [3]"))
         (q (run-program 1 p)))
    (is (= 1 (classical-bit q 0)))
    (is (= 0 (classical-bit q 1)))
    (is (= 1 (classical-bit q 2)))
    (is (= 0 (classical-bit q 3)))))

(deftest test-classical-binaries ()
  "Test the classical binary instructions AND, OR, MOVE, and EXCHANGE."
  (flet ((test-prog (instr a b expected-a expected-b)
           (let* ((p (with-output-to-quil
                       (when (= 1 a)
                         (format t "TRUE [0]~%"))
                       (when (= 1 b)
                         (format t "TRUE [1]~%"))
                       (format t "~A [0] [1]~%" instr)))
                  (q (run-program 1 p)))
             (is (= expected-a (classical-bit q 0)))
             (is (= expected-b (classical-bit q 1))))))
    (test-prog "AND" 0 0  0 0)
    (test-prog "AND" 0 1  0 0)
    (test-prog "AND" 1 0  1 0)
    (test-prog "AND" 1 1  1 1)

    (test-prog "OR" 0 0  0 0)
    (test-prog "OR" 0 1  0 1)
    (test-prog "OR" 1 0  1 1)
    (test-prog "OR" 1 1  1 1)

    (test-prog "MOVE" 0 0  0 0)
    (test-prog "MOVE" 0 1  0 0)
    (test-prog "MOVE" 1 0  1 1)
    (test-prog "MOVE" 1 1  1 1)

    (test-prog "EXCHANGE" 0 0  0 0)
    (test-prog "EXCHANGE" 0 1  1 0)
    (test-prog "EXCHANGE" 1 0  0 1)
    (test-prog "EXCHANGE" 1 1  1 1)))

(deftest test-flip-circuit ()
  "Test the FLIP circuit. Will flip [0] once, and [1] twice. Tests in particular the ability to generate unique labels within circuits."
  (let* ((p (with-output-to-quil
              ;; We need 3 blank ancillas.
              "DEFCIRCUIT FLIP addr ancilla:"
              "    JUMP-WHEN @one addr"
              "    X ancilla"
              "    LABEL @one"
              "    MEASURE ancilla addr"
              "FLIP [0] 0"
              "FLIP [1] 1"
              "FLIP [1] 2"))
         (q (run-program 3 p)))
    (is (= 1 (classical-bit q 0)))
    (is (= 0 (classical-bit q 1)))))
