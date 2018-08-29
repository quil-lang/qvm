;;;; instruction-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-empty-program-regular-qvm ()
  (let* ((p (with-output-to-quil))
         (q (run-program 1 p)))
    (is (double-float= 0 (qubit-probability q 0)))))

(deftest test-defgate ()
  (let* ((p (with-output-to-quil
              "DECLARE ro BIT"
              "DEFGATE TEST:"
              "    0, 1"
              "    1, 0"
              "TEST 0"
              "MEASURE 0 ro[0]"))
         (q (run-program 1 p)))
    (is (= 1 (qvm:memory-ref q "ro" 0)))))

(deftest test-nop ()
  (let* ((p (with-output-to-quil
              (dotimes (i 5)
                (write-line "NOP"))))
         (q (run-program 1 p)))
    (is (double-float= 0 (qubit-probability q 0)))))

(deftest test-jump ()
  (let* ((p (with-output-to-quil
              "X 0"
              "JUMP @SKIP"
              "X 0"
              "LABEL @SKIP"))
         (q (run-program 1 p)))
    (is (double-float= 1 (qubit-probability q 0)))))

(deftest test-jump-when ()
  (let* ((p (with-output-to-quil
              "DECLARE ro BIT"
              "X 0"
              "MEASURE 0 ro[0]"
              "JUMP-WHEN @SKIP ro[0]"
              "X 0"
              "LABEL @SKIP"))
         (q (run-program 1 p)))
    (is (double-float= 1 (qubit-probability q 0)))))

(deftest test-jump-unless ()
  (let* ((p (with-output-to-quil
              "DECLARE ro BIT"
              "X 0"
              "LABEL @REDO"
              "MEASURE 0 ro[0]"
              "JUMP-UNLESS @SKIP ro[0]"
              "X 0"
              "JUMP @REDO"
              "LABEL @SKIP"))
         (q (run-program 1 p)))
    (is (double-float= 0 (qubit-probability q 0)))))

(deftest test-reset ()
  (let* ((p (with-output-to-quil
              "X 0"
              "X 1"
              "RESET"))
         (q (run-program 2 p)))
    (is (double-float= 0 (qubit-probability q 0)))
    (is (double-float= 0 (qubit-probability q 1)))))

(deftest test-reset-qubit ()
  (loop :repeat 10 :do
    (let* ((p (with-output-to-quil
                "X 0"
                "X 1"
                "H 2"
                "RESET 0"
                "RESET 2"))
           (q (run-program 2 p)))
      (is (double-float= 0 (qubit-probability q 0)))
      (is (double-float= 1 (qubit-probability q 1)))
      (is (double-float= 0 (qubit-probability q 2))))))

(deftest test-halt ()
  (let* ((p (with-output-to-quil
              "X 0"
              "HALT"
              "X 1"))
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

(deftest test-move ()
  "Test that the MOVE instruction works, since it's instrumental to the rest of the tests."
  (let* ((p (with-output-to-quil
              "DECLARE b BIT[2]"
              "DECLARE o OCTET[2]"
              "DECLARE int INTEGER[2]"
              "DECLARE r REAL[2]"

              "MOVE b[0] 1"
              "MOVE o[0] 2"
              "MOVE int[0] 3"
              "MOVE r[0] 4.0"
              "MOVE b[1] b[0]"
              "MOVE o[1] o[0]"
              "MOVE int[1] int[0]"
              "MOVE r[1] r[0]"))
         (q (run-program 1 p)))
    (is (= 1 (qvm:memory-ref q "b" 0)))
    (is (= 1 (qvm:memory-ref q "b" 1)))
    (is (= 2 (qvm:memory-ref q "o" 0)))
    (is (= 2 (qvm:memory-ref q "o" 1)))
    (is (= 3 (qvm:memory-ref q "int" 0)))
    (is (= 3 (qvm:memory-ref q "int" 1)))
    (is (= 4.0d0 (qvm:memory-ref q "r" 0)))
    (is (= 4.0d0 (qvm:memory-ref q "r" 1)))))


(deftest test-classical-unaries ()
  "Test the classical unary instructions TRUE, FALSE, and NOT."
  (let* ((p (with-output-to-quil
              "DECLARE r REAL"
              "DECLARE int INTEGER"
              "MOVE r 1.0"
              "MOVE int 1"))
         (q (run-program 1 p)))
    (is (= 1 (qvm:memory-ref q "r" 0)))
    (is (= 1 (qvm:memory-ref q "int" 0)))))

#+ign
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
              "DECLARE ro BIT[2]"
              "DEFCIRCUIT FLIP addr ancilla:"
              "    JUMP-WHEN @one addr"
              "    X ancilla"
              "    LABEL @one"
              "    MEASURE ancilla addr"
              "FLIP ro[0] 0"
              "FLIP ro[1] 1"
              "FLIP ro[1] 2"))
         (q (run-program 3 p)))
    (is (= 1 (qvm:memory-ref q "ro" 0)))
    (is (= 0 (qvm:memory-ref q "ro" 1)))))
