;;;; tests/stabilizer-qvm-tests.lisp
;;;;
;;;; Author: Andrew Shi

(in-package #:qvm-tests)

(defun test-quickrun (qvm prog)
  (qvm::load-program qvm prog)
  (qvm::run qvm))

(deftest test-zero-state ()
  (let ((pure-qvm (qvm::make-qvm 10))
        (stab-qvm (qvm::make-stabilizer-qvm 10)))
    (is (every #'cl-quil.clifford::complex~
               (qvm::amplitudes pure-qvm)
               (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm))))))

(deftest test-bell-state ()
  (let ((pure-qvm (qvm::make-qvm 2))
        (stab-qvm (qvm::make-stabilizer-qvm 2))
        (program (cl-quil::parse-quil "H 0
CNOT 0 1
S 1")))
    (test-quickrun pure-qvm program)
    (test-quickrun stab-qvm program)
    (is (every #'cl-quil.clifford::complex~
               (qvm::amplitudes pure-qvm)
               (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm))))))

(deftest test-nonclifford ()
  (let ((pure-qvm (qvm::make-qvm 2))
        (stab-qvm (qvm::make-stabilizer-qvm 2))
        (program (cl-quil::parse-quil "T 0")))
    (test-quickrun pure-qvm program)
    (signals simple-error
      (test-quickrun stab-qvm program))))

(deftest test-circuits-small ()
  (let ((pure-qvm (qvm::make-qvm 2))
        (stab-qvm (qvm::make-stabilizer-qvm 2))
        (program (cl-quil::parse-quil "T 0")))
    (test-quickrun pure-qvm program)
    (signals simple-error
      (test-quickrun stab-qvm program))))

(deftest test-one-qubit-one-clifford ()
  (dotimes (i 100)
    (let* ((n 3)
           (pure-qvm (qvm::make-qvm n))
           (stab-qvm (qvm::make-stabilizer-qvm n))
           (program (qvm::random-clifford-program 1 1 n)))
      (test-quickrun pure-qvm program)
      (test-quickrun stab-qvm program)
      (is (every #'cl-quil.clifford::complex~
                 (qvm::amplitudes pure-qvm)
                 (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm)))))))

(deftest test-two-qubit-one-clifford ()
  (dotimes (i 100)
    (let* ((n 3)
           (pure-qvm (qvm::make-qvm n))
           (stab-qvm (qvm::make-stabilizer-qvm n))
           (program (qvm::random-clifford-program 1 2 n)))
      (test-quickrun pure-qvm program)
      (test-quickrun stab-qvm program)
      (is (every #'cl-quil.clifford::complex~
                 (qvm::amplitudes pure-qvm)
                 (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm)))))))

(deftest test-one-qubit-two-clifford ()
  (declare debug)
  (dotimes (i 100)
    (let* ((n 3)
           (pure-qvm (qvm::make-qvm n))
           (stab-qvm (qvm::make-stabilizer-qvm n))
           (program (qvm::random-clifford-program 2 1 n)))
      (test-quickrun pure-qvm program)
      (test-quickrun stab-qvm program)
      (is (every #'cl-quil.clifford::complex~
                 (qvm::amplitudes pure-qvm)
                 (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm)))))))

(deftest test-two-qubit-two-clifford ()
  (dotimes (i 100)
    (let* ((n 3)
           (pure-qvm (qvm::make-qvm n))
           (stab-qvm (qvm::make-stabilizer-qvm n))
           (program (qvm::random-clifford-program 2 2 n)))
      (test-quickrun pure-qvm program)
      (test-quickrun stab-qvm program)
      (is (every #'cl-quil.clifford::complex~
                 (qvm::amplitudes pure-qvm)
                 (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm)))))))

(deftest test-hella-random-cliffords ()
  (dotimes (i 100)
    (let* ((n 10)
           (pure-qvm (qvm::make-qvm n))
           (stab-qvm (qvm::make-stabilizer-qvm n))
           (program (qvm::random-clifford-program 10 3 n)))
      (test-quickrun pure-qvm program)
      (test-quickrun stab-qvm program)
      (is (every #'cl-quil.clifford::complex~
                 (qvm::amplitudes pure-qvm)
                 (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm)))))))

;; Doesn't really work because matrix-to-clifford seems broken
(deftest test-clifford-matrix-conversions ()
  (loop :for i :from 1 :to 4 :do
    (loop :repeat 100
          :for c := (cl-quil.clifford::random-clifford i)
          :for m := (cl-quil.clifford::clifford-to-matrix-v2 c)
          :for d := (cl-quil.clifford::matrix-to-clifford m)
          :do (is (cl-quil.clifford::clifford= c d)))))

(defparameter *cool-tests* '(test-zero-state test-bell-state test-nonclifford
                             test-random-small))
