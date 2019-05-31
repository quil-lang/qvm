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
CNOT 0 1")))
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

(deftest test-random-small ()
  (let ((pure-qvm (qvm::make-qvm 5))
        (stab-qvm (qvm::make-stabilizer-qvm 5))
        (program (qvm::random-clifford-program 10 2 4)))
    (test-quickrun pure-qvm program)
    (test-quickrun stab-qvm program)
    (is (every #'cl-quil.clifford::complex~
               (qvm::amplitudes pure-qvm)
               (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm))))))

(defparameter *cool-tests* '(test-zero-state test-bell-state test-nonclifford
                             test-random-small))
