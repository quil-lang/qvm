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

(deftest test-random-small ()
  (declare (optimize (speed 0) debug (space 0)))
  (dotimes (i 100)
    (let* ((n 2)
           (pure-qvm (qvm::make-qvm n))
           (stab-qvm (qvm::make-stabilizer-qvm n))
           (program (qvm::random-clifford-program 2 n n)))
      (test-quickrun pure-qvm program)
      (test-quickrun stab-qvm program)
      #+ignore(break "~A~2%~A~%" (qvm::amplitudes pure-qvm) (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm)))
      (is (every #'cl-quil.clifford::complex~
                 (qvm::amplitudes pure-qvm)
                 (cl-quil.clifford::tableau-wavefunction (qvm::stabilizer-qvm-tableau stab-qvm)))))))

(deftest test-clifford-matrix-conversions ()
  (loop :for i :from 1 :to 4 :do
    (loop :repeat 100
          :for c := (cl-quil.clifford::random-clifford i)
          :for m := (cl-quil.clifford::clifford-to-matrix-v2 c)
          :for d := (cl-quil.clifford::matrix-to-clifford m)
          :do (is (cl-quil.clifford::clifford= c d)))))

(defparameter *cool-tests* '(test-zero-state test-bell-state test-nonclifford
                             test-random-small))
