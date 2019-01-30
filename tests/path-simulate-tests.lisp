;;;; path-simulate-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

;;; This file contains tests of the MADPI simulation technique.

(defun test-path-eval-instance (s)
  (let* ((p (quil:parse-quil-string (format nil s)))
         (n (quil:qubits-needed p))
         (qvm (make-qvm n)))
    (load-program qvm p)
    (run qvm)
    (is (every #'cflonum=
               ;; Path simulate
               (wavefunction-from-path-simulation p)
               ;; QVM result
               (qvm::amplitudes qvm)))
    nil))

(deftest test-madpi ()
  "Test the MADPI technique for program simulation."
  (mapc #'test-path-eval-instance
        '("X 0"
          "X 1"
          "X 0~%X 1"
          "H 0~%CNOT 0 1"
          "H 0~%H 1~%H 2~%CSWAP 0 1 2"
          "H 1~%CNOT 0 1~%X 0~%CCNOT 0 1 2~%CPHASE(0.5) 0 2"))
  nil)
