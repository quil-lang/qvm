;;;; modifier-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-controlled-x ()
  "Test that a CONTROLLED modifier seems to work in a common case."
  (with-execution-modes (:compile :interpret)
    (let* ((p-normal (with-output-to-quil
                       "H 0"
                       "CNOT 0 1"))
           (p-modified (with-output-to-quil
                         "H 0"
                         "CONTROLLED X 0 1"))
           (q-normal (run-program 2 p-normal))
           (q-modified (run-program 2 p-modified)))
      (is (every #'cflonum=
                 (qvm::amplitudes q-normal)
                 (qvm::amplitudes q-modified))))))

(deftest test-controlled-controlled-x ()
  "Test that a CONTROLLED modifier seems to work in a common case."
  (with-execution-modes (:compile :interpret)
    (let* ((p-normal (with-output-to-quil
                       "H 0"
                       "H 1"
                       "H 2"
                       "CCNOT 0 1 2"))
           (p-modified (with-output-to-quil
                         "H 0"
                         "H 1"
                         "H 2"
                         "CONTROLLED CONTROLLED X 0 1 2"))
           (q-normal (run-program 3 p-normal))
           (q-modified (run-program 3 p-modified)))
      (is (every #'cflonum=
                 (qvm::amplitudes q-normal)
                 (qvm::amplitudes q-modified))))))


(deftest test-dagger-inversion ()
  "Test that DAGGER modifiers seem to work in a case where we want to reverse a circuit."
  (with-execution-modes (:compile :interpret)
    (let* ((p (with-output-to-quil
                ;; Start
                "H 0"
                "CONTROLLED RY(2*pi/3) 0 1"
                "RX(pi/3) 0"
                "T 2"
                "CSWAP 0 1 2"
                ;; Invert
                "DAGGER CSWAP 0 1 2"
                "DAGGER T 2"
                "DAGGER RX(pi/3) 0"
                "DAGGER CONTROLLED RY(2*pi/3) 0 1"
                "DAGGER H 0"))
           (q (run-program 3 p)))
      (is (cflonum= 1 (qvm::nth-amplitude q 0)))
      (loop :for i :from 1 :below (expt 2 3) :do
        (is (cflonum= 0 (qvm::nth-amplitude q i)))))))
