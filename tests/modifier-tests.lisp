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

(deftest test-forked-rx ()
  (with-execution-modes (:compile :interpret)
    (let* ((p-first-branch
             (with-output-to-quil "FORKED RX(0, pi) 0 1"))
           (p-second-branch
             (with-output-to-quil
               "X 0"
               "FORKED RX(0, pi) 0 1")))
      ;; Since q0 should be in the zero state, the FORKED should
      ;; produce RX(0) 1, hence leaving q1 in the zero state.
      (is (cflonum= 0 (qvm:probability (qvm::nth-amplitude (run-program 2 p-first-branch) 3))))
      ;; q1 is now in the one state, forked should produce RX(pi) 1,
      ;; leaving q1 in the one state.
      (is (cflonum= 1 (qvm:probability (qvm::nth-amplitude (run-program 2 p-second-branch) 3)))))))

(deftest test-forked-rx-with-controlled-and-dagger ()
  (with-execution-modes (:compile :interpret)
    (let* ((p-first-branch
             (with-output-to-quil "CONTROLLED DAGGER FORKED RX(0, pi) 0 1 2"))
           (p-second-branch
             (with-output-to-quil
               "X 0"
               "X 1"
               "CONTROLLED DAGGER FORKED RX(0, pi) 0 1 2")))
      ;; Since q0, q1 should be in the zero state, the FORKED should
      ;; produce RX(0) 2, hence leaving q2 in the zero state.
      (is (cflonum= 0 (qvm:probability (qvm::nth-amplitude (run-program 3 p-first-branch) 7))))
      ;; q0, q1 are now in the one state, forked should produce RX(pi) 2,
      ;; leaving q2 in the one state.
      (is (cflonum= 1 (qvm:probability (qvm::nth-amplitude (run-program 3 p-second-branch) 7)))))))

(deftest test-multiply-forked-rx ()
  ;; AKA the uniformly controlled rotation
  ;; TODO Cover all possibilities.
  (with-execution-modes (:compile :interpret)
    (let* ((p-first-branch
             (with-output-to-quil "FORKED FORKED RX(0, 0, 0, pi) 0 1 2"))
           (p-second-branch
             (with-output-to-quil
               "X 0"
               "X 1"
               "FORKED FORKED RX(0, 0, 0, pi) 0 1 2")))
      ;; Since q0, q1 should be in the zero state, the FORKED should
      ;; produce RX(0) 2, hence leaving q2 in the zero state.
      (is (every (alexandria:curry #'cflonum= 0)
                 (map-amplitudes (run-program 3 p-first-branch) #'qvm:probability)))
      ;; q0, q1 are now in the one state, forked should produce RX(pi) 2,
      ;; leaving q2 in the one state.
      (is (cflonum= 1 (qvm:probability (qvm::nth-amplitude (run-program 3 p-second-branch) 7)))))

    (let* ((p-first-branch
             (with-output-to-quil "FORKED FORKED RX(pi, 0, 0, pi) 0 1 2"))
           (p-second-branch
             (with-output-to-quil
               "X 0"
               "X 1"
               "FORKED FORKED RX(pi, 0, 0, pi) 0 1 2")))
      ;; Since q0, q1 should be in the zero state, the FORKED should
      ;; produce RX(pi) 2, hence leaving q2 in the one state.
      (is (cflonum= 1 (qvm:probability (qvm::nth-amplitude (run-program 3 p-first-branch) 4))))
      ;; q0, q1 are now in the one state, forked should produce RX(pi) 2,
      ;; leaving q2 in the one state.
      (is (cflonum= 1 (qvm:probability (qvm::nth-amplitude (run-program 3 p-second-branch) 7)))))))
