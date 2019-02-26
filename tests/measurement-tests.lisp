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
          :do (is (= address (qvm::index-to-address index i 0))))))

(deftest test-simple-measurements ()
  "Test that some simple measurements work."
  (with-execution-modes (:compile :interpret)
    (let ((p (with-output-to-quil
               "DECLARE ro BIT[3]"
               "X 0"
               "X 2"
               "MEASURE 0 ro[0]"
               "MEASURE 1 ro[1]"
               "MEASURE 2 ro[2]")))
      (let ((qvm (qvm:run-program (cl-quil:qubits-needed p) p)))
        (is (= 1 (qvm:memory-ref qvm "ro" 0)))
        (is (= 0 (qvm:memory-ref qvm "ro" 1)))
        (is (= 1 (qvm:memory-ref qvm "ro" 2)))))))

(deftest test-simple-measurements-with-swap ()
  "Test that some simple measurements work with SWAP."
  (with-execution-modes (:compile :interpret)
    (let ((p (with-output-to-quil
               "DECLARE ro BIT[3]"
               "X 0"
               "X 2"
               "SWAP 0 1"
               "SWAP 0 2"
               "MEASURE 0 ro[0]"
               "MEASURE 1 ro[1]"
               "MEASURE 2 ro[2]")))
      (let ((qvm (qvm:run-program (cl-quil:qubits-needed p) p)))
        (is (= 1 (qvm:memory-ref qvm "ro" 0)))
        (is (= 1 (qvm:memory-ref qvm "ro" 1)))
        (is (= 0 (qvm:memory-ref qvm "ro" 2)))))))

(deftest test-unit-wavefunction-after-measurements ()
  "Test that the wavefunction is of length nearly 1 after measurements."
  (with-execution-modes (:compile :interpret)
    (let ((progs (loop :for i :from 5 :to 15
                       :collect (with-output-to-quil
                                  (loop :for q :below i
                                        :do (format t "H ~D~%MEASURE ~D~%" q q))))))
      (loop :for p :in progs
            :for q := (qvm:run-program (cl-quil:qubits-needed p) p)
            :do (is (double-float= 1 (sqrt (reduce #'+ (qvm::amplitudes q) :key #'probability))))))))

(defun test-range (program repetitions percent-zeros percent-ones &key (tolerance 0.05))
  (let ((q (qvm:make-qvm (cl-quil:qubits-needed program)
                         :classical-memory-model
                         (qvm::memory-descriptors-to-qvm-memory-model
                          (quil:parsed-program-memory-definitions program))))
        (counts (vector 0 0)))
    (qvm:load-program q program)
    (loop :repeat repetitions
          :do (qvm::reset-classical-memory q)
              (qvm::reset-quantum-state q)
              (qvm:run q)
              (incf (aref counts (qvm:memory-ref q "ro" 0))))
    (let ((got-percent-zeros (float (/ (aref counts 0) repetitions)))
          (got-percent-ones  (float (/ (aref counts 1) repetitions))))
      (is (<= (max 0.0 (- percent-zeros tolerance))
              got-percent-zeros
              (min 1.0 (+ percent-zeros tolerance))))
      (is (<= (max 0.0 (- percent-ones tolerance))
              got-percent-ones
              (min 1.0 (+ percent-ones tolerance)))))))

(deftest test-hadamard-measurements ()
  (with-execution-modes (:compile :interpret)
    (let ((n 100000)
          (p (with-output-to-quil
               "DECLARE ro BIT"
               "I 1"
               "H 0"
               "MEASURE 0 ro[0]"
               "RESET")))
      (test-range p n 0.5 0.5))))

(deftest test-quarter-rotation-measurements ()
  (with-execution-modes (:compile :interpret)
    (let* ((n 100000)
           (one (expt (sin (/ pi 4 2)) 2))
           (zero (- 1 one))
           (p (with-output-to-quil
                "DECLARE ro BIT"
                "I 1"
                "RX(pi/4) 0"
                "MEASURE 0 ro[0]"
                "RESET")))
      (test-range p n zero one))))

(deftest test-three-quarter-rotation-measurements ()
  (with-execution-modes (:compile :interpret)
    (let* ((n 100000)
           (zero (expt (sin (/ pi 4 2)) 2))
           (one (- 1 zero))
           (p (with-output-to-quil
                "DECLARE ro BIT"
                "I 1"
                "RX(3*pi/4) 0"
                "MEASURE 0 ro[0]"
                "RESET")))
      (test-range p n zero one))))

(deftest test-eighth-rotation-measurements ()
  (with-execution-modes (:compile :interpret)
    (let* ((n 100000)
           (one (expt (sin (/ pi 8 2)) 2))
           (zero (- 1 one))
           (p (with-output-to-quil
                "DECLARE ro BIT"
                "I 1"
                "RX(pi/8) 0"
                "MEASURE 0 ro[0]"
                "RESET")))
      (test-range p n zero one))))

(deftest test-sample-wavefunction ()
  "Test that we can sample from a wavefunction correctly."
  (flet ((test-vector (&rest args)
           (apply #'make-vector
                  (length args)
                  (mapcar #'sqrt args))))
    ;; Simple uniform test.
    (let ((v (test-vector 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1)))
      (loop :for i :below (length v)
            :for p :from 0.0d0 :by 0.1d0
            :do (is (= i (qvm::sample-wavefunction-as-distribution-in-parallel-truly v p))))
      (is (= 1 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.11d0)))
      (is (= 1 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.15d0)))
      (is (= 7 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 1.0d0))))

    ;; Slightly more complicated test.
    (let ((v (test-vector 0.0 0.0 0.0 0.1 0.5 0.2 0.1 0.1)))
      (is (= 3 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.0d0)))
      (is (= 3 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.09999d0)))
      (is (= 4 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.1d0)))
      (is (= 4 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.10001d0))))

    ;; A 1q case.
    (let ((v (test-vector 0.2 0.8)))
      (is (= 0 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.0d0)))
      (is (= 0 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.1d0)))
      (is (= 1 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.2d0)))
      (is (= 1 (qvm::sample-wavefunction-as-distribution-in-parallel-truly v 0.8d0))))))

(deftest test-measure-all ()
  "Test that we can measure all qubits successfully in a few cases."
  (with-execution-modes (:compile :interpret)
    (let ((px (list
               (with-output-to-quil "I 0" "I 1" "I 2")
               (with-output-to-quil "X 0" "I 1" "I 2")
               (with-output-to-quil "I 0" "X 1" "I 2")
               (with-output-to-quil "X 0" "X 1" "I 2")
               (with-output-to-quil "I 0" "I 1" "X 2")
               (with-output-to-quil "X 0" "I 1" "X 2")
               (with-output-to-quil "I 0" "X 1" "X 2")
               (with-output-to-quil "X 0" "X 1" "X 2"))))
      (loop :for i :from 0
            :for p :in px
            :for bits := (list (ldb (byte 1 0) i)
                               (ldb (byte 1 1) i)
                               (ldb (byte 1 2) i))
            :do (let ((qvm (make-qvm 3)))
                  (qvm:load-program qvm p)
                  (qvm:run qvm)
                  (multiple-value-bind (qvm measured-bits)
                      (qvm:measure-all qvm)
                    (is (every #'= bits measured-bits))
                    (is (= 1 (qvm::nth-amplitude qvm i)))))))))

(deftest test-out-of-bounds-measurement ()
  "Test that measuring a qubit out-of-bounds is an error."
  (signals error (qvm:measure (qvm:make-qvm 1) -1))
  (signals error (qvm:measure (qvm:make-qvm 1) 1))
  (not-signals error (qvm:measure (qvm:make-qvm 1) 0)))
