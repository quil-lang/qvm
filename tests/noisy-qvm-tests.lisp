;;;; noisy-qvm-tests.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Nikolas Tezak

(in-package #:qvm-tests)

(deftest test-simple-gate-noise ()
  "Test that the noisy gates behave as expected."
  (let ((p (with-output-to-quil
             (write-line "X 0")))
        (tries 500)
        (results-desired
          (list
           (qvm::make-vector 2 0 -1)
           (qvm::make-vector 2 1 0)
           (qvm::make-vector 2 (complex 0 -1) 0)))
        (qvm (make-instance 'qvm:depolarizing-qvm
                            :x 1/3
                            :y 1/3
                            :z 1/3
                            :number-of-qubits 1
                            :classical-memory-size 64)))
    (qvm:load-program qvm p)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((psi (qvm::amplitudes (progn
                                            (qvm::reset qvm)
                                            (qvm:run qvm)))))
                (setf results-desired
                      (remove psi results-desired :test #'equalp))))
    (is (plusp tries))))

(deftest test-simple-measurement-noise ()
  "Test that the noisy gates behave as expected."
  (let ((p (with-output-to-quil
             (write-line "MEASURE 0 [0]")))
        (tries 500)
        (results-desired (list 0 1))
        (qvm (make-instance 'qvm:depolarizing-qvm
                            :measure-x 1/5
                            :number-of-qubits 1
                            :classical-memory-size 64)))
    (qvm:load-program qvm p)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((result (qvm::classical-bit (progn
                                                  (qvm::reset qvm)
                                                  (qvm::reset-classical-memory qvm)
                                                  (qvm:run qvm))
                                                0)))
                (setf results-desired
                      (remove result results-desired :test #'=))))
    (is (plusp tries))))


(deftest test-noisy-x-gate ()
  "Test that the noisy gates behave as expected."
  (let ((p (with-output-to-quil
             (write-line "X 0")
             (write-line "X 1")))
        (tries 500)
        (results-desired
          (list
           (qvm::make-vector 4 0 0 0 1)
           (qvm::make-vector 4 0 0 1 0)
           (qvm::make-vector 4 0 0 (complex 0 -1) 0)))
        (qvm (make-instance 'qvm:noisy-qvm :number-of-qubits 2
                                           :classical-memory-size 64)))
    (qvm:load-program qvm p)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((psi (qvm::amplitudes (progn
                                            (qvm::reset qvm)
                                            (qvm:set-noisy-gate qvm "X" '(0) (qvm::make-pauli-perturbed-1q-gate "X" 1/4 1/4 1/4))
                                            (qvm:run qvm)))))
                (setf results-desired
                      (remove psi results-desired :test #'equalp))))
    (is (plusp tries))))

(deftest test-noisy-readout ()
  "Test that the noisy readout behaves as expected."
  (let ((p (with-output-to-quil
             (write-line "MEASURE 0 [0]")
             (write-line "MEASURE 1 [1]")))
        (tries 500)
        (results-desired '(1 0))
        (qvm (make-instance 'qvm:noisy-qvm :number-of-qubits 2
                                           :classical-memory-size 64)))
    (qvm:load-program qvm p)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let* ((qvm-final (progn
                                  (qvm::reset qvm)
                                  (qvm::set-readout-povm qvm 1 '(0.8d0 0.1d0
                                                                0.2d0 0.9d0))
                                  (qvm:run qvm)))
                    (a (qvm::classical-bit qvm-final 0))
                    (b (qvm::classical-bit qvm-final 1)))
                (is (= a 0))
                (setf results-desired
                      (remove b results-desired :test #'eq))))
    (is (plusp tries))))

(deftest test-noisy-measure-all ()
  "Test that MEASURE-ALL works correctly for noisy readout"
  (let ((p (with-output-to-quil
             (write-line "X 0")
             (write-line "X 1")))
        (tries 500)
        (results-desired '((1 1)
                           (1 0)))
        (qvm (make-instance 'qvm:noisy-qvm :number-of-qubits 2
                                           :classical-memory-size 64)))
    (qvm:load-program qvm p)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let* ((qvm-final (progn
                                  (qvm::reset qvm)
                                  (qvm::set-readout-povm qvm 1 '(0.8d0 0.1d0
                                                                 0.2d0 0.9d0))
                                  (qvm:run qvm))))
                (multiple-value-bind (qvm-final measured-bits)
                    (measure-all qvm-final)
                  (declare (ignore qvm-final))
                  (setf results-desired
                        (remove measured-bits results-desired :test #'equalp)))))
    (is (plusp tries))))
