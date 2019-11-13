;;;; noisy-qvm-tests.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Nikolas Tezak

(in-package #:qvm-tests)

(defun check-noisy-pointer-is-correct (qvm)
  (when (typep qvm 'qvm:noisy-qvm)
    (is (or (not (qvm::requires-swapping-amps-p (qvm::state qvm)))
            (eq (qvm::amplitudes qvm)
                (qvm::original-amplitudes qvm))))))

(deftest test-empty-program-depolarizing-qvm ()
  (let ((p (with-output-to-quil))
        (q (make-instance 'qvm:depolarizing-qvm
                          :classical-memory-subsystem nil
                          :number-of-qubits 1
                          :x 1/3
                          :y 1/3
                          :z 1/3)))
    (qvm:load-program q p)
    (qvm:run q)
    (is (double-float= 0 (qubit-probability q 0)))))

(deftest test-simple-gate-noise ()
  "Test that the noisy gates behave as expected."
  (let ((p (with-output-to-quil
             "X 0"))
        (tries 500)
        (results-desired
          (list
           (make-vector 2 0 -1)
           (make-vector 2 1 0)
           (make-vector 2 (complex 0 -1) 0)))
        (qvm (make-instance 'qvm:depolarizing-qvm
                            :classical-memory-subsystem nil
                            :number-of-qubits 1
                            :x 1/3
                            :y 1/3
                            :z 1/3)))
    (qvm:load-program qvm p :supersede-memory-subsystem t)
    (qvm::reset-quantum-state qvm)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((psi (qvm::amplitudes (progn
                                            (qvm::reset-quantum-state qvm)
                                            (qvm:run qvm)))))
                (setf results-desired
                      (remove psi results-desired :test #'equalp))))
    (is (plusp tries))))

(deftest test-simple-measurement-noise ()
  "Test that the noisy gates behave as expected."
  (let* ((p (with-output-to-quil
              "DECLARE ro BIT"
              "MEASURE 0 ro[0]"))
         (tries 500)
         (results-desired (list 0 1))
         (qvm (make-instance 'qvm:depolarizing-qvm
                             :measure-x 1/5
                             :number-of-qubits 1
                             :classical-memory-subsystem nil)))
    (qvm:load-program qvm p :supersede-memory-subsystem t)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((result (qvm:memory-ref (progn
                                              (qvm::reset-quantum-state qvm)
                                              (qvm::reset-classical-memory qvm)
                                              (qvm:run qvm))
                                            "ro" 0)))
                (setf results-desired
                      (remove result results-desired :test #'=))))
    (is (plusp tries))))

(deftest test-empty-program-noisy-qvm ()
  (let ((p (with-output-to-quil))
        (q (make-instance 'qvm:noisy-qvm
                          :classical-memory-subsystem nil
                          :number-of-qubits 1)))
    (qvm:load-program q p)
    (qvm:run q)
    (check-noisy-pointer-is-correct q)
    (is (double-float= 0 (qubit-probability q 0)))))

(deftest test-noisy-x-gate ()
  "Test that the noisy gates behave as expected."
  (let ((p (with-output-to-quil
             "X 0"
             "X 1"))
        (tries 500)
        (results-desired
          (list
           (make-vector 4 0 0 0 1)
           (make-vector 4 0 0 1 0)
           (make-vector 4 0 0 (complex 0 -1) 0)))
        (qvm (make-instance 'qvm:noisy-qvm :classical-memory-subsystem nil
                                           :number-of-qubits 2)))
    (qvm:load-program qvm p :supersede-memory-subsystem t)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let ((psi (qvm::amplitudes (progn
                                            (qvm::reset-quantum-state qvm)
                                            (qvm:set-noisy-gate qvm "X" '(0) (qvm::make-pauli-perturbed-1q-gate "X" 1/4 1/4 1/4))
                                            (prog1 (qvm:run qvm)
                                              (check-noisy-pointer-is-correct qvm))))))
                (setf results-desired
                      (remove psi results-desired :test #'equalp))))
    (is (plusp tries))))

(defun test-noisy-readout-2q-qvm (qvm)
  "Test that noisy readout behaves as expected on the given QVM (having at least 2 qubits)."
  (let* ((p (with-output-to-quil
              "DECLARE ro BIT[2]"
              "MEASURE 0 ro[0]"
              "MEASURE 1 ro[1]"))
         (tries 500)
         (results-desired '(1 0)))
    (qvm:load-program qvm p :supersede-memory-subsystem t)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let* ((qvm-final (progn
                                  (qvm::reset-quantum-state qvm)
                                  (qvm::set-readout-povm qvm 1 '(0.8d0 0.1d0
                                                                 0.2d0 0.9d0))
                                  (prog1 (qvm:run qvm)
                                    (check-noisy-pointer-is-correct qvm))))
                    (a (qvm:memory-ref qvm-final "ro" 0))
                    (b (qvm:memory-ref qvm-final "ro" 1)))
                (is (zerop a))
                (setf results-desired
                      (remove b results-desired :test #'=))))
    ;; For this POVM, we should not have exhausted all 500 tries in
    ;; any realistic situation.
    (is (plusp tries))))

(deftest test-noisy-qvm-noisy-readout ()
  "Test that the noisy readout behaves as expected."
  (test-noisy-readout-2q-qvm
   (make-instance 'qvm:noisy-qvm :number-of-qubits 2
                                 :classical-memory-subsystem nil)))

(deftest test-noisy-measure-all ()
  "Test that MEASURE-ALL works correctly for noisy readout"
  (let ((p (with-output-to-quil
             "X 0"
             "X 1"))
        (tries 500)
        (results-desired '((1 1)
                           (1 0)))
        (qvm (make-instance 'qvm:noisy-qvm :number-of-qubits 2
                                           :classical-memory-subsystem nil)))
    (qvm:load-program qvm p :supersede-memory-subsystem t)
    (loop :while (and (plusp (length results-desired))
                      (plusp tries))
          :do (decf tries)
              (let* ((qvm-final (progn
                                  (qvm::reset-quantum-state qvm)
                                  (qvm::set-readout-povm qvm 1 '(0.8d0 0.1d0
                                                                 0.2d0 0.9d0))
                                  (prog1 (qvm:run qvm)
                                    (check-noisy-pointer-is-correct qvm)))))
                (multiple-value-bind (qvm-final measured-bits)
                    (measure-all qvm-final)
                  (declare (ignore qvm-final))
                  (setf results-desired
                        (remove measured-bits results-desired :test #'equalp)))))
    (is (plusp tries))))
