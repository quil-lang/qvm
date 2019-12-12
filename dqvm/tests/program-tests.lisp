;;;; tests/run-tests.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2-tests)

(defun make-zero-ket-wavefunction (number-of-amplitudes)
  "Create zero-ket wavefunction."
  (make-array number-of-amplitudes
              :element-type 'qvm:cflonum
              :initial-contents (loop :for i :from 0 :below number-of-amplitudes
                                      :collect (qvm:cflonum (qvm:boolean-bit (zerop i))))))

(defun make-debug-wavefunction (number-of-amplitudes)
  "Create wavefunction whose amplitudes are consecutive numbers starting from zero."
  (make-array number-of-amplitudes
              :element-type 'qvm:cflonum
              :initial-contents (loop :for i :from 0 :below number-of-amplitudes
                                      :collect (qvm:cflonum i))))

(defun run-qvm (number-of-qubits parsed-program &optional initial-wavefunction)
  "Run PARSED-PROGRAM on a PURE-STATE-QVM with qubit size NUMBER-OF-QUBITS and return the wavefunction."
  (let* ((qvm:*compile-before-running* nil)
         (number-of-amplitudes (expt 2 number-of-qubits))
         (state (make-instance 'qvm:pure-state :num-qubits number-of-qubits
                                               :amplitudes (or initial-wavefunction
                                                               (make-debug-wavefunction number-of-amplitudes))))
         (qvm (make-instance 'qvm:pure-state-qvm
                             :number-of-qubits number-of-qubits
                             :state state)))
    (qvm:load-program qvm parsed-program)
    (qvm:run qvm)
    (qvm::amplitudes qvm)))

(defun run-dqvm (number-of-qubits parsed-program &optional initial-wavefunction)
  "Run PARSED-PROGRAM on a DISTRIBUTED-QVM with qubit size NUMBER-OF-QUBITS and return the wavefunction."
  (let* ((block-size (expt 2 (dqvm2::get-maximum-arity (quil:parsed-program-executable-code parsed-program))))
         (qvm (make-distributed-qvm :number-of-qubits number-of-qubits
                                    :number-of-processes 1 :rank 0
                                    :block-size block-size))
         (amplitudes (amplitudes qvm))
         (number-of-amplitudes (length amplitudes)))

    (let ((initial-wavefunction (or initial-wavefunction
                                    (make-debug-wavefunction number-of-amplitudes))))

      (dotimes (i number-of-amplitudes)
        (setf (aref amplitudes i) (aref initial-wavefunction i))))

    (qvm:load-program qvm parsed-program)
    (qvm:run qvm)

    ;; Reorganize amplitudes before returning.
    (let ((ordered-amplitudes (make-zero-ket-wavefunction number-of-amplitudes)))
      (setf (aref ordered-amplitudes 0) (qvm:cflonum 0))

      (dotimes (i number-of-amplitudes ordered-amplitudes)
        (let ((address (get-address-by-offset (addresses qvm) i)))
          (setf (aref ordered-amplitudes address) (aref amplitudes i)))))))

(deftest test-deterministic-programs ()
  (dqvm2::setup-logger)                 ; Placate format-LOG.

  (let ((number-of-qubits 4)
        (programs '("I 0"
                    "X 1"
                    "Y 2"
                    "Z 3"
                    "I 3; X 2; Y 1; Z 0"
                    "SWAP 0 1"
                    "SWAP 0 2"
                    "CNOT 0 1"
                    "CNOT 1 0"
                    "CNOT 0 2"
                    "CNOT 2 0"
                    "CNOT 1 2"
                    "CNOT 2 1"
                    "CNOT 0 3"
                    "CNOT 3 0"
                    "CNOT 1 3"
                    "CNOT 3 1"
                    "CNOT 2 3"
                    "CNOT 3 2"
                    "H 0; H 1; H 2; RESET 1; RESET 2"
                    "CCNOT 0 1 2"
                    "CCNOT 0 2 1"
                    "CCNOT 1 0 2"
                    "CCNOT 2 0 1"
                    "CCNOT 1 2 0"
                    "CCNOT 2 1 0"
                    "I 0; S 1"
                    "S 0; T 1"
                    "I 0; T 1"
                    "T 0; I 1"
                    "CZ 0 1"
                    "CZ 1 0"
                    "CZ 0 2"
                    "CZ 2 0"
                    "CZ 1 2"
                    "CZ 2 1"
                    "ISWAP 0 1"
                    "ISWAP 1 0"
                    "ISWAP 0 2"
                    "ISWAP 2 0"
                    "ISWAP 1 2"
                    "ISWAP 2 1"
                    "CCNOT 0 1 2; X 1; CCNOT 0 2 1; Y 2; CCNOT 1 0 2; H 0; CCNOT 2 0 1; Z 1; CCNOT 1 2 0; H 2; CCNOT 2 1 0")))

    (dolist (program programs)
      (let* ((parsed-program (quil:parse-quil program))
             (wf1 (run-qvm number-of-qubits parsed-program))
             (wf2 (run-dqvm number-of-qubits parsed-program)))
        (is (every #'quil::double= wf1 wf2))))))

(deftest test-non-deterministic-programs ()
  (dqvm2::setup-logger)                 ; Placate FORMAT-LOG.

  (let ((measure-result (run-dqvm 2 (quil:parse-quil "H 0; H 1; MEASURE 1")
                                  (make-zero-ket-wavefunction 4)))
        (array1 (make-array 4 :element-type 'qvm:cflonum
                              :initial-contents (loop :for i :from 0 :below 4
                                                      :collect (qvm:cflonum (if (< i 2)
                                                                                (/ 1 (sqrt 2))
                                                                                0)))))
        (array2 (make-array 4 :element-type 'qvm:cflonum
                              :initial-contents (loop :for i :from 0 :below 4
                                                      :collect (qvm:cflonum (if (<= 2 i)
                                                                                (/ 1 (sqrt 2))
                                                                                0))))))

    (is (alexandria:xor (every #'quil::double~ measure-result array1)
                        (every #'quil::double~ measure-result array2)))))
