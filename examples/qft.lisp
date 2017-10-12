;;;; examples/qft.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-examples)

;;;; Circuit generators for the Quantum Fourier Transform.

(defun bit-reversal-circuit (qubits)
  "Create a circuit which does a bit reversal on the amplitude indexes."
  (let ((n (length qubits)))
    (if (< n 2)
        nil
        (loop :for i :below (floor n 2)
              :for qs :in qubits
              :for qe :in (reverse qubits)
              :collect (make-instance 'quil:gate-application
                                      :operator "SWAP"
                                      :arguments (list (quil:qubit qs)
                                                       (quil:qubit qe)))))))

(defun qft-circuit (qubits)
  "Generate the QFT circuit on the given qubits."
  (labels ((qft (qubits)
             (destructuring-bind (q . qs) qubits
               (if (null qs)
                   (list (make-instance 'quil:gate-application
                                        :operator "H"
                                        :arguments (list (quil:qubit q))))
                   (let ((cR nil))
                     (loop :with n := (1+ (length qs))
                           :for i :from (1- n) :downto 1
                           :for qi :in qs
                           :for angle := (qvm:flonum (/ pi (expt 2 (- n i))))
                           :do (push (make-instance
                                      'quil:gate-application
                                      :operator "CPHASE"
                                      :parameters (list (quil:constant angle))
                                      :arguments (list (quil:qubit q)
                                                       (quil:qubit qi)))
                                     cR))
                     (append
                      (qft qs)
                      cR
                      (list (make-instance 'quil:gate-application
                                           :operator "H"
                                           :arguments (list (quil:qubit q))))))))))
    (make-instance 'quil:parsed-program
                   :gate-definitions nil
                   :circuit-definitions nil
                   :executable-code
                   (concatenate
                    'vector
                    ;; Core QFT with normalization.
                    (qft qubits)

                    ;; Re-ordering the output.
                    (bit-reversal-circuit qubits)))))
