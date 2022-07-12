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
              :collect (make-instance 'cl-quil:gate-application
                                      :operator #.(cl-quil:named-operator "SWAP")
                                      :name-resolution (cl-quil:lookup-standard-gate "SWAP")
                                      :arguments (list (cl-quil:qubit qs)
                                                       (cl-quil:qubit qe)))))))

(defun qft-circuit (qubits)
  "Generate the QFT circuit on the given qubits."
  (labels ((qft (qubits)
             (destructuring-bind (q . qs) qubits
               (if (null qs)
                   (list (make-instance 'cl-quil:gate-application
                                        :operator #. (cl-quil:named-operator "H")
                                        :name-resolution (cl-quil:lookup-standard-gate "H")
                                        :arguments (list (cl-quil:qubit q))))
                   (let ((cR nil))
                     (loop :with n := (1+ (length qs))
                           :for i :from (1- n) :downto 1
                           :for qi :in qs
                           :for angle := (qvm:flonum (/ pi (expt 2 (- n i))))
                           :do (push (make-instance
                                      'cl-quil:gate-application
                                      :operator #.(cl-quil:named-operator "CPHASE")
                                      :name-resolution (cl-quil:lookup-standard-gate "CPHASE")
                                      :parameters (list (cl-quil:constant angle))
                                      :arguments (list (cl-quil:qubit q)
                                                       (cl-quil:qubit qi)))
                                     cR))
                     (append
                      (qft qs)
                      cR
                      (list (make-instance 'cl-quil:gate-application
                                           :operator #. (cl-quil:named-operator "H")
                                           :name-resolution (cl-quil:lookup-standard-gate "H")
                                           :arguments (list (cl-quil:qubit q))))))))))
    (make-instance 'cl-quil:parsed-program
                   :gate-definitions nil
                   :circuit-definitions nil
                   :executable-code
                   (concatenate
                    'vector
                    ;; Core QFT with normalization.
                    (qft qubits)

                    ;; Re-ordering the output.
                    (bit-reversal-circuit qubits)))))
