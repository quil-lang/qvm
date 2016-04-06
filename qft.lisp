;;;; qft.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; Circuit generators for the Quantum Fourier Transform.

(defun bit-reversal-circuit (qubits)
  "Create a circuit which does a bit reversal on the amplitude indexes."
  (let ((n (length qubits)))
    (if (< n 2)
        nil
        (loop :for i :below (floor n 2)
              :for qs :in qubits
              :for qe :in (reverse qubits)
              :collect `(swap ,qs ,qe)))))

(defun qft-circuit (qubits)
  "Generate the QFT circuit on the given qubits."
  (labels ((qft (qubits)
             (destructuring-bind (q . qs) qubits
               (if (null qs)
                   (list `(HADAMARD ,q))
                   (let ((cR nil))
                     (loop :with n := (1+ (length qs))
                           :for i :from (1- n) :downto 1
                           :for qi :in qs
                           :do (push `(CPHASE ,q ,qi ,(/ pi (expt 2 (- n i)))) cR))
                     (append
                      (qft qs)
                      cR
                      (list `(HADAMARD ,q))))))))
    (append
     ;; Core QFT with normalization.
     (qft qubits)

     ;; Re-ordering the output.
     (bit-reversal-circuit qubits))))
