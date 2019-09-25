;;;; examples/vqe.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:qvm-examples)

;;; Example of the usage of the variational quantum eigensolver algorithm to
;;; compute the ground state energy of an H2 molecule.  The hard-coded values
;;; below are for a conformation where the inter-atomic distance is 0.5
;;; Angstroms. The reference value for the ground energy at this conformation
;;; is -1.05515979 Hartree.

;;; The reference state, ansatz, and Hamiltonian used below were generated
;;; using PyQuil, OpenFermion, PySCF, and the Qu & Co Chemistry library.

(defparameter *number-of-qubits* 4
  "Number of qubits in the quantum virtual machine")

(defparameter *reference-state* "X 0; X 1" "Reference state program (HF).")

(defparameter *ansatz* "
RX(pi/2) 0
H 2
CNOT 0 1
CNOT 1 2
RZ(-1.0*theta[0]) 2
CNOT 1 2
CNOT 0 1
RX(-pi/2) 0
H 2
H 0
RX(pi/2) 2
CNOT 0 1
CNOT 1 2
RZ(1.0*theta[0]) 2
CNOT 1 2
CNOT 0 1
H 0
RX(-pi/2) 2
RX(pi/2) 0
RX(pi/2) 1
RX(pi/2) 2
H 3
CNOT 0 1
CNOT 1 2
CNOT 2 3
RZ(-0.5*theta[1]) 3
CNOT 2 3
CNOT 1 2
CNOT 0 1
RX(-pi/2) 0
RX(-pi/2) 1
RX(-pi/2) 2
H 3
H 0
RX(pi/2) 1
H 2
H 3
CNOT 0 1
CNOT 1 2
CNOT 2 3
RZ(-0.5*theta[1]) 3
CNOT 2 3
CNOT 1 2
CNOT 0 1
H 0
RX(-pi/2) 1
H 2
H 3
RX(pi/2) 0
H 1
RX(pi/2) 2
RX(pi/2) 3
CNOT 0 1
CNOT 1 2
CNOT 2 3
RZ(0.5*theta[1]) 3
CNOT 2 3
CNOT 1 2
CNOT 0 1
RX(-pi/2) 0
H 1
RX(-pi/2) 2
RX(-pi/2) 3
H 0
H 1
H 2
RX(pi/2) 3
CNOT 0 1
CNOT 1 2
CNOT 2 3
RZ(0.5*theta[1]) 3
CNOT 2 3
CNOT 1 2
CNOT 0 1
H 0
H 1
H 2
RX(-pi/2) 3
H 0
H 1
RX(pi/2) 2
H 3
CNOT 0 1
CNOT 1 2
CNOT 2 3
RZ(0.5*theta[1]) 3
CNOT 2 3
CNOT 1 2
CNOT 0 1
H 0
H 1
RX(-pi/2) 2
H 3
RX(pi/2) 0
H 1
H 2
H 3
CNOT 0 1
CNOT 1 2
CNOT 2 3
RZ(-0.5*theta[1]) 3
CNOT 2 3
CNOT 1 2
CNOT 0 1
RX(-pi/2) 0
H 1
H 2
H 3
H 0
RX(pi/2) 1
RX(pi/2) 2
RX(pi/2) 3
CNOT 0 1
CNOT 1 2
CNOT 2 3
RZ(0.5*theta[1]) 3
CNOT 2 3
CNOT 1 2
CNOT 0 1
H 0
RX(-pi/2) 1
RX(-pi/2) 2
RX(-pi/2) 3
RX(pi/2) 0
RX(pi/2) 1
H 2
RX(pi/2) 3
CNOT 0 1
CNOT 1 2
CNOT 2 3
RZ(-0.5*theta[1]) 3
CNOT 2 3
CNOT 1 2
CNOT 0 1
RX(-pi/2) 0
RX(-pi/2) 1
H 2
RX(-pi/2) 3
RX(pi/2) 1
H 3
CNOT 1 2
CNOT 2 3
RZ(-1.0*theta[0]) 3
CNOT 2 3
CNOT 1 2
RX(-pi/2) 1
H 3
H 1
RX(pi/2) 3
CNOT 1 2
CNOT 2 3
RZ(1.0*theta[0]) 3
CNOT 2 3
CNOT 1 2
H 1
RX(-pi/2) 3
"
  "Parametric UCCSD ansatz.")

(defparameter *pauli-coefficients* '(0.37983135178095495d0
                                     0.2139353102452124d0
                                     0.2139353102452124d0
                                    -0.36914431524376695d0
                                    -0.36914431524376695d0
                                     0.17992650976405983d0
                                     0.042217556922433896d0
                                    -0.042217556922433896d0
                                    -0.042217556922433896d0
                                     0.042217556922433896d0
                                     0.13459240346368873d0
                                     0.17680996038612262d0
                                     0.17680996038612262d0
                                     0.13459240346368873d0
                                     0.18620984259247159d0)
  "Coefficients of the Pauli operators present in the Hamiltonian.")

(defparameter *pauli-terms* (mapcar #'quil:parse-quil '("I 0"
                                                        "Z 0"
                                                        "Z 1"
                                                        "Z 2"
                                                        "Z 3"
                                                        "Z 0; Z 1"
                                                        "Y 0; X 1; X 2; Y 3"
                                                        "X 0; X 1; Y 2; Y 3"
                                                        "Y 0; Y 1; X 2; X 3"
                                                        "X 0; Y 1; Y 2; X 3"
                                                        "Z 0; Z 2"
                                                        "Z 0; Z 3"
                                                        "Z 1; Z 2"
                                                        "Z 1; Z 3"
                                                        "Z 2; Z 3"))
  "Pauli operators in the Hamiltonian.")

(defparameter *initial-thetas* #(0.0d0 -0.036014483d0)
  "Initial guess for the classical optimization method.")

(defun run-vqe (&key (number-of-qubits *number-of-qubits*)
                  (initial-thetas *initial-thetas*)
                  (reference-state *reference-state*)
                  (ansatz *ansatz*)
                  (pauli-coefficients *pauli-coefficients*)
                  (pauli-terms *pauli-terms*))
  "Run the variational quantum eigensolver algorithm."
  (flet ((objective-function (thetas)
           "Compute the energy based on the values of THETAS."
           (loop :with state-prep := (quil:parse-quil
                                      (format nil "DECLARE theta REAL[2]~%~
                                                   MOVE theta[0] ~F~%~
                                                   MOVE theta[1] ~F~%~A~A"
                                              (elt thetas 0) (elt thetas 1)
                                              reference-state ansatz))
                 :with expectations := (qvm-app::perform-expectation 'qvm-app::pure-state state-prep pauli-terms number-of-qubits)
                 :for u :in pauli-coefficients :for v :in expectations :sum (* u v))))

    (cl-grnm:grnm-optimize #'objective-function initial-thetas)))
