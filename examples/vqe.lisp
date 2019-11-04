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

(defparameter *reference-state* "
DEFCIRCUIT REFERENCE:
    X 0
    X 1
"
  "Reference state program (HF).")

(defparameter *ansatz* "
DEFCIRCUIT ANSATZ:
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

(defparameter *coefficients* '#.(mapcar (lambda (x) (coerce x 'qvm:flonum))
                                        '(0.37983135178095495d0
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
                                          0.18620984259247159d0))
              "Coefficients of the Pauli operators present in the Hamiltonian.")

(defparameter *operators* (mapcar #'quil:parse-quil '("I 0"
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

(defun error-missing-initform (initarg)
  (error "The value of ~S must be specified" initarg))

(defclass hamiltonian ()
  ((coefficients
    :type list
    :initarg :coefficients
    :initform (error-missing-initform :coefficients)
    :documentation "Coefficients of the operators in the Hamiltonian.")
   (operators
    :type list
    :initarg :operators
    :initform (error-missing-initform :operators)
    :documentation "Operators in the Hamiltonian."))
  (:documentation "Hamiltonian function."))

(defun make-extended-hamiltonian (hamiltonian coefficient operator)
  "Return a new HAMILTONIAN with an extra term of the form COEFFICIENT times OPERATOR."
  (declare (type hamiltonian hamiltonian)
           (type qvm:flonum coefficient)
           (type quil:parsed-program operator operator))
  (make-instance 'hamiltonian
                 :coefficients (cons coefficient (slot-value hamiltonian 'coefficients))
                 :operators (cons operator (slot-value hamiltonian 'operators))))

(defclass vqe-problem ()
  ((number-of-qubits
    :type alexandria:non-negative-fixnum
    :initarg :number-of-qubits
    :initform (error-missing-initform :number-of-qubits)
    :documentation "Number of qubits.")
   (reference-state
    :type string
    :initarg :reference-state
    :initform (error-missing-initform :reference-state)
    :documentation "Reference state. A string with a Quil program containing a DEFCIRCUIT form named REFERENCE.")
   (ansatz
    :type string
    :initarg :ansatz
    :initform (error-missing-initform :ansatz)
    :documentation "Ansatz for the eigenproblem. A string with a Quil program containing a DEFCIRCUIT form named ANSATZ.")
   (hamiltonian
    :type hamiltonian
    :initarg :hamiltonian
    :initform (error-missing-initform :hamiltonian)
    :documentation "The operators in the Hamiltonian and their coefficients."))
  (:documentation "Auxiliary class for eigenproblems solved by VQE."))

(defun make-vqe-problem (&key (number-of-qubits *number-of-qubits*)
                           (reference-state *reference-state*)
                           (ansatz *ansatz*)
                           (coefficients *coefficients*)
                           (operators *operators*))
  (let ((hamiltonian (make-instance 'hamiltonian
                                    :coefficients (copy-seq coefficients)
                                    :operators (copy-seq operators))))
    (make-instance 'vqe-problem
                   :number-of-qubits number-of-qubits
                   :reference-state reference-state
                   :ansatz ansatz
                   :hamiltonian hamiltonian)))

(defun make-ansatz-string (thetas &key dagger)
  "Return Quil code that evaluates the values of the sequence THETAS and instantiates an ansatz (or its Hermitian conjugate, if DAGGER is T)."
  (with-output-to-string (stream)
    (loop :for i :below (length thetas)
          :for theta := (elt thetas i)
          :do (format stream "MOVE theta[~d] ~F~%" i theta))
    (format stream "~:[REFERENCE~%ANSATZ~;DAGGER ANSATZ~%DAGGER REFERENCE~]~%" dagger)))

(defun solve-vqe-problem (vqe-problem &optional (initial-thetas *initial-thetas*))
  "Run the variational quantum eigensolver algorithm."
  (let* ((number-of-qubits (slot-value vqe-problem 'number-of-qubits))
         (reference-state (slot-value vqe-problem 'reference-state))
         (ansatz (slot-value vqe-problem 'ansatz))
         (hamiltonian (slot-value vqe-problem 'hamiltonian))
         (operators (slot-value hamiltonian 'operators))
         (coefficients (slot-value hamiltonian 'coefficients)))

    (flet ((objective-function (thetas)
             "Compute the energy based on the values of THETAS."
             (loop :with quil := (format nil "DECLARE theta REAL[2]~%~A~%~A~%~A~%" reference-state ansatz (make-ansatz-string thetas))
                   :with state-prep := (quil:parse-quil quil)
                   :with expectations := (qvm-app::perform-expectation 'qvm-app::pure-state state-prep operators number-of-qubits)
                   :for u :of-type qvm:flonum :in coefficients
                   :for v :of-type qvm:flonum :in expectations
                   :sum (* u v) :of-type qvm:flonum)))
      (cl-grnm:grnm-optimize #'objective-function initial-thetas))))

(defun make-penalty-term (vqe-problem thetas-1 thetas-2 &key dagger)
  "Return a parsed program resulting from concatenating the ansatz characterized by THETAS-2 (possibly taking its Hermitian conjugate, depending on the value of DAGGER) to the ansatz determined by THETAS-1.

In other words, we return a program implementing the operator A(θ₂)† A(θ₁)."
  (let ((quil (format nil "DECLARE theta REAL[2]~%~A~%~A~%~A~%~A~%"
                      (slot-value vqe-problem 'reference-state)
                      (slot-value vqe-problem 'ansatz)
                      (make-ansatz-string thetas-1)
                      (make-ansatz-string thetas-2 :dagger dagger))))
    (quil:parse-quil quil)))

(defun find-inverse-ansatz (vqe-problem thetas initial-values)
  "Let A = A(θ) be the ANSATZ determined by THETAS. Return the value of η that maximizes ⟨0|A(η) A(θ)|0⟩.

The value of η is used to implement the penalty term in the deflation method. Ideally, one would use A(θ)† instead of A(η), but the original paper states that doing so might be hard if gate fidelities are low. The aim then is to find η such that A(η) ≈ A(θ)†."
  (let ((number-of-qubits (slot-value vqe-problem 'number-of-qubits)))
    (flet ((objective-function (etas)
             (let ((operators (list (make-penalty-term vqe-problem thetas etas :dagger nil)))
                   (empty-state-prep (quil:parse-quil "DECLARE theta REAL[2]")))
               (first (qvm-app::perform-expectation 'qvm-app::pure-state empty-state-prep operators number-of-qubits)))))
      (cl-grnm:grnm-optimize #'objective-function initial-values))))
