;;;; gates.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defclass gate ()
  ((dimension :initarg :dimension
              :reader gate-dimension)
   (name :initarg :name
         :reader gate-name)
   (documentation :initarg :documentation
                  :reader gate-documentation))
  (:documentation "Abstract class for gates."))

(defclass simple-gate (gate)
  ((matrix :initarg :matrix
           :reader simple-gate-matrix))
  (:documentation "Non-parameterized gate."))

(defclass parameterized-gate (gate)
  ((arity :initarg :arity
          :reader parameterized-gate-arity)
   (matrix-function :initarg :matrix-function
                    :reader parameterized-gate-matrix-function
                    :documentation "Function mapping ARITY complex numbers to a DIMENSION x DIMENSION matrix."))
  (:documentation "A gate parameterized by complex numbers."))

(defgeneric gate-operator (gate &rest parameters)
  (:documentation "Given a gate GATE and possibly some parameters PARAMETERS, compute the associated gate operator matrix.")
  (:method ((g simple-gate) &rest parameters)
    (assert (null parameters) () "The simple gate ~A is not parameterized."
            (gate-name g))
    (simple-gate-matrix g))
  (:method ((g parameterized-gate) &rest parameters)
    (let ((expected (parameterized-gate-arity g))
          (actual   (length parameters)))
      (assert (= expected actual)
              ()
              "The parameterized gate ~A requires ~D parameters but received ~D."
              (gate-name g)
              expected
              actual))
    (apply (parameterized-gate-matrix-function g) parameters)))

(defvar *default-gate-definitions* (make-hash-table :test 'equal)
  "A table of default gate definitions.")

(defmacro define-default-gate (name qubits (&rest params) &body matrix-code)
  "Defines a gate and adds it to the default-provided gate table.

    NAME: The name of the gate (symbol)
    QUBITS: The number of qubits the gate intends to act on.
    PARAMS: Parameter list for the gate (may be empty)
    MATRIX-CODE: The code, which may depend on PARAMS, to generate the gate's operator.
"
  (check-type name symbol)
  (check-type qubits (integer 1))
  (let ((name-string (symbol-name name))
        (dimension (expt 2 qubits)))
    (multiple-value-bind (forms decls doc-string)
        (alexandria:parse-body matrix-code :documentation t)
      (cond
        ;; Simple gate.
        ((null params)
         (let ((matrix (gensym "MATRIX-")))
           `(setf (gethash ',name-string *default-gate-definitions*)
                  (let ((,matrix (locally ,@decls ,@forms)))
                    (assert (= (array-dimension ,matrix 0)
                               (array-dimension ,matrix 1)
                               ,dimension))
                    (make-instance 'simple-gate
                                   :name ',name-string
                                   :documentation ',doc-string
                                   :dimension ',dimension
                                   :matrix ,matrix)))))
        ;; Parameterized gate.
        (t
         (let ((arity (length params))
               (matrix-fn (gensym (format nil "~A-MATRIX-FN-" name-string))))
           `(setf (gethash ',name-string *default-gate-definitions*)
                  (flet ((,matrix-fn ,params
                           ,@decls
                           ;; TODO: Check that DIMENSION matches the generated matrix.
                           ,@forms))
                    (make-instance 'parameterized-gate
                                   :name ',name-string
                                   :documentation ',doc-string
                                   :dimension ,dimension
                                   :arity ,arity
                                   :matrix-function #',matrix-fn)))))))))

;; Evaluate this stuff earlier so we can have access while reading.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun operator-matrix-from-truth-table (truth-table-outputs)
    "Return an appropriate matrix which can act as an operator for the truth table outputs TRUTH-TABLE-OUTPUTS (represented as a list) encoded in binary lexicographic order.

Example: A NAND gate can be made with

    (operator-matrix-from-truth-table '(1 1 1 0))
"
    (let* ((column-length (length truth-table-outputs))
           (operator-size (* 2 column-length))
           (matrix (make-matrix operator-size))
           (one (cflonum 1)))
      (loop :for i :below column-length
            :for x :in truth-table-outputs
            :for offset := (* 2 i)
            :do (if (zerop x)
                    (setf (aref matrix offset offset)           one
                          (aref matrix (1+ offset) (1+ offset)) one)
                    (setf (aref matrix offset (1+ offset))      one
                          (aref matrix (1+ offset) offset)      one)))
      matrix))

  (defun controlled (U)
    "Construct a controlled version of the one-qubit matrix operator U."
    (assert (= 2
               (array-dimension U 0)
               (array-dimension U 1)))
    (let ((u00 (aref U 0 0))
          (u01 (aref U 0 1))
          (u10 (aref U 1 0))
          (u11 (aref U 1 1)))
      (make-matrix 4
                   1 0 0 0
                   0 1 0 0
                   0 0 u00 u01
                   0 0 u10 u11))))

;;; The default gate set

(define-default-gate H 1 ()
  "The Hadamard gate."
  '#.(let ((1/sqrt2 (/ (sqrt 2.0d0))))
       (make-matrix 2
                    1/sqrt2 1/sqrt2
                    1/sqrt2 (- 1/sqrt2))))

(define-default-gate I 1 ()
  "The identity gate."
  '#.(make-matrix 2
                  1 0
                  0 1))

(define-default-gate X 1 ()
  "The Pauli-X gate."
  '#.(make-matrix 2
                  0 1
                  1 0))

(define-default-gate Y 1 ()
  "The Pauli-Y gate."
  '#.(make-matrix 2
                  0       #C(0 -1)
                  #C(0 1) 0))

(define-default-gate Z 1 ()
  "The Pauli-Z gate."
  '#.(make-matrix 2
                  1 0
                  0 -1))

(define-default-gate CNOT 2 ()
  "The controlled NOT gate."
  '#.(controlled #2A((0 1) (1 0))))

(define-default-gate CCNOT 3 ()
  "The Toffoli gate, AKA the CCNOT gate."
  '#.(make-matrix 8
                  1 0 0 0 0 0 0 0
                  0 1 0 0 0 0 0 0
                  0 0 1 0 0 0 0 0
                  0 0 0 1 0 0 0 0
                  0 0 0 0 1 0 0 0
                  0 0 0 0 0 1 0 0
                  0 0 0 0 0 0 0 1
                  0 0 0 0 0 0 1 0))

(define-default-gate RX 1 (theta)
  "The R_x(theta) gate."
  (let* ((theta/2 (/ theta 2))
         (cos (cos theta/2))
         (isin (complex 0.0d0 (- (sin theta/2)))))
    (make-matrix 2
                 cos isin
                 isin cos)))

(define-default-gate RY 1 (theta)
  "The R_y(theta) gate."
  (let* ((theta/2 (/ theta 2))
         (cos (cos theta/2))
         (sin (sin theta/2)))
    (make-matrix 2
                 cos (- sin)
                 sin cos)))

(define-default-gate RZ 1 (theta)
  "The R_z(theta) gate."
  (let ((theta/2 (/ theta 2)))
    (make-matrix 2
                 (cis (- theta/2)) 0
                 0                 (cis theta/2))))

(define-default-gate phase 1 (alpha)
  "A regular phase gate. Equivalent to R_z multiplied by a phase."
  (make-matrix 2
               1 0
               0 (cis alpha)))

(define-default-gate S 1 ()
  "The S gate."
  '#.(make-matrix 2
                  1 0
                  0 #C(0 1)))

(define-default-gate T 1 ()
  "The T gate."
  '#.(make-matrix 2
                  1 0
                  0 (cis (/ pi 4))))

(define-default-gate CPHASE00 2 (alpha)
  "The controlled phase gate (00-variant)."
  (make-matrix 4
               (cis alpha) 0 0 0
               0           1 0 0
               0           0 1 0
               0           0 0 1))

(define-default-gate CPHASE01 2 (alpha)
  "The controlled phase gate (01-variant)."
  (make-matrix 4
               1 0           0 0
               0 (cis alpha) 0 0
               0 0           1 0
               0 0           0 1))

(define-default-gate CPHASE10 2 (alpha)
  "The controlled phase gate (10-variant)."
  (make-matrix 4
               1 0 0           0
               0 1 0           0
               0 0 (cis alpha) 0
               0 0 0           1))

(define-default-gate CPHASE 2 (alpha)
  "The controlled phase gate (11-variant).

Note that this is a controlled version of a R_z gate multiplied by a phase."
  (make-matrix 4
               1 0 0 0
               0 1 0 0
               0 0 1 0
               0 0 0 (cis alpha)))

(define-default-gate SWAP 2 ()
  "A quantum gate that swaps the relevant amplitudes of two qubits."
  '#.(make-matrix 4
                  1 0 0 0
                  0 0 1 0
                  0 1 0 0
                  0 0 0 1))

(define-default-gate CSWAP 3 ()
  "The Fredkin gate, AKA the CSWAP gate."
  '#.(make-matrix 8
                  1 0 0 0 0 0 0 0
                  0 1 0 0 0 0 0 0
                  0 0 1 0 0 0 0 0
                  0 0 0 1 0 0 0 0
                  0 0 0 0 1 0 0 0
                  0 0 0 0 0 0 1 0
                  0 0 0 0 0 1 0 0
                  0 0 0 0 0 0 0 1))

(define-default-gate ISWAP 2 ()
  "The ISWAP gate, for superconducting quantum computers."
  '#.(make-matrix 4
                  1 0       0       0
                  0 0       #C(0 1) 0
                  0 #C(0 1) 0       0
                  0 0       0       1))

(define-default-gate PSWAP 2 (theta)
  "The parametric SWAP gate, for superconducting quantum computers."
  (make-matrix 4
               1 0           0           0
               0 0           (cis theta) 0
               0 (cis theta) 0           0
               0 0           0           1))
