;;;; gates.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;;;;;;;;;;;;;;;;;;;; Gate Object Definitions ;;;;;;;;;;;;;;;;;;;;;;;

(defclass gate ()
  ((dimension :initarg :dimension
              :reader gate-dimension
              :documentation "The minimal dimension of the space the gate acts on.")
   (name :initarg :name
         :reader gate-name
         :documentation "The name of the gate.")
   (documentation :initarg :documentation
                  :reader gate-documentation
                  :documentation "Documentation about the gate."))
  (:metaclass abstract-class)
  (:documentation "Abstract class for gates."))

(defclass static-gate (gate)
  ()
  (:metaclass abstract-class)
  (:documentation "An abstract class representing gates which could be represented as a static matrix."))

(defclass dynamic-gate (gate)
  ((arity :initarg :arity
          :reader dynamic-gate-arity
          :documentation "The number of parameters the gate requires."))
  (:metaclass abstract-class)
  (:documentation "An abstract class representing gates which could be represented as a static matrix."))

(defclass simple-gate (static-gate)
  ((matrix :initarg :matrix
           :reader simple-gate-matrix
           :documentation "The matrix of the gate."))
  (:documentation "Non-parameterized gate represented as a dense matrix."))

(defclass permutation-gate (static-gate)
  ((permutation :initarg :permutation
                :reader permutation-gate-permutation
                :documentation "The permutation representation of the gate.")
   (permute-function :initarg :permute-function
                     :reader permutation-gate-permute-function
                     :documentation "A function which permutes a vector in-place according to the permutation."))
  (:documentation "A gate which could be realized as a permutation matrix."))

(defun permutation-to-transpositions (permutation)
  "Decompose a permutation PERMUTATION represented as a sequence of non-negative integers into a list of transpositions represented as conses."
  (let ((indices (copy-seq permutation))
        (swaps nil))
    (dotimes (dest (length indices) (nreverse swaps))
      (let ((src (elt indices dest)))
        (loop :while (< src dest) :do
          (setf src (elt indices src)))
        (when (/= src dest)
          (push (cons src dest) swaps))))))

(defun generate-permutation-gate-function (permutation)
  "Generate an efficient unary function which modifies a quantum state according to the permutation PERMUTATION."
  (labels ((generate-code (transpositions)
             (loop :with variable := (gensym "V")
                   :for (left . right) :in transpositions
                   :collect `(rotatef (aref ,variable ,left)
                                      (aref ,variable ,right))
                     :into rotations
                   :finally (return (values variable rotations))))

           (generate-lambda-form (variable code)
             `(lambda (,variable)
                (declare (optimize speed
                                   (safety 0)
                                   (debug 0)
                                   (space 0))
                         (type quantum-state ,variable))
                ,@code
                nil)))
    (multiple-value-bind (variable code)
        (generate-code (permutation-to-transpositions permutation))
      (compile nil (print (generate-lambda-form variable code))))))

(defun make-permutation-gate (name documentation &rest permutation)
  "Make a permutation gate with the permut"
  (check-type name string)
  (check-type documentation string)
  (assert (zerop (logand (length permutation)
                         (1- (length permutation))))
          (permutation)
          "Permutation ~A must be of a power-of-two number of elements."
          permutation)
  (make-instance
   'permutation-gate
   :name name
   :documentation documentation
   :dimension (length permutation)
   :permutation permutation
   :permute-function (generate-permutation-gate-function permutation)))

(defclass parameterized-gate (dynamic-gate)
  ((matrix-function :initarg :matrix-function
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
    (let ((expected (dynamic-gate-arity g))
          (actual   (length parameters)))
      (assert (= expected actual)
              ()
              "The parameterized gate ~A requires ~D parameters but received ~D."
              (gate-name g)
              expected
              actual))
    (apply (parameterized-gate-matrix-function g) parameters)))


;;;;;;;;;;;;;;;;;;;;;;;;;; Gate Application ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; One might ask why we define a generic function to act on a
;;; wavefunction with qubits, as opposed to a state vector. I (Robert)
;;; think the the latter approach would be more mathematically
;;; elegant, but unfortunately, it is much harder to optimize. The
;;; compiler will be unable to inline any of the various chains of
;;; functions if we need to dispatch on every subspace application.

(defgeneric apply-gate (gate wavefunction qubits &rest parameters)
  (:documentation "Apply a gate GATE to the wavefunction WAVEFUNCTION on the sub-Hilbert space defined by the NAT-TUPLE of qubit indexes QUBITS. PARAMETERS is a list of numeric parameters passed to a dynamic gate.")

  (:method ((gate simple-gate) wavefunction qubits &rest parameters)
    (apply-matrix-operator
     (apply #'gate-operator gate parameters)
     wavefunction
     qubits))

  (:method ((gate parameterized-gate) wavefunction qubits &rest parameters)
    (apply-matrix-operator
     (apply #'gate-operator gate parameters)
     wavefunction
     qubits))

  (:method ((gate permutation-gate) wavefunction qubits &rest parameters)
    (assert (null parameters) (parameters) "Parameters don't make sense for a permutation gate.")
    (apply-operator
     (permutation-gate-permute-function gate)
     wavefunction
     qubits)))


;;;;;;;;;;;;;;;;;;;;;; Default Gate Definitions ;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-gate-definitions* (make-hash-table :test 'equal)
  "A table of default gate definitions.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun record-standard-gate (name gate)
    "Record the standard gate GATE under the name NAME in the default list of standard gates."
    (check-type name string)
    (check-type gate gate)
    (setf (gethash name *default-gate-definitions*) gate)
    (values)))

(defmacro define-default-gate (name num-qubits (&rest params) &body matrix-code)
  "Defines a gate and adds it to the default-provided gate table.

    NAME: The name of the gate (symbol)
    NUM-QUBITS: The number of qubits the gate intends to act on.
    PARAMS: Parameter list for the gate (may be empty)
    MATRIX-CODE: The code, which may depend on PARAMS, to generate the gate's operator.
"
  (check-type name symbol)
  (check-type num-qubits (integer 1))
  (let ((name-string (symbol-name name))
        (dimension (expt 2 num-qubits)))
    (multiple-value-bind (forms decls doc-string)
        (alexandria:parse-body matrix-code :documentation t)
      (cond
        ;; Simple gate.
        ((null params)
         (let ((matrix (gensym "MATRIX-")))
           `(record-standard-gate
             ',name-string
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
           `(record-standard-gate
             ',name-string
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

(record-standard-gate "I" (make-permutation-gate
                           "I"
                           "The identity gate."
                           0 1))

(record-standard-gate "X" (make-permutation-gate
                           "X"
                           "The Pauli-X gate."
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

(record-standard-gate "CNOT" (make-permutation-gate
                              "CNOT"
                              "The controlled-X or controlled-NOT gate."
                              0 1 3 2))

(record-standard-gate "CCNOT" (make-permutation-gate
                              "CCNOT"
                              "The Toffoli gate, AKA the CCNOT gate."
                              0 1 2 3
                              4 5 7 6))

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

(record-standard-gate "CSWAP" (make-permutation-gate
                              "CSWAP"
                              "The Fredkin gate, AKA the CSWAP gate."
                              0 1 2 3
                              4 6 5 7))

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
