;;;; gates.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;; TODO: Ensure complex matrices.
(defun operator-matrix-from-truth-table (truth-table-outputs)
  "Return an appropriate matrix which can act as an operator for the truth table outputs TRUTH-TABLE-OUTPUTS (represented as a list) encoded in binary lexicographic order."
  (let* ((column-length (length truth-table-outputs))
         (operator-size (* 2 column-length))
         (matrix (make-array (list operator-size operator-size)
                             :initial-element 0)))
    (loop :for i :below column-length
          :for x :in truth-table-outputs
          :for offset := (* 2 i)
          :do (if (zerop x)
                  (setf (aref matrix offset offset) 1
                        (aref matrix (1+ offset) (1+ offset)) 1)
                  (setf (aref matrix offset (1+ offset)) 1
                        (aref matrix (1+ offset) offset) 1)))
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
                 0 0 u10 u11)))

(defun hadamard (qvm q)
  "The Hadamard gate."
  (apply-operator
   qvm
   '#.(let ((1/sqrt2 (/ (sqrt 2.0d0))))
        (make-matrix 2
                     1/sqrt2 1/sqrt2
                     1/sqrt2 (- 1/sqrt2)))
   (nat-tuple q)))

(defun pauli-X (qvm q)
  "The Pauli-X gate."
  (apply-operator qvm
                  '#.(make-matrix 2
                                  0 1
                                  1 0)
                  (nat-tuple q)))

(defun pauli-Y (qvm q)
  "The Pauli-Y gate."
  (apply-operator qvm
                  '#.(make-matrix 2
                                  0       #C(0 -1)
                                  #C(0 1) 0)
                  (nat-tuple q)))

(defun pauli-Z (qvm q)
  "The Pauli-Z gate."
  (apply-operator qvm
                  '#.(make-matrix 2
                                  1 0
                                  0 -1)
                  (nat-tuple q)))

(defun qnot (qvm q)
  "The quantum NOT gate, AKA the Pauli-X gate."
  (pauli-X qvm q))

(defun cnot (qvm q1 q2)
  "The controlled NOT gate."
  (apply-operator qvm
                  (controlled #2A((0 1) (1 0)))
                  (nat-tuple q1 q2)))

(defun sqrt-qnot (qvm q)
  "The quantum square-root-of-NOT gate."
  (apply-operator
   qvm
   '#.(let ((1/sqrt2 (/ (sqrt 2.0d0))))
        (make-matrix 2
                     1/sqrt2 (- 1/sqrt2)
                     1/sqrt2 1/sqrt2))
   (nat-tuple q)))

(defun nand (qvm q1 q2 q3)
  "The quantum NAND gate."
  (apply-operator
   qvm
   (operator-matrix-from-truth-table '(1 1 1 0))
   (nat-tuple q1 q2 q3)))

(defun rotation-x (qvm q theta)
  "The R_x(theta) gate."
  (let* ((theta/2 (/ theta 2))
         (cos (cos theta/2))
         (isin (complex 0.0d0 (- (sin theta/2)))))
    (apply-operator
     qvm
     (make-matrix 2
                  cos isin
                  isin cos)
     (nat-tuple q))))

(defun rotation-y (qvm q theta)
  "The R_y(theta) gate."
  (let* ((theta/2 (/ theta 2))
         (cos (cos theta/2))
         (sin (sin theta/2)))
    (apply-operator
     qvm
     (make-matrix 2
                  cos (- sin)
                  sin cos)
     (nat-tuple q))))

(defun rotation-z (qvm q theta)
  "The R_z(theta) gate."
  (let* ((theta/2 (/ theta 2)))
    (apply-operator
     qvm
     (make-matrix 2
                  (cis (- theta/2)) 0
                  0                 (cis theta/2))
     (nat-tuple q))))

(defun cphase (qvm q1 q2 alpha)
  "The controlled phase gate.

Note that this is a controlled version of a R_z gate multiplied by a phase."
  (apply-operator
   qvm
   (make-matrix 4
                1 0 0 0
                0 1 0 0
                0 0 1 0
                0 0 0 (cis alpha))
   (nat-tuple q1 q2)))

(defun swap (qvm q1 q2)
  "A quantum gate that swaps the relevant amplitudes of two qubits."
  (apply-operator
   qvm
   '#.(make-matrix 4
                   1 0 0 0
                   0 0 1 0
                   0 1 0 0
                   0 0 0 1)
   (nat-tuple q1 q2)))

(defun sqrt-swap (qvm q1 q2)
  "The square-root-of-SWAP gate."
  (apply-operator
   qvm
   '#.(make-matrix 4
                   1 0 0 0
                   0 #C(0.5 0.5) #C(0.5 -0.5) 0
                   0 #C(0.5 -0.5) #C(0.5 0.5) 0
                   0 0 0 1)
   (nat-tuple q1 q2)))

(defun toffoli (qvm q1 q2 q3)
  "The Toffoli gate, AKA the CCNOT gate."
  (apply-operator
   qvm
   '#.(make-matrix 8
                   1 0 0 0 0 0 0 0
                   0 1 0 0 0 0 0 0
                   0 0 1 0 0 0 0 0
                   0 0 0 1 0 0 0 0
                   0 0 0 0 1 0 0 0
                   0 0 0 0 0 1 0 0
                   0 0 0 0 0 0 0 1
                   0 0 0 0 0 0 1 0)
   (nat-tuple q1 q2 q3)))

(defun fredkin (qvm q1 q2 q3)
  "The Fredkin gate, AKA the CSWAP gate."
  (apply-operator
   qvm
   '#.(make-matrix 8
                   1 0 0 0 0 0 0 0
                   0 1 0 0 0 0 0 0
                   0 0 1 0 0 0 0 0
                   0 0 0 1 0 0 0 0
                   0 0 0 0 1 0 0 0
                   0 0 0 0 0 0 1 0
                   0 0 0 0 0 1 0 0
                   0 0 0 0 0 0 0 1)
   (nat-tuple q1 q2 q3)))


;;; Meta-operators

(defun reset (qvm)
  "Perform a reset. Bring all qubits to |0>."
  (map-into (amplitudes qvm) (constantly #C(0.0d0 0.0d0)))
  (setf (aref (amplitudes qvm) 0) #C(1.0d0 0.0d0))
  qvm)

;;; These are useful for debugging and other classical execution. They
;;; are a particular feature of this implementation, not a part of the
;;; specification of the QAM/QIL.

(defun print-amplitudes (qvm &optional string)
  "Print the amplitudes nicely, prepended with the optional string STRING."
  (let ((amplitudes
          (coerce (amplitudes qvm) 'list)))
    (format t "~@[~A: ~]~{~A~^, ~}~%" string amplitudes)
    ;; Return the qvm.
    qvm))

(defun print-probabilities (qvm &optional string)
  "Print the probabilities nicely, prepended with the optional string STRING."
  (let ((probabilities
          (map 'list #'probability (amplitudes qvm))))
    (format t "~@[~A: ~]~{~5F~^, ~}~%" string probabilities)
    ;; Return the qvm.
    qvm))

(defun classical-call (qvm function)
  "Classically call the function FUNCTION (which takes one argument, the QVM)."
  (funcall function qvm)
  qvm)
