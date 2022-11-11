;;;; depolarizing-noise.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defclass depolarizing-qvm (pure-state-qvm)
  ((probability-gate-x
    :initarg :x
    :accessor probability-gate-x
    :documentation "Probability of a Pauli X gate happening after a gate application or reset.")
   (probability-gate-y
    :initarg :y
    :accessor probability-gate-y
    :documentation "Probability of a Pauli Y gate happening after a gate application or reset.")
   (probability-gate-z
    :initarg :z
    :accessor probability-gate-z
    :documentation "Probability of a Pauli Z gate happening after a gate application or reset.")
   (probability-measure-x
    :initarg :measure-x
    :accessor probability-measure-x
    :documentation "Probability of a Pauli X gate happening before a measurement.")
   (probability-measure-y
    :initarg :measure-y
    :accessor probability-measure-y
    :documentation "Probability of a Pauli Y gate happening before a measurement.")
   (probability-measure-z
    :initarg :measure-z
    :accessor probability-measure-z
    :documentation "Probability of a Pauli Z gate happening before a measurement."))
  (:documentation "A quantum virtual machine with parametric depolarizing noise.")
  (:default-initargs :x 0.0d0
                     :y 0.0d0
                     :z 0.0d0
                     :measure-x 0.0d0
                     :measure-y 0.0d0
                     :measure-z 0.0d0))

(defgeneric add-depolarizing-noise (qvm qubits px py pz)
  (:documentation
   "Apply depolarizing noise to the list QUBITS of the QVM with the following probabilities:

    * Probability of an X-gate PX,
    * Probability of a Y-gate PY, and
    * Probability of a Z-gate PZ.

It should be that PX + PY + PZ <= 1.")
  (:method ((qvm pure-state-qvm) qubits px py pz)
    (assert (<= (+ px py pz) 1))
    (let ((X (quil:gate-definition-to-gate (quil:lookup-standard-gate "X")))
          (Y (quil:gate-definition-to-gate (quil:lookup-standard-gate "Y")))
          (Z (quil:gate-definition-to-gate (quil:lookup-standard-gate "Z")))
          (sum (+ px py pz)))
      (probabilistically sum
        (setf px (/ px sum)
              py (/ py sum)
              pz (/ pz sum))
        (let ((r (random 1.0d0))
              (pure-state (state qvm)))
          (when (< r px)
            (apply-gate-to-state X pure-state qubits)
            (return-from add-depolarizing-noise))
          (decf r px)
          (when (< r py)
            (apply-gate-to-state Y pure-state qubits)
            (return-from add-depolarizing-noise))
          (decf r py)
          (when (< r pz)
            (apply-gate-to-state Z pure-state qubits)
            (return-from add-depolarizing-noise)))))))

;;; Noise gets added to only the qubits being changed.
(defmethod transition :after ((qvm depolarizing-qvm) (instr quil:application))
  (dolist (arg (quil:application-arguments instr))
    (when (typep arg 'quil:qubit)
      (let ((instr-qubits (quil:qubit-index arg)))
        (add-depolarizing-noise qvm (list instr-qubits)
                                (probability-gate-x qvm)
                                (probability-gate-y qvm)
                                (probability-gate-z qvm))))))

;;; Noise gets added to every qubit after a RESET.
(defmethod transition :after ((qvm depolarizing-qvm) (instr quil:reset))
  (declare (ignore instr))
  (dotimes (q (number-of-qubits qvm))
    (add-depolarizing-noise qvm (list q)
                            (probability-gate-x qvm)
                            (probability-gate-y qvm)
                            (probability-gate-z qvm))))

;;; Noise gets added to only the qubit being measured, before
;;; measurement occurs.
(defmethod transition :before ((qvm depolarizing-qvm) (instr quil:measurement))
  (let ((q (quil:qubit-index (quil:measurement-qubit instr))))
    (add-depolarizing-noise qvm (list q)
                            (probability-measure-x qvm)
                            (probability-measure-y qvm)
                            (probability-measure-z qvm))))

;;; Don't compile things for the depolarizing-qvm.
(defmethod compile-loaded-program ((qvm depolarizing-qvm))
  qvm)

;;;
;;; for want of a better idea, we replicate most of the above for stabilizer QVMs
;;;

(defclass depolarizing-stabilizer-qvm (stabilizer-qvm)
  ((probability-gate-x
    :initarg :x
    :accessor probability-gate-x
    :documentation "Probability of a Pauli X gate happening after a gate application or reset.")
   (probability-gate-y
    :initarg :y
    :accessor probability-gate-y
    :documentation "Probability of a Pauli Y gate happening after a gate application or reset.")
   (probability-gate-z
    :initarg :z
    :accessor probability-gate-z
    :documentation "Probability of a Pauli Z gate happening after a gate application or reset.")
   (probability-measure-x
    :initarg :measure-x
    :accessor probability-measure-x
    :documentation "Probability of a Pauli X gate happening before a measurement.")
   (probability-measure-y
    :initarg :measure-y
    :accessor probability-measure-y
    :documentation "Probability of a Pauli Y gate happening before a measurement.")
   (probability-measure-z
    :initarg :measure-z
    :accessor probability-measure-z
    :documentation "Probability of a Pauli Z gate happening before a measurement."))
  (:documentation "A quantum virtual machine with parametric depolarizing noise.")
  (:default-initargs :x 0.0d0
                     :y 0.0d0
                     :z 0.0d0
                     :measure-x 0.0d0
                     :measure-y 0.0d0
                     :measure-z 0.0d0))

(defmethod transition :after ((qvm depolarizing-stabilizer-qvm) (instr cl-quil:application))
  (dolist (arg (cl-quil:application-arguments instr))
    (when (typep arg 'cl-quil:qubit)
      (let ((instr-qubits (quil:qubit-index arg)))
        (add-depolarizing-noise qvm (list instr-qubits)
                                (probability-gate-x qvm)
                                (probability-gate-y qvm)
                                (probability-gate-z qvm))))))

(defmethod transition :after ((qvm depolarizing-stabilizer-qvm) (instr cl-quil:reset))
  (declare (ignore instr))
  (dotimes (q (number-of-qubits qvm))
    (add-depolarizing-noise qvm (list q)
                            (probability-gate-x qvm)
                            (probability-gate-y qvm)
                            (probability-gate-z qvm))))

(defmethod transition :before ((qvm depolarizing-stabilizer-qvm) (instr cl-quil:measurement))
  (let ((q (cl-quil:qubit-index (cl-quil:measurement-qubit instr))))
    (add-depolarizing-noise qvm (list q)
                            (probability-measure-x qvm)
                            (probability-measure-y qvm)
                            (probability-measure-z qvm))))

(defmethod compile-loaded-program ((qvm depolarizing-stabilizer-qvm))
  qvm)

(defmethod add-depolarizing-noise ((qvm stabilizer-qvm) qubits px py pz)
  (flet ((apply-gate (instr)
           (let ((clifford (gate-application-to-clifford instr)))
             (apply (compile-clifford clifford)
                    (stabilizer-qvm-tableau qvm)
                    qubits))))
    (assert (<= (+ px py pz) 1))
    (let ((X (apply #'quil::build-gate "X" () qubits))
          (Y (apply #'quil::build-gate "Y" () qubits))
          (Z (apply #'quil::build-gate "Z" () qubits))
          (sum (+ px py pz)))
      (probabilistically sum
        (setf px (/ px sum)
              py (/ py sum)
              pz (/ pz sum))
        (let ((r (random 1.0d0)))
          (when (< r px)
            (apply-gate X)
            (return-from add-depolarizing-noise))
          (decf r px)
          (when (< r py)
            (apply-gate Y)
            (return-from add-depolarizing-noise))
          (decf r py)
          (when (< r pz)
            (apply-gate Z)
            (return-from add-depolarizing-noise)))))))
