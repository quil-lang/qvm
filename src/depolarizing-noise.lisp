;;;; depolarizing-noise.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defclass noisy-qvm (quantum-virtual-machine)
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
  (:default-initargs :x 0.0
                     :y 0.0
                     :z 0.0
                     :measure-x 0.0
                     :measure-y 0.0
                     :measure-z 0.0))

(defun add-depolarizing-noise (qvm qubit px py pz)
  "Apply depolarizing noise to the qubit numbered QUBIT of the QVM with the following probabilities:

    * Probability of an X-gate PX,
    * Probability of a Y-gate PY, and
    * Probability of a Z-gate PZ.

It should be that PX + PY + PZ <= 1.
"
  (check-type qubit unsigned-byte)
  (assert (<= (+ px py pz) 1))
  (let ((X (lookup-gate qvm "X"))
        (Y (lookup-gate qvm "Y"))
        (Z (lookup-gate qvm "Z"))
        (sum (+ px py pz)))
    (probabilistically sum
      (setf px (/ px sum)
            py (/ py sum)
            pz (/ pz sum))
      (let ((r (random 1.0))
            (amplitudes (amplitudes qvm)))
        (when (< r px)
          (apply-gate X amplitudes (nat-tuple qubit))
          (return-from add-depolarizing-noise))
        (decf r px)
        (when (< r py)
          (apply-gate Y amplitudes (nat-tuple qubit))
          (return-from add-depolarizing-noise))
        (decf r py)
        (when (< r pz)
          (apply-gate Z amplitudes (nat-tuple qubit))
          (return-from add-depolarizing-noise))))))

;;; Noise gets added to only the qubits being changed.
(defmethod transition :after ((qvm noisy-qvm) (instr cl-quil:application))
  (declare (ignore instr))
  (dolist (arg (cl-quil:application-arguments instr))
    (when (typep arg 'cl-quil:qubit)
      (let ((q (cl-quil:qubit-index arg)))
        (add-depolarizing-noise qvm q
                                (probability-gate-x qvm)
                                (probability-gate-y qvm)
                                (probability-gate-z qvm))))))

;;; Noise gets added to every qubit after a RESET.
(defmethod transition :after ((qvm noisy-qvm) (instr cl-quil:reset))
  (declare (ignore instr))
  (dotimes (q (number-of-qubits qvm))
    (add-depolarizing-noise qvm q
                            (probability-gate-x qvm)
                            (probability-gate-y qvm)
                            (probability-gate-z qvm))))

;;; Noise gets added to only the qubit being measured, before
;;; measurement occurs.
(defmethod transition :before ((qvm noisy-qvm) (instr cl-quil:measurement))
  (let ((q (cl-quil:qubit-index (cl-quil:measurement-qubit instr))))
    (add-depolarizing-noise qvm q
                            (probability-measure-x qvm)
                            (probability-measure-y qvm)
                            (probability-measure-z qvm))))
