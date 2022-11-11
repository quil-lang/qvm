;;;; error-qvm.lisp
;;;;
;;;; This file implements an efficient QVM which tracks the propagation of Pauli
;;;; errors through a CNOT-dihedral circuit which, in the absence of errors, is
;;;; always meant to MEASURE zeroes.  For example, surface code cycles for the
;;;; usual toric code have this property.

(in-package #:qvm.error)

(defclass error-qvm (fowler-qvm classical-memory-mixin)
  ((X-vector
    :accessor error-qvm-X-vector
    :type simple-bit-vector
    :documentation "A bit-vector which tracks whether this qubit has experienced an X-error.")
   (Z-vector
    :accessor error-qvm-Z-vector
    :type simple-bit-vector
    :documentation "A bit-vector which tracks whether this qubit has experienced a Z-error.")
   (num-qubits
    :type unsigned-byte
    :reader error-qvm-num-qubits
    :initarg :num-qubits))
  (:documentation "A noisy QVM that can efficiently the propagation of Pauli errors through gates drawn from the CNOT-dihedral group of order 8.  See FOWLER-QVM for a description of the noise model and parameters."))

(defun make-error-qvm (num-qubits
                       &key
                         (classical-memory-model quil::**empty-memory-model**)
                         (noise-probability 0.0d0)
                         (noise-class 0))
  "Constructs a fresh instance of a specialized QVM which efficiently simulates Pauli error propagation through CNOT-dihedral circuits.

See ERROR-QVM for a description of the noise model and noise keyword arguments."
  (check-type num-qubits unsigned-byte)
  (check-type classical-memory-model quil:memory-model)
  (let* ((subsystem (make-instance 'classical-memory-subsystem
                                   :classical-memory-model
                                   classical-memory-model))
         (qvm (make-instance 'error-qvm
                             :num-qubits num-qubits
                             :classical-memory-subsystem subsystem
                             :noise-probability noise-probability
                             :noise-class noise-class)))
    (setf (error-qvm-X-vector qvm) (make-array num-qubits :element-type 'bit :initial-element 0)
          (error-qvm-Z-vector qvm) (make-array num-qubits :element-type 'bit :initial-element 0))
    qvm))

(defmethod number-of-qubits ((qvm error-qvm))
  (error-qvm-num-qubits qvm))

;;;;;;;;;;;;;;;;;;;;;;;;; TRANSITION Methods ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod measure ((qvm error-qvm) q)
  (values qvm (aref (error-qvm-X-vector qvm) q)))

(defmethod transition ((qvm error-qvm) (instr quil:reset))
  (let ((num-qubits (number-of-qubits qvm)))
    (setf (error-qvm-X-vector qvm) (make-array num-qubits :element-type 'bit :initial-element 0)
          (error-qvm-Z-vector qvm) (make-array num-qubits :element-type 'bit :initial-element 0))
    (incf (qvm::pc qvm))
    qvm))

(defmethod transition ((qvm error-qvm) (instr quil:reset-qubit))
  (let ((index (quil:qubit-index (quil:reset-qubit-target instr))))
    (setf (aref (error-qvm-X-vector qvm) index) 0
          (aref (error-qvm-Z-vector qvm) index) 0))
  (incf (qvm::pc qvm))
  qvm)

(defmethod transition ((qvm error-qvm) (instr quil:measure))
  (incf (qvm::pc qvm))
  (qvm::measure-and-store qvm
                          (quil:qubit-index (quil:measurement-qubit instr))
                          (quil:measure-address instr)))

(defmethod transition ((qvm error-qvm) (instr quil:measure-discard))
  (incf (qvm::pc qvm))
  (measure qvm (quil:qubit-index (quil:measurement-qubit instr))))

(defmethod transition ((qvm error-qvm) (instr quil:gate-application))
  (check-type (quil:application-operator instr) quil:named-operator)
  (adt:with-data (quil:named-operator name) (quil:application-operator instr)
    (cond
      ;; intentional toggles do nothing
      ((or (string= "X" name)
           (string= "Y" name)
           (string= "Z" name)
           (string= "I" name))
       nil)
      ;; hadamards trade X and Z
      ((string= "H" name)
       (let* ((index (quil:qubit-index (first (quil:application-arguments instr))))
              (X (aref (error-qvm-X-vector qvm) index))
              (Z (aref (error-qvm-Z-vector qvm) index)))
         (setf (aref (error-qvm-X-vector qvm) index) Z
               (aref (error-qvm-Z-vector qvm) index) X)))
      ;; CNOTs either transmit or replicate pauli errors.
      ;; Xs and Zs replicate when present on control and target respectively
      ((string= "CNOT" name)
       (let* ((control (quil:qubit-index (first  (quil:application-arguments instr))))
              (target  (quil:qubit-index (second (quil:application-arguments instr))))
              (Xc (aref (error-qvm-X-vector qvm) control))
              (Xt (aref (error-qvm-X-vector qvm) target))
              (Zc (aref (error-qvm-Z-vector qvm) control))
              (Zt (aref (error-qvm-Z-vector qvm) target)))
         (when (plusp Xc)
           (setf (aref (error-qvm-X-vector qvm) target)
                 (logxor Xt #b1)))
         (when (plusp Zt)
           (setf (aref (error-qvm-Z-vector qvm) control)
                 (logxor Zc #b1)))))
      (t
       (error "Gate ~a is not CNOT-dihedral." instr))))
  (incf (qvm::pc qvm))
  qvm)

;;;;;;;;;;;;;;;;;;;;;;;;; FOWLER-QVM Methods ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy-fowler-qvm ((qvm error-qvm))
  (let ((new-qvm
          (make-instance 'error-qvm
                         :noise-class (fowler-qvm-noise-class qvm)
                         :noise-probability (fowler-qvm-noise-probability qvm)
                         :num-qubits (error-qvm-num-qubits qvm)
                         :gate-definitions (qvm::gate-definitions qvm)
                         :wait-function (qvm::wait-function qvm)
                         :classical-memory-subsystem (qvm::classical-memory-subsystem qvm))))
    (setf (error-qvm-X-vector new-qvm) (copy-seq (error-qvm-X-vector qvm))
          (error-qvm-Z-vector new-qvm) (copy-seq (error-qvm-Z-vector qvm)))
    new-qvm))

(defmethod %fast-apply ((qvm error-qvm) instr-name qubit)
  (cond
    ((string= "X" instr-name)
     (setf (aref (error-qvm-X-vector qvm) qubit)
           (logxor #b1 (aref (error-qvm-X-vector qvm) qubit))))
    ((string= "Z" instr-name)
     (setf (aref (error-qvm-Z-vector qvm) qubit)
           (logxor #b1 (aref (error-qvm-Z-vector qvm) qubit))))
    ((string= "Y" instr-name)
     (setf (aref (error-qvm-X-vector qvm) qubit)
           (logxor #b1 (aref (error-qvm-X-vector qvm) qubit))
           (aref (error-qvm-Z-vector qvm) qubit)
           (logxor #b1 (aref (error-qvm-Z-vector qvm) qubit))))
    (t
     (error "Bad instruction in %FAST-APPLY: ~a" instr-name))))
