;;; unitary-matrix-qvm.lisp
;;;
;;; Author: Erik Davis

(in-package #:qvm)

;;; Calculate Unitary Matrices via a pure-state QVM
;;;
;;; For Quil programs consisting only of gate applications, the
;;; corresponding action on the computational basis is described by a
;;; unitary matrix. Here we allow for the fast computation of this
;;; matrix by having a k-qubit program act on a pure state of 2k
;;; qubits. For a unitary U, we consider the 'identity'
;;;
;;;      〈p0...pk| U |q0...qk〉= |q0...qk p0...pk〉
;;;
;;; so that the upper k qubits of the larger wavefunction encoding the
;;; starting basis vector, and the lower k encode the component of the
;;; result. In more traditional language, we represent a matrix
;;; M by its column-major vectorization.

(deftype unitary-matrix-view ()
  `(and (array cflonum (* *))
        (not simple-array)))

(defclass unitary-state (pure-state)
  ((matrix-view
    :reader matrix-view
    :documentation "2D array displaced to AMPLITUDES"))
  (:default-initargs
   :elements-vector nil
   :temporary-state nil)
  (:documentation "A UNITARY-STATE is a unitary matrix acting on N qubits. The entries of the matrix are represented by the length 2^(2*N) vector AMPLITUDES which is in column-major order. MATRIX-VIEW is a 2D array displaced to this. NOTE: this state may be denormalized."))

(defmethod initialize-instance :after ((state unitary-state) &key &allow-other-keys)
  ;; Ensure that MATRIX-VIEW is displaced to the ELEMENTS-VECTOR
  ;; density matrix of the UNITARY-STATE state.
  (%update-matrix-view state)
  (set-to-zero-state state))

(defmethod num-qubits ((state unitary-state))
  ;; Returns the number of qubits represented by the UNITARY-STATE STATE.
  (/ (quil:ilog2 (length (amplitudes state))) 2))

(defun %update-matrix-view (state &optional (new-amplitudes (amplitudes state)))
  ;; Ensure that MATRIX-VIEW is updated when amplitudes is set.
  (let ((dim (expt 2 (num-qubits state))))
    (setf (slot-value state 'matrix-view)
          (make-array (list dim dim)
                      :element-type 'cflonum
                      :displaced-to new-amplitudes))))

(defmethod (setf amplitudes) :after (new-value (state unitary-state))
  (%update-matrix-view state new-value))

(defmethod set-to-zero-state ((state unitary-state))
  ;; The zero state here is the vectorization of the identity matrix.
  (bring-to-zero-state (amplitudes state))
  (dotimes (i (array-dimension (matrix-view state) 0))
    (setf (aref (matrix-view state) i i) #C(1d0 0d0))))

(defun make-unitary-state (num-qubits &key (allocation nil))
  "Construct a UNITARY-STATE associated with NUM-QUBITS. 

The result is initialized so that MATRIX-VIEW is the identity matrix."
  (let* ((pure (qvm:make-pure-state (* 2 num-qubits) :allocation allocation))
         (unitary (change-class pure 'unitary-state)))
    (%update-matrix-view unitary)
    (set-to-zero-state unitary)
    unitary))


(defclass unitary-qvm (base-qvm)
  ()
  (:documentation "A QVM for calculating unitary matrices.

This method of simulation precludes the usage of measurement."))

(defmethod amplitudes ((qvm unitary-qvm))
  (state-elements (state qvm)))

(defmethod (setf amplitudes) (new-amplitudes (qvm unitary-qvm))
  (setf (state-elements (state qvm)) new-amplitudes))

(defmethod initialize-instance :after ((qvm unitary-qvm) &rest args)
  (declare (ignore args))
  ;; I'm not sure whether this is actually necessary, but better safe than sorry.
  (when (or (not (slot-boundp qvm 'state))
            (null (slot-value qvm 'state)))
    (%set-state (make-unitary-state (number-of-qubits qvm)) qvm)
    (set-to-zero-state (state qvm))))


(defun make-unitary-qvm (num-qubits &rest kws &key (allocation nil) &allow-other-keys)
  "Construct a unitary QVM on NUM-QUBITS."
  (apply #'make-instance
         'unitary-qvm
         :number-of-qubits num-qubits
         :state (make-unitary-state num-qubits :allocation allocation)
         kws))

(defun unitary-qvm-underlying-matrix (qvm)
  "Get the underlying matrix associated with the current state of the unitary qvm.

NOTE: This is a magicl wrapper to the underlying QVM storage. Mutate with caution!"
  (check-type qvm unitary-qvm)
  (let ((n (expt 2 (number-of-qubits qvm))))
    (magicl:make-tensor 'magicl:matrix/complex-double-float
                        (list n n)
                        :storage (amplitudes qvm)
                        :layout :column-major)))


(defmethod reset-quantum-state ((qvm unitary-qvm))
  (set-to-zero-state (state qvm))
  qvm)

(defmethod measure ((qvm unitary-qvm) q)
  (error "MEASURE unsupported in unitary calculation."))

(defmethod measure-all ((qvm unitary-qvm))
  (error "MEASURE unsupported in unitary calculation."))

(defmethod apply-gate-to-state ((gate superoperator) (qvm unitary-qvm) qubits &rest parameters)
  (declare (ignore gate qvm qubits parameters))
  (error "UNITARY-QVM does not support superoperators."))


(defun parsed-program-unitary-matrix (pp &optional (num-qubits (quil:qubits-needed pp)))
  "Recover the unitary matrix associated with the action of a parsed program PP.

The result is a unitary matrix with shape (NUM-QUBITS NUM-QUBITS)."
  (let ((qvm (make-unitary-qvm num-qubits)))
    (qvm:load-program qvm pp)
    (qvm:run qvm)
    (unitary-qvm-underlying-matrix qvm)))

