(in-package #:qvm)

;; This is the state protocol for setting the initial state to the zero state.
(defgeneric set-to-zero-state (state)
  (:documentation "Set the initial state to the pure zero state."))

;; This is the state protocol for applying a gate to a state.
(defgeneric apply-gate-to-state (state gate qubits params)
  (:documentation "modifies the state amplitudes by applying the information stored in params."))

;; This is the state protocol for applying kraus operators to a state. 
(defgeneric apply-kraus-ops-to-state (state instr kraus-ops params))


(defclass quantum-system-state ()
  ((allocation
    :reader allocation
    :initarg allocation))
  (:metaclass abstract-class))

(defclass pure-state (quantum-system-state)
  ((amplitudes
    :accessor amplitudes
    :initarg :amplitudes)
   (trial-amplitudes
    :accessor trial-amplitudes
    :initarg :trial-amplitudes)))

(defun make-pure-state (num-qubits &key (allocation nil))
  "ALLOCATION is an optional argument with the following behavior.

    - If it's NULL (default), then a standard wavefunction in the Lisp heap will be allocated.

    - If it's a STRING, then the wavefunction will be allocated as a shared memory object, accessible by that name.

    - Otherwise, it's assumed to be an object that is compatible with the ALLOCATION-LENGTH and ALLOCATE-VECTOR methods
    - will probs have to redo this in multiple places, have a helper function do the allocation stuff
"
  (let ((allocation
          (etypecase allocation
            (null
             (make-instance 'lisp-allocation :length (expt 2 num-qubits)))
            (string
             (make-instance 'posix-shared-memory-allocation :length (expt 2 num-qubits)
                                                            :name allocation))
            (t
             (assert (= (allocation-length allocation) (expt 2 num-qubits)))
             allocation))))
    (multiple-value-bind (amplitudes finalizer)
        (allocate-vector allocation)
      ;; initialize amplitudes to zero state (efficiently)
      (setf (aref amplitudes 0) (cflonum 1)) 
      (let ((state (make-instance 'pure-state :num-qubits num-qubits :amplitudes amplitudes :allocation allocation)))
        ;; When the state disappears, make sure the shared
        ;; memory gets deallocated too.
        (tg:finalize state finalizer)
        state))))


(defmethod initialize-instance :after ((state pure-state) &key num-qubits &allow-other-keys)
  (cond 
    ((and (slot-boundp state 'amplitudes)
          (not (null (slot-value state 'amplitudes))))
     (assert (<= num-qubits (wavefunction-qubits (amplitudes state)))
             ()
             state
             (wavefunction-qubits (amplitudes state))
             num-qubits))
     (t
      (setf
       (amplitudes state) (make-lisp-cflonum-vector (expt 2 num-qubits))
       (trial-amplitudes state) (make-lisp-cflonum-vector (expt 2 num-qubits))))))



(defmethod num-qubits ((state pure-state))
  (quil:ilog2 (length (amplitudes state))))


(defmethod set-to-zero-state ((state pure-state))
  (bring-to-zero-state (amplitudes state)))


(defmethod apply-gate-to-state ((state pure-state) gate qubits params)
  (apply #'apply-gate gate (amplitudes state) (apply #'nat-tuple qubits) params))


;;; DENSITY-MATRIX-STATE class -------------------------------------------------
(defclass density-matrix-state (quantum-system-state)
  ((amplitudes
    :accessor amplitudes
    :initarg :amplitudes) 
   (matrix-view
    :initarg :matrix-view
    :reader matrix-view
    :documentation "2D array displaced to amplitudes")
   (temporary-state
    :initarg :temporary-state
    :initform nil
    :accessor temporary-state)))


(defmethod num-qubits ((state density-matrix-state))
  (/ (quil:ilog2 (length (amplitudes state))) 2))


(defmethod (setf amplitudes) :after (new-value (state density-matrix-state))
  (let ((dim (expt 2 (num-qubits state))))
    (setf (slot-value state 'matrix-view) (make-array (list dim dim)
                                                      :element-type 'cflonum
                                                      :displaced-to new-value))))



(defmethod initialize-instance :after ((state density-matrix-state) &key num-qubits &allow-other-keys)
  (setf (amplitudes state) (make-lisp-cflonum-vector (expt 2 (* 2 num-qubits)))))


(defmethod set-to-zero-state ((state density-matrix-state))
  (bring-to-zero-state (amplitudes state)))


#|(defmethod apply-gate-to-state ((state density-matrix-state) gate qubits params)  
;; Transition will now look like this:
;;
;; (def transition
;;   (assert blah)
;;   (let (gate-name blah)
;;        (gate blah)
;;        (params blah)
;;        (qubits blah)
;;        
;;      (apply-gate-to-state state gate qubits params)
;;      (incf (pc qvm))  
;;
  (let* ((ghosts (mapcar (alexandria:curry #'+ (num-qubits state)) qubits))
         (superoperator (single-kraus gate)))
    (multiple-value-bind (new-density temp-storage) (apply-superoperator superoperator
                                                                         (amplitudes state)
                                                                         (apply #'nat-tuple qubits)
                                                                         (apply #'nat-tuple ghosts)
                                                                         :temporary-storage (temporary-state state)
                                                                         :params params)
      (declare (ignore new-density))
      (setf (temporary-state state) temp-storage))))


(defmethod apply-kraus-ops-to-state ((state density-matrix-state) instr kraus-ops params)
  (let* ((kraus-sops (mapcar #'lift-matrix-to-superoperator kraus-ops))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr)))
         (ghosts (mapcar (alexandria:curry #'+ (num-qubits state)) qubits)))
   (multiple-value-bind (new-density temp-storage) (apply-superoperator kraus-sops
                                                                         (amplitudes state)
                                                                         (apply #'nat-tuple qubits)
                                                                         (apply #'nat-tuple ghosts)
                                                                         :temporary-storage (temporary-state state)
                                                                         :params params)
      (declare (ignore new-density))
      (setf (temporary-state state) temp-storage))))|#



