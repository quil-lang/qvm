(in-package #:qvm)

;; This is the state protocol for setting the initial state to the zero state.
(defgeneric set-to-zero-state (state)
  (:documentation "Set the initial state to the pure zero state."))

(defgeneric requires-swapping-amps-p (state))

(defgeneric swap-internal-amplitude-pointers (state))


(defclass quantum-system-state ()
  ((allocation
    :reader allocation
    :initarg :allocation))
  (:metaclass abstract-class))

(defclass pure-state (quantum-system-state)
  ((amplitudes
    :accessor amplitudes
    :initarg :amplitudes)
   (trial-amplitudes
    :accessor %trial-amplitudes
    :initarg :trial-amplitudes
    :documentation "A second wavefunction used when applying a noisy quantum channel. Applying a Kraus map generally requires evaluating psi_j = K_j * psi for several different j, making it necessary to keep the original wavefunction around.  This value should be a QUANTUM-STATE whose size is compatible with the number of qubits of the CHANNEL-QVM. The actual values can be initialized in any way because they will be overwritten. As such, it merely is scratch space for intermediate computations, and hence should not be otherwise directly accessed.")
   (original-amplitudes
    :reader original-amplitudes
    :documentation  "A reference to the original pointer of amplitude memory, so the amplitudes can sit in the right place at the end of a computation.")))

(defun make-pure-state (num-qubits &key (allocation nil))
  "ALLOCATION is an optional argument with the following behavior.
    - If it's NULL (default), then a standard wavefunction in the Lisp heap will be allocated.
    - If it's a STRING, then the wavefunction will be allocated as a shared memory object, accessible by that name.
    - Otherwise, it's assumed to be an object that is compatible with the ALLOCATION-LENGTH and ALLOCATE-VECTOR methods
    - will probs have to redo this in multiple places, have a helper function do the allocation stuff"
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
      (let ((state (make-instance 'pure-state :num-qubits num-qubits 
                                              :amplitudes amplitudes 
                                              :allocation allocation)))
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
      ;; Initialize the AMPLITUDES and TRIAL-AMPLITUDES to an empty
      ;; array of the correct size.
      (amplitudes state) (make-lisp-cflonum-vector (expt 2 num-qubits)))))
  (setf
     (%trial-amplitudes state) (make-lisp-cflonum-vector (expt 2 num-qubits))
      ;; Save a pointer to the originally provided memory.
      (slot-value state 'original-amplitudes) (amplitudes state)))

(defmethod num-qubits ((state pure-state))
  (quil:ilog2 (length (amplitudes state))))

(defmethod set-to-zero-state ((state pure-state))
  (bring-to-zero-state (amplitudes state)))

(defmethod requires-swapping-amps-p ((state pure-state))
  "Does the state require swapping of internal pointers?"
  (and (not (eq (amplitudes state) (original-amplitudes state)))
       #+sbcl (eq ':foreign (sb-introspect:allocation-information
                             (original-amplitudes state)))))

(defmethod swap-internal-amplitude-pointers ((state pure-state))
  ;; Copy the correct amplitudes into place.
  (copy-wavefunction (amplitudes state) (original-amplitudes state))
  ;; Get the pointer back in home position. We want to swap them,
  ;; not overwrite, because we want the scratch memory to be intact.
  (rotatef (amplitudes state) (%trial-amplitudes state)))

  

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

(defmethod initialize-instance :after ((state density-matrix-state) &rest args)
  (declare (ignore args))
  (let ((dim (expt 2 (num-qubits state))))
    (setf (slot-value state 'matrix-view)
          (make-array (list dim dim)
                      :element-type 'cflonum
                      :displaced-to (amplitudes state)))))


(defmethod num-qubits ((state density-matrix-state))
  (/ (quil:ilog2 (length (amplitudes state))) 2))


(defmethod (setf amplitudes) :after (new-value (state density-matrix-state))
  (let ((dim (expt 2 (num-qubits state))))
    (setf (slot-value state 'matrix-view) (make-array (list dim dim)
                                                      :element-type 'cflonum
                                                      :displaced-to new-value))))


(defmethod set-to-zero-state ((state density-matrix-state))
  (bring-to-zero-state (amplitudes state)))

(defun make-density-matrix-state (num-qubits &key (allocation nil))
  ;; The amplitudes store vec(ρ), i.e. the entries of the density
  ;; matrix ρ in row-major order. For a system of N qubits, ρ has
  ;; dimension 2^N x 2^N, hence a total of 2^(2N) entries.

  ;; The initial state is the pure zero state, which is
  ;; represented by all zero entries except for a 1 in the first
  ;; position. See also RESET-QUANTUM-STATE, which we avoid
  ;; calling here because it performs an additional full traversal
  ;; of the vector.
  (let* ((expected-size (expt 2 (* 2 num-qubits)))
         ;; See also MAKE-QVM for this kind of code.
         (allocation
           (etypecase allocation
             (null
              (make-instance 'lisp-allocation :length expected-size))
             (string
              (make-instance 'posix-shared-memory-allocation :length expected-size
                                                             :name allocation))
             (t
              (assert (= (expt 2 (* 2 num-qubits)) (allocation-length allocation)))
              allocation))))
    (multiple-value-bind (matrix-entries finalizer)
        (allocate-vector allocation) 
      ;; Go into the zero state.
      (setf (aref matrix-entries 0) (cflonum 1)) 
      (let ((state (make-instance 'density-matrix-state
                                :amplitudes matrix-entries
                                :allocation allocation)))
        (tg:finalize state finalizer)
        state))))


(defmethod requires-swapping-amps-p ((state density-matrix-state))
  ;; skip for density-matrix-state
  (declare (ignore state)))


(defmethod swap-internal-amplitude-pointers ((state density-matrix-state))
  ;; skip for density-matrix-state
  (declare (ignore state)))


