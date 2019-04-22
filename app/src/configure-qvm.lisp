;;;; src/configure-qvm.lisp
;;;;
;;;; Author: Nikolas Tezak
;;;;         Robert Smith

(in-package #:qvm-app)

(defun throw-error-if-over-allocated (num-qubits)
  "Throws an error if the number of qubits requested exceeds the max (defined from command line parameter --qubit-limit)."
  (when (and (integerp *qubit-limit*) (> num-qubits *qubit-limit*))
    (error "~D qubit~:P were requested, but the QVM ~
            is limited to ~D qubit~:P." num-qubits *qubit-limit*)))

(defun make-wavefunction (length)
  (multiple-value-bind (vec fin)
      (allocate-vector (funcall **default-allocation** length))
    (qvm::bring-to-zero-state vec)
    (values vec fin)))

(defun make-appropriate-qvm (quil num-qubits gate-noise measurement-noise)
  "Determine if a QVM:NOISY-QVM or QVM:DEPOLARIZING-QVM is needed."
  (throw-error-if-over-allocated num-qubits)
  (format-log "Making qvm of ~D qubit~:P" num-qubits)
  (let* ((kraus-ops (extract-kraus-ops quil))
         (readout-povms (extract-readout-povms quil))
         (need-noisy-qvm (or (plusp
                              (hash-table-count kraus-ops))
                             (plusp
                              (hash-table-count readout-povms))))
         (classical-memory-model (qvm::memory-descriptors-to-qvm-memory-model
                                  (quil:parsed-program-memory-definitions quil))))
    (cond
      ((and (null gate-noise)
            (null measurement-noise)
            (not need-noisy-qvm))
       (qvm:make-qvm num-qubits :classical-memory-model classical-memory-model
                                :allocation (funcall **default-allocation**
                                                     (expt 2 num-qubits))))
      (need-noisy-qvm
       (when (not (and (null gate-noise)
                       (null measurement-noise)))
         (error "Cannot simultaneously support Pauli error model and Kraus channels."))

       (multiple-value-bind (amps fin) (make-wavefunction (expt 2 num-qubits))
         (let ((q
                 (make-instance 'qvm:noisy-qvm
                                :number-of-qubits num-qubits
                                :noisy-gate-definitions kraus-ops
                                :readout-povms readout-povms
                                :classical-memory-subsystem (make-instance 'qvm:classical-memory-subsystem
                                                                           :classical-memory-model
                                                                           classical-memory-model)
                                :amplitudes amps)))
           (when fin (tg:finalize q fin))
           q)))
      (t
       (let ((gate-noise (or gate-noise '(0.0 0.0 0.0)))
             (measurement-noise (or measurement-noise '(0.0 0.0 0.0))))
         (multiple-value-bind (amps fin) (make-wavefunction (expt 2 num-qubits))
           (let ((q (make-instance 'qvm:depolarizing-qvm
                                   :number-of-qubits num-qubits
                                   :classical-memory-subsystem (make-instance 'qvm:classical-memory-subsystem
                                                                              :classical-memory-model
                                                                              classical-memory-model)
                                   :x (elt gate-noise 0)
                                   :y (elt gate-noise 1)
                                   :z (elt gate-noise 2)
                                   :measure-x (elt measurement-noise 0)
                                   :measure-y (elt measurement-noise 1)
                                   :measure-z (elt measurement-noise 2)
                                   :amplitudes amps)))
             (when fin (tg:finalize q fin))
             q)))))))


(defun extract-kraus-ops (quil)
  "Iterate over the instructions in QUIL, collect all PRAGMA ADD-KRAUS
statements and convert them into a hash-table with keys
'(<gatename> (<qubit1>+)) and as values a list of MAGICL matrices.
"
  (let ((kraus-ops (make-hash-table :test 'equal)))
    (loop :for instr :across (quil:parsed-program-executable-code quil)
          :when (typep instr 'cl-quil::pragma-add-kraus)
            :do (let ((existing-ops (gethash (list (cl-quil::pragma-operator-name instr)
                                                   (cl-quil::pragma-qubit-arguments instr))
                                             kraus-ops))
                      (d (expt 2 (length (cl-quil::pragma-qubit-arguments instr)))))
                  (setf (gethash (list (cl-quil::pragma-operator-name instr)
                                       (cl-quil::pragma-qubit-arguments instr))
                                 kraus-ops)
                        (append existing-ops
                                (list (cl-quil::make-row-major-matrix
                                       d d
                                       (cl-quil::pragma-matrix-entries instr))))))
                ;; change to no-op to prevent the QVM from logging
                ;; warnings about these pragmas
                (change-class instr 'cl-quil:no-operation))
    kraus-ops))

(defun extract-readout-povms (quil)
  "Iterate over the instructions in QUIL, collect all PRAGMA
READOUT-POVM statements and convert them into a hash-table with keys
given by the qubit id and as values MAGICL matrices that encode the
single qubit readout povm.
"
  (let ((readout-povms (make-hash-table :test 'equal)))
    (loop :for instr :across (quil:parsed-program-executable-code quil)
          :when (typep instr 'cl-quil::pragma-readout-povm)
            :do (setf (gethash (cl-quil::pragma-qubit-index instr) readout-povms)
                      (cl-quil::pragma-matrix-entries instr))
                ;; change to no-op to prevent the QVM from logging
                ;; warnings about these pragmas
                (change-class instr 'cl-quil:no-operation))
    readout-povms))


(defgeneric overwrite-execution-parameters-according-to-program
    (qvm program &key &allow-other-keys)
  ;; We want to execute every available method without playing funny
  ;; games using :BEFORE/:AFTER/:AROUND in the standard method
  ;; combination.
  (:method-combination progn)
  (:documentation "Overwrite execution parameters of the QVM according to the program, and any other keys of data. (This is used, for example, to extract and set the memory model or noise parameters.)"))

;;; Overwrite the program and memory subsystem.
(defmethod overwrite-execution-parameters-according-to-program progn
  ((qvm qvm:pure-state-qvm) program &key &allow-other-keys)
  ;; XXX: We may want to keep the classical memory across runs!
  (load-program qvm program :supersede-memory-subsystem t)
  nil)

(defun %load-noisy-parameters (qvm program)
  (setf (slot-value qvm 'qvm::noisy-gate-definitions) (extract-kraus-ops program)
        (slot-value qvm 'qvm::readout-povms)          (extract-readout-povms program))
  nil)

;;; Overwrite the noisy operators.
(defmethod overwrite-execution-parameters-according-to-program progn
  ((qvm qvm:noisy-qvm) program &key &allow-other-keys)
  (%load-noisy-parameters qvm program)
  nil)

(defmethod overwrite-execution-parameters-according-to-program progn
  ((qvm qvm:density-qvm) program &key &allow-other-keys)
  (%load-noisy-parameters qvm program)
  ;; EXTRACT-KRAUS-OPS gives us matrices, but DENSITY-QVM is expecting superoperators
  ;; Time to convert
  (loop :with kraus-ops := (qvm::noisy-gate-definitions qvm)
        :for key :being :the :hash-keys :of kraus-ops
        :do (setf (gethash key kraus-ops)
                  (qvm::kraus-list (mapcar #'qvm::lift-matrix-to-superoperator
                                           (gethash key kraus-ops)))))
  nil)

(defmethod overwrite-execution-parameters-according-to-program progn
  ((qvm qvm:depolarizing-qvm) program &key gate-noise measurement-noise &allow-other-keys)
  (declare (ignore program))
  ;; Only overwrite if we were provided with something.
  (when (or gate-noise measurement-noise)
    (let ((gate-noise        (or gate-noise        '(0.0 0.0 0.0)))
          (measurement-noise (or measurement-noise '(0.0 0.0 0.0))))
      (setf (qvm::probability-gate-x qvm)    (elt gate-noise 0)
            (qvm::probability-gate-y qvm)    (elt gate-noise 1)
            (qvm::probability-gate-z qvm)    (elt gate-noise 2)
            (qvm::probability-measure-x qvm) (elt measurement-noise 0)
            (qvm::probability-measure-y qvm) (elt measurement-noise 1)
            (qvm::probability-measure-z qvm) (elt measurement-noise 2))
      nil)))
