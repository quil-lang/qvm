;;;; src/configure-qvm.lisp
;;;;
;;;; Author: Nikolas Tezak

(in-package #:qvm-app)

(defun throw-error-if-over-allocated (num-qubits)
  "Throws an error if the number of qubits requested exceeds the max (defined from command line parameter --qubit-limit)."
  (when (and (integerp *qubit-limit*) (> num-qubits *qubit-limit*))
    (error "~D qubits were requested, but the QVM ~
             is limited to ~D qubits." num-qubits *qubit-limit*))
  (when (and **persistent-wavefunction**
             (> num-qubits (qvm::wavefunction-qubits **persistent-wavefunction**)))
    (error "~D qubits were requested, but the persistent QVM ~
            was specified to only have ~D. Consider increasing it."
           num-qubits
           (qvm::wavefunction-qubits **persistent-wavefunction**))))

(defgeneric make-appropriate-qvm (simulation-method quil num-qubits gate-noise measurement-noise))

(defmethod make-appropriate-qvm ((simulation-method (eql 'pure-state)) quil num-qubits gate-noise measurement-noise)
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
       (make-instance 'qvm:pure-state-qvm
                      :number-of-qubits num-qubits
                      :classical-memory-subsystem (make-instance 'qvm:classical-memory-subsystem
                                                                 :classical-memory-model
                                                                 classical-memory-model)
                      :amplitudes **persistent-wavefunction**)) ; Could be NIL.
      (need-noisy-qvm
       (when (not (and (null gate-noise)
                       (null measurement-noise)))
         (error "Cannot simultaneously support Pauli error model and Kraus channels."))
       (make-instance 'qvm:noisy-qvm
                      :number-of-qubits num-qubits
                      :noisy-gate-definitions kraus-ops
                      :readout-povms readout-povms
                      :classical-memory-subsystem (make-instance 'qvm:classical-memory-subsystem
                                                                 :classical-memory-model
                                                                 classical-memory-model)
                      :amplitudes **persistent-wavefunction**))
      (t
       (let ((gate-noise (or gate-noise '(0.0 0.0 0.0)))
             (measurement-noise (or measurement-noise '(0.0 0.0 0.0))))
         (make-instance 'qvm:depolarizing-qvm
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
                        :amplitudes **persistent-wavefunction**))))))



(defmethod make-appropriate-qvm ((simulation-method (eql 'full-density-matrix)) quil num-qubits gate-noise measurement-noise)
  (when (and (integerp *qubit-limit*) (> num-qubits *qubit-limit*))
    (error "~D qubits were requested, but the QVM ~
             is limited to ~D qubits." num-qubits *qubit-limit*))
  (when (and **persistent-wavefunction**
             (> num-qubits
                (qvm::full-density-number-of-qubits **persistent-wavefunction**)))
    (error "~D qubits were requested, but the persistent QVM ~
            was specified to only have ~D. Consider increasing it."
           num-qubits
           (qvm::full-density-number-of-qubits **persistent-wavefunction**)))
  (let* ((kraus-ops (extract-kraus-ops quil))
         (readout-povms (extract-readout-povms quil))
         (classical-memory-model (qvm::memory-descriptors-to-qvm-memory-model
                                  (quil:parsed-program-memory-definitions quil))))

    ;; EXTRACT-KRAUS-OPS gives us matrices, but DENSITY-QVM is expecting superoperators
    ;; Time to convert
    (loop :for key :being :the :hash-keys :of kraus-ops
          :do (setf (gethash key kraus-ops)
                    (qvm::kraus-list (mapcar #'qvm::lift-matrix-to-superoperator
                                             (gethash key kraus-ops)))))

    (let ((memory (make-instance 'qvm:classical-memory-subsystem
                                 :classical-memory-model
                                 classical-memory-model)))
      (if **persistent-wavefunction**
          (make-instance 'qvm:density-qvm
                         ;; if the persistent wf is allocated, we need to use the whole thing
                         :number-of-qubits (qvm::full-density-number-of-qubits **persistent-wavefunction**)
                         :noisy-gate-definitions kraus-ops
                         :readout-povms readout-povms
                         :classical-memory-subsystem memory
                         :amplitudes **persistent-wavefunction**)
          (qvm::make-density-qvm num-qubits
                                 :noisy-gate-definitions kraus-ops
                                 :readout-povms readout-povms
                                 :classical-memory-subsystem memory)))))



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
