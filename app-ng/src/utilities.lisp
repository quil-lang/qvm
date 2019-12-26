;;;; app-ng/src/utilities.lisp
;;;;
;;;; Author: Robert Smith
;;;;         appleby

(in-package #:qvm-app-ng)

(defun get-random-state (arg)
  (etypecase arg
    (null (qvm:seeded-random-state nil))
    (unsigned-byte (qvm:seeded-random-state arg))))

;; Stolen from HUNCHENTOOT::ISO-TIME
(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun collect-memory-registers (qvm addresses)
  "Return a HASH-TABLE of exactly the QVM memory registers requested in ADDRESSES.

ADDRESSES is a HASH-TABLE where the keys are mem register names and the values are either T to indicate that we should return all indices for the corresponding register, or else a LIST of the desired indices for that register."
  (let ((results (make-hash-table :test 'equal)))
    (maphash (lambda (name indexes)
               (cond
                 ;; Give everything back.
                 ((eq indexes t)
                  (loop :with mv := (gethash name (qvm::classical-memories qvm))
                        :for idx :below (qvm::memory-view-length mv)
                        :collect (qvm:memory-ref qvm name idx) :into mem
                        :finally (push mem (gethash name results))))
                 ;; Give only some things back.
                 ((alexandria:proper-list-p indexes)
                  (loop :for idx :in indexes
                        :collect (qvm:memory-ref qvm name idx) :into mem
                        :finally (push mem (gethash name results))))
                 (t
                  (error "Invalid address parameter for memory named ~S." name))))
             addresses)
    results))

(defun simulation-method->qvm-type (simulation-method &key pauli-noise-p)
  "Return the QVM type for the given SIMULATION-METHOD and noise characteristics.

PAULI-NOISE-P indicates the presence or absence of Pauli channel noise."
  (ecase simulation-method
    (pure-state (if pauli-noise-p
                    'qvm:depolarizing-qvm
                    'qvm:pure-state-qvm))
    (full-density-matrix 'qvm:density-qvm)))

(defgeneric octets-required-for-qvm (qvm-type num-qubits)
  (:method ((qvm-type (eql 'qvm:pure-state-qvm)) num-qubits)
    (qvm:octets-required-for-qubits num-qubits))
  (:method ((qvm-type (eql 'qvm:depolarizing-qvm)) num-qubits)
    (qvm:octets-required-for-qubits num-qubits))
  (:method ((qvm-type (eql 'qvm:noisy-qvm)) num-qubits)
    ;; Space required for the AMPLITUDES, plus a copy in TRIAL-AMPLITUDES
    (* 2 (qvm:octets-required-for-qubits num-qubits)))
  (:method ((qvm-type (eql 'qvm:density-qvm)) num-qubits)
    ;; Space required for the 2^N x 2^N density matrix, plus possible duplicate in TEMPORARY-STATE.
    (* 2 (qvm:octets-required-for-qubits (* 2 num-qubits)))))

(defun run-program-on-qvm (qvm parsed-program &optional addresses)
  "Load and run PARSED-PROGRAM on the given QVM.

Return a HASH-TABLE of the requested classical memory ADDRESSES."
  (qvm:load-program qvm parsed-program :supersede-memory-subsystem t)
  (qvm:run qvm)
  (collect-memory-registers qvm (or addresses (make-hash-table))))
