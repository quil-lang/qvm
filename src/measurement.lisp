;;;; measurement.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(declaim (inline index-to-address))
(defun index-to-address (index qubit state)
  "Given an amplitude index INDEX, find the amplitude address for the INDEX'th basis state with QUBIT in the STATE state.

Specifically, given an integer whose bit string is

    INDEX = LLLLRRRR,

compute the address

    Result = LLLL{0,1}RRRR

which is the index with a {1, 0} injected at the QUBIT'th position."
  (declare (type nat-tuple-element qubit)
           (type amplitude-address index)
           (type bit state)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((LLLLb (let ((LLLL (ash index (- qubit))))
                 (declare (type amplitude-address LLLL))
                 (logior state (the amplitude-address (ash LLLL 1)))))
        (RRRR  (ldb (byte qubit 0) index)))
    (declare (type amplitude-address LLLLb RRRR))
    (logior (the amplitude-address (ash LLLLb qubit))
            RRRR)))

(defun qubit-probability (qvm qubit)
  "The probability that the physical qubit addressed by QUBIT is 1."
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (inline probability psum-range))
  (let ((wavefunction (amplitudes qvm)))
    (declare (type quantum-state wavefunction))
    (psum-dotimes (i (ash (length wavefunction) -1))
      (let ((address (index-to-address i qubit 1)))
        (declare (type amplitude-address address))
        (probability (aref wavefunction address))))))

(defun multi-qubit-probability (qvm &rest qubits)
  "Compute the probability that the qubits QUBITS will measure to 1 within the quantum virtual machine QVM."
  (first (last (state-probabilities qvm (apply #'nat-tuple qubits)))))

(defun force-measurement (measured-value qubit qvm)
  "Force the quantum system QVM to have the qubit QUBIT collapse/measure to MEASURED-VALUE. Modify the amplitudes of all other qubits accordingly."
  (declare (optimize speed (safety 0) (debug 0) (space 0))
           (type bit measured-value))
  (let ((wavefunction (amplitudes qvm))
        (annihilated-state (- 1 measured-value)))
    (declare (type quantum-state wavefunction)
             (type bit annihilated-state))
    ;; Here we step through the wavefunction amplitudes
    ;;
    ;;     |...xyzaMbc...>
    ;;             ^ M = 1 - MEASURED-VALUE
    ;;
    ;; and set them to zero. The old way to do this was to do it as a
    ;; projective measurement.
    (if (<= *qubits-required-for-parallelization* (number-of-qubits qvm))
        (lparallel:pdotimes (i (ash (length wavefunction) -1))
          (let ((address (index-to-address i qubit annihilated-state)))
            (declare (type amplitude-address address))
            (setf (aref wavefunction address) (cflonum 0))))
        (dotimes (i (ash (length wavefunction) -1))
          (let ((address (index-to-address i qubit annihilated-state)))
            (declare (type amplitude-address address))
            (setf (aref wavefunction address) (cflonum 0)))))

    (normalize-wavefunction wavefunction))

  ;; Return the QVM.
  qvm)

(defun measure (qvm q c)
  "Non-deterministically perform a measurement on the qubit addressed by Q in the quantum virtual machine QVM. Store the bit at the classical bit memory address C. If C is instead NIL, don't store.

Return two values:

    1. The resulting QVM.
    2. The measured classical bit."
  (let* ((r (random 1.0d0))
         (cbit (if (<= r (qubit-probability qvm q))
                   1
                   0)))
    ;; Force the non-deterministic measurement.
    (force-measurement cbit q qvm)

    ;; Store the classical bit if necessary.
    (unless (null c)
      (setf (classical-bit qvm c) cbit))

    ;; Return the qvm.
    (values qvm cbit)))

(defun parallel-measure (qvm qubits)
  "Perform a measurement on the list of qubits QUBITS within the QVM, returning two values:

    1. The resulting QVM.
    2. A list of the measurements in the same order as the provided qubits."
  (loop :for q :in qubits
        :collect (multiple-value-bind (new-qvm cbit)
                     (measure qvm q nil)
                   (setf qvm new-qvm)
                   cbit) :into measurements
        :finally (return (values qvm measurements))))
