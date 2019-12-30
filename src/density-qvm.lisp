;;;; src/density-qvm.lisp
;;;;
;;;; Authors: Robert Smith
;;;;          Erik Davis

(in-package #:qvm)

;;; This file implements density matrix evolution.

;;; General Overview

;;; The general approach taken here is that the DENSITY-QVM inherits
;;; most of its behavior from the PURE-STATE-QVM, but provides more
;;; specific methods for a few operations (namely TRANSITION, MEASURE,
;;; and MEASURE-ALL). A density matrix ρ of n qubits is represented in
;;; a PURE-STATE-QVM as a 2n qubit vector of amplitudes, vec(ρ). On
;;; the other hand, the DENSITY-QVM also maintains a 2^n x 2^n array
;;; displaced to vec(ρ), for convenience. See below for the definition
;;; of vec(ρ).

;;; Another feature of our implementation is that support for noisy
;;; operations and measurements mirrors the interface presented by
;;; NOISY-QVM. In particular, the DENSITY-QVM presents
;;; NOISY-GATE-DEFINITIONS and READOUT-POVMS slots, and has a similar
;;; interface for updating these slots (through the SET-NOISY-GATE and
;;; SET-READOUT-POVM methods, respectively).

(deftype density-operator-matrix-view ()
  "The matrix view of a density operator."
  `(and (array cflonum (* *))
        (not simple-array)))

(defclass density-qvm (base-qvm)
  ((noisy-gate-definitions :initarg :noisy-gate-definitions
                           :accessor noisy-gate-definitions
                           :initform (make-hash-table :test 'equalp))
   (readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table)))
  (:documentation "A density matrix simulator."))


;;; Creation and Initialization

(defmethod amplitudes ((qvm density-qvm))
  (state-elements (state qvm)))

(defmethod (setf amplitudes) (new-amplitudes qvm)
  (setf (state-elements (state qvm)) new-amplitudes))

(defmethod initialize-instance :after ((qvm density-qvm) &rest args)
  (declare (ignore args))
  (check-superoperators (alexandria:hash-table-values (noisy-gate-definitions qvm)))
  (check-all-povms (alexandria:hash-table-values (readout-povms qvm)))
  ;; PURE-STATE-QVM does its own allocation, which we don't want, so
  ;; here we make sure that the AMPLITUDES slot has a vector of the
  ;; right size (e.g. it was constructed by MAKE-DENSITY-QVM).
  (when (or (not (slot-boundp qvm 'state))
            (null (slot-value qvm 'state)))
    (%set-state (make-instance 'density-matrix-state :num-qubits (number-of-qubits qvm))
                qvm)
    (set-to-zero-state (state qvm))))


(defun make-density-qvm (num-qubits &key (allocation nil) &allow-other-keys)
  (make-instance 'density-qvm :number-of-qubits num-qubits 
                              :state (make-density-matrix-state 
                                      num-qubits  
                                      :allocation allocation)))


(defmethod reset-quantum-state ((qvm density-qvm))
  ;; It just so happens that the pure, zero state is the same in
  ;; this formalism, i.e., a 1 in the first entry.
  (bring-to-zero-state (amplitudes qvm))
  qvm)


;;; Superoperators

;;; Ordinary gates, as well as user-specified "Kraus operators", are
;;; represented by a SUPEROPERATOR type. The quil syntax for
;;; specifying Kraus operators is the same here as in the NOISY-QVM --
;;; namely, through pragmas a user may specify a "noisy gate" on a
;;; specific set of qubits, and during DENSITY-QVM evaluation such a
;;; noisy gate definition will replace the usual unitary one. The
;;; primary difference between the DENSITY-QVM and the NOISY-QVM in
;;; this regard is that application of a noisy gate in the DENSITY-QVM
;;; is completely deterministic and "folds all of the noisy" into the
;;; density matrix, whereas the NOISY-QVM is nondeterministic and
;;; tracks only a specific realization of the gate noise.

;;; XXX: These generics are specialized by NOISY-QVM as well. Maybe
;;; this should be defined as a protocol in another, separate file for
;;; ease of reading (so that folks writing new QVMs know to implement
;;; them).
(defgeneric set-noisy-gate (qvm gate-name qubits kraus-ops)
  (:documentation "Add noisy gate definition to QVM for a SIMPLE-GATE specified by
GATE-NAME in terms of the Kraus operator representation

   rho -> sum_{j=1}^n K_j rho K_j^H.

The argument KRAUS-OPS should hold the Kraus operators as list of
MAGICL matrices '(K1 K2 ... Kn)."))

(defgeneric set-readout-povm (qvm qubit povm)
  (:documentation "For a QUBIT belonging to a QVM specify a POVM to encode
possible readout errors.

POVM must be a 4-element list of double-floats."))

(defmethod set-noisy-gate ((qvm density-qvm) gate-name qubits kraus-ops)
  (check-kraus-ops kraus-ops)
  ;; Wrap a matrix in a gate in a superoperator...
  (setf (gethash (list gate-name qubits) (noisy-gate-definitions qvm))
        (kraus-list (mapcar #'ensure-superoperator kraus-ops))))

(defmethod set-readout-povm ((qvm density-qvm) qubit povm)
  (check-povm povm)
  (setf (gethash qubit (readout-povms qvm)) povm)
  nil)

(defmethod transition ((qvm density-qvm) (instr quil:gate-application))
  (assert (typep (quil:application-operator instr) 'quil:named-operator) ; TODO XXX support gate modifiers
          (instr)
          "The density QVM doesn't support gate modifiers.")
  (let*  ((gate (pull-teeth-to-get-a-gate instr))
          (params (mapcar (lambda (p) (force-parameter p qvm))
                          (quil:application-parameters instr)))
          (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))
    (apply #'apply-gate-to-state gate (state qvm) qubits params)
    (incf (pc qvm))
    qvm))


;;; Measurement

;;; In the PURE-STATE-QVM there is only one sensible meaning for a
;;; measurement: outcomes are sampled according to their respective
;;; probabilities, and then wavefunction collapse occurs. In the
;;; DENSITY-QVM, this could in principle be augmented by a second
;;; notion of measurement: namely, the outcome probabilities allow us
;;; to compute an "expected" outcome, which is generically a mixed
;;; state. The situation here is analogous to the question of gate
;;; noise, where one must choose between working with a specific
;;; realization of the noise process or its entire
;;; distribution. Whereas for noise we prefer the latter, for
;;; measurement we prefer the former, because i) it is necessary to
;;; force collapse when measurements are needed for classical control,
;;; and ii) it is what most people expect anyways.

(defun density-qvm-measurement-probabilities (qvm)
  "Computes the probability distribution of measurement outcomes (a vector)
  associated with the STATE of the DENSITY-QVM."
  (density-matrix-state-measurement-probabilities (state qvm)))

(defmethod apply-classical-readout-noise ((qvm density-qvm) (instr quil:measure))
  (%corrupt-qvm-memory-with-povm qvm instr (readout-povms qvm)))

(defmethod apply-classical-readout-noise ((qvm density-qvm) (instr quil:measure-discard))
  ;; Don't apply readout noise for a MEASURE-DISCARD.
  (declare (ignore qvm instr))
  nil)

(defmethod measure-all ((qvm density-qvm))
  (multiple-value-bind (qvm-ret measured-bits)
      (naive-measure-all qvm)
    (values
     qvm-ret
     (perturb-measured-bits qvm-ret measured-bits (readout-povms qvm)))))

(defmethod transition :around ((qvm density-qvm) (instr quil:measurement))
  ;; perform actual measurement
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))

;;; Don't compile things for the density-qvm.
(defmethod compile-loaded-program ((qvm density-qvm))
  qvm)

;;; TODO: FIXME: we should be able to compile density operator stuff
;;; just fine.
(defmethod compile-instruction ((qvm density-qvm) isn)
  (declare (ignore qvm))
  isn)
