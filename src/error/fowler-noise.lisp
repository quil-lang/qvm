;;;; fowler-noise.lisp
;;;;
;;;; This file is a reparametrization of `DEPOLARIZING-NOISE-QVM', further
;;;; extended over the stabilizer QVM.  The reparameterization is to match
;;;; Fowler's convention: a set of flags toggle whether noise is applied after
;;;; various kinds of gates (see `FOWLER-NOISE'), a single parameter determining
;;;; the probability of whether noise is _not_ applied after a gate, and in the
;;;; complementary case uniform probability of the different sorts of Pauli
;;;; flips one can apply.
;;;;
;;;; Fowler further groups the flags into "classes" which assume that one is
;;;; specifically simulating upkeep for a surface code.  Rather than make that
;;;; assumption here, we let users implement them as they see fit.

(in-package #:qvm.error)

(deftype fowler-noise ()
  "Flag enumeration for different kinds of depolarizing noise, following Fowler's classification convention.

HIGH |    4    |   3   |  2 |  1 |   0  | LOW
---------------------------------------------
     | READOUT | RESET | 2Q | 1Q | IDLE |"
  '(unsigned-byte 5))

(defclass fowler-qvm ()
  ((noise-probability
    :initarg :noise-probability
    :accessor fowler-qvm-noise-probability
    :type (real 0 1)
    :documentation "Occurrence probability of a variety of noise events.")
   (noise-class
    :initarg :noise-class
    :initform 31
    :accessor fowler-qvm-noise-class
    :type fowler-noise
    :documentation "Noise events of class <= NOISE-CLASS will be applied."))
  (:metaclass abstract-class)
  (:documentation "A quantum virtual machine with noise as specified on page 11 of /1208.0928.

This noise model carries a set of flags that select certain subsets of operations (see `FOWLER-NOISE').  When a flag is set, this noise model applies spatially-uniform depolarizing noise to the corresponding operations.")
  (:default-initargs :noise-probability 0d0))

(defclass fowler-pure-state-qvm (fowler-qvm qvm::pure-state-qvm)
  ())

(defclass fowler-stabilizer-qvm (fowler-qvm qvm::stabilizer-qvm)
  ())

(defmethod copy-fowler-qvm ((qvm fowler-pure-state-qvm))
  "Makes a \"deep copy\" of (the quantum part of) `QVM', of type `FOWLER-PURE-STATE-QVM'.

WARNING: Deep copy does _not_ include the classical memory subsystem. Writes to the returned QVM's classical memory will be visible to the original QVM's classical memory, and vice versa."
  (make-instance 'fowler-pure-state-qvm
                 :noise-class (fowler-qvm-noise-class qvm)
                 :noise-probability (fowler-qvm-noise-probability qvm)
                 :number-of-qubits (number-of-qubits qvm)
                 :state (make-instance 'pure-state
                                       :num-qubits (number-of-qubits qvm)
                                       :amplitudes (alexandria:copy-array
                                                    (qvm::amplitudes (qvm::state qvm))))
                 :gate-definitions (qvm::gate-definitions qvm)
                 :wait-function (qvm::wait-function qvm)
                 :classical-memory-subsystem (qvm::classical-memory-subsystem qvm)))

(defmethod copy-fowler-qvm ((qvm fowler-stabilizer-qvm))
  "Makes a \"deep copy\" of (the quantum part of) `QVM', of type `FOWLER-STABILIZER-QVM'.

WARNING: Deep copy does _not_ include the classical memory subsystem. Writes to the returned QVM's classical memory will be visible to the original QVM's classical memory, and vice versa."
  (make-instance 'fowler-stabilizer-qvm
                 :noise-class (fowler-qvm-noise-class qvm)
                 :noise-probability (fowler-qvm-noise-probability qvm)
                 :tableau (alexandria:copy-array (qvm::stabilizer-qvm-tableau qvm))
                 :gate-definitions (qvm::gate-definitions qvm)
                 :wait-function (qvm::wait-function qvm)
                 :classical-memory-subsystem (qvm::classical-memory-subsystem qvm)))

(defmethod compile-loaded-program ((qvm fowler-qvm))
  qvm)

(define-condition unknown-depolarizing-effect (error)
   ((instruction :reader unknown-depolarizing-effect-instruction :initarg :instruction))
   (:report (lambda (condition stream)
              (format stream "FOWLER-QVM doesn't know how to follow ~a with depolarizing noise" 
                      (unknown-depolarizing-effect-instruction condition)))))

;;;
;;; read individual flags out of the error-type bitvector
;;;

(declaim (inline fowler-qvm-noise-identity?
                 fowler-qvm-noise-1Q-gate?
                 fowler-qvm-noise-2Q-gate?
                 fowler-qvm-noise-reset?
                 fowler-qvm-noise-readout?))

(defun fowler-qvm-noise-identity? (qvm)
  (logbitp 0 (fowler-qvm-noise-class qvm)))

(defun fowler-qvm-noise-1Q-gate? (qvm)
  (logbitp 1 (fowler-qvm-noise-class qvm)))

(defun fowler-qvm-noise-2Q-gate? (qvm)
  (logbitp 2 (fowler-qvm-noise-class qvm)))

(defun fowler-qvm-noise-reset? (qvm)
  (logbitp 3 (fowler-qvm-noise-class qvm)))

(defun fowler-qvm-noise-readout? (qvm)
  (logbitp 4 (fowler-qvm-noise-class qvm)))

;;;
;;; basic qvm behavior definitions
;;;

(defgeneric %fast-apply (qvm instr-name qubit)
  (:documentation "Convenience routine for applying a gate in a way that dodges an active noise model."))

(defmethod %fast-apply ((qvm fowler-pure-state-qvm) instr-name qubit)
  (let* ((gate (quil:gate-definition-to-gate (quil:lookup-standard-gate instr-name)))
         (pure-state (qvm::state qvm)))
    (apply-gate-to-state gate pure-state (list (quil:qubit qubit)))))

(defmethod %fast-apply ((qvm fowler-stabilizer-qvm) instr-name qubit)
  (let* ((instr (quil::build-gate instr-name () qubit))
         (clifford (qvm::gate-application-to-clifford instr)))
    (apply (qvm::compile-clifford clifford)
           (qvm::stabilizer-qvm-tableau qvm)
           (list qubit))))

;;;
;;; noise behavior definitions
;;;

(defmethod transition :after ((qvm fowler-qvm) (instr cl-quil:application))
  (case (length (cl-quil::application-arguments instr))
    (1
     (cond
       ((equalp (quil::named-operator "I")
                (cl-quil::application-operator instr))
        (when (fowler-qvm-noise-identity? qvm)
          (let ((qubit (quil:qubit-index (first (cl-quil::application-arguments instr))))
                (p/3 (* 1/3 (fowler-qvm-noise-probability qvm))))
            (multiprobabilistically
              (p/3 (%fast-apply qvm "X" qubit))
              (p/3 (%fast-apply qvm "Y" qubit))
              (p/3 (%fast-apply qvm "Z" qubit))))))
       ((equalp (quil::named-operator "H")
                (cl-quil::application-operator instr))
        (when (fowler-qvm-noise-1Q-gate? qvm)
          (let ((qubit (quil:qubit-index (first (cl-quil::application-arguments instr))))
                (p/3 (/ (fowler-qvm-noise-probability qvm) 3)))
            (multiprobabilistically
              (p/3 (%fast-apply qvm "X" qubit))
              (p/3 (%fast-apply qvm "Y" qubit))
              (p/3 (%fast-apply qvm "Z" qubit))))))
       (t
        (warn "FOWLER-QVM doesn't know how to follow ~a with depolarizing noise"
              instr))))
    (2
     (cond
       ((equalp (quil::named-operator "CNOT")
                (cl-quil::application-operator instr))
        (when (fowler-qvm-noise-2Q-gate? qvm)
          ;; apply a non-II pauli each w/ probability p/15
          (let ((p (quil:qubit-index (first  (cl-quil::application-arguments instr))))
                (q (quil:qubit-index (second (cl-quil::application-arguments instr))))
                (p/15 (* 1/15 (fowler-qvm-noise-probability qvm))))
            (multiprobabilistically
              (p/15 (%fast-apply qvm "X" p))
              (p/15 (%fast-apply qvm "Y" p))
              (p/15 (%fast-apply qvm "Z" p))
              (p/15 (%fast-apply qvm "X" q))
              (p/15 (%fast-apply qvm "Y" q))
              (p/15 (%fast-apply qvm "Z" q))
              (p/15 (%fast-apply qvm "X" p) (%fast-apply qvm "X" q))
              (p/15 (%fast-apply qvm "X" p) (%fast-apply qvm "Y" q))
              (p/15 (%fast-apply qvm "X" p) (%fast-apply qvm "Z" q))
              (p/15 (%fast-apply qvm "Y" p) (%fast-apply qvm "X" q))
              (p/15 (%fast-apply qvm "Y" p) (%fast-apply qvm "Y" q))
              (p/15 (%fast-apply qvm "Y" p) (%fast-apply qvm "Z" q))
              (p/15 (%fast-apply qvm "Z" p) (%fast-apply qvm "X" q))
              (p/15 (%fast-apply qvm "Z" p) (%fast-apply qvm "Y" q))
              (p/15 (%fast-apply qvm "Z" p) (%fast-apply qvm "Z" q))))))
       (t
        (cerror "FOWLER-QVM doesn't know how to follow ~a with depolarizing noise"
                'unknown-depolarizing-effect :instruction instr))))
    (otherwise
     (cerror "FOWLER-QVM doesn't know how to follow ~a with depolarizing noise"
             'unknown-depolarizing-effect :instruction instr)))
  qvm)

(defmethod transition :after ((qvm fowler-qvm) (instr cl-quil:reset-qubit))
  ;; attempt to initialize to |g>, but initialize instead to |e> w/ probability p
  (when (fowler-qvm-noise-reset? qvm)
    (qvm::probabilistically (fowler-qvm-noise-probability qvm)
      (let ((qubit (quil:qubit-index (quil::reset-qubit-target instr))))
        (%fast-apply qvm "X" qubit))))
  qvm)

(defmethod transition :before ((qvm fowler-qvm) (instr cl-quil:measurement))
  ;; attempt to perform Z-meas, but report and project to wrong state w/ probability p
  (when (fowler-qvm-noise-readout? qvm)
    (qvm::probabilistically (fowler-qvm-noise-probability qvm)
      (let ((qubit (quil:qubit-index (cl-quil:measurement-qubit instr))))
        (%fast-apply qvm "X" qubit))))
  qvm)
