;;;; lazy-entangle.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;;; This file contains code to efficiently work with qubit subsystems
;;;; (uncorrelated states).

(defstruct (subsystem (:predicate subsystemp))
  "Representation of the state of a subsystem of qubits."
  ;; STATE is a quantum operator expressed in the standard computation
  ;; basis of QUBITS. For example, if QUBITS is #b101, then the basis
  ;; of STATE is:
  ;;
  ;;     |0⟩₂ ⊗ |0⟩₀
  ;;     |0⟩₂ ⊗ |1⟩₀
  ;;     |1⟩₂ ⊗ |0⟩₀
  ;;     |1⟩₂ ⊗ |1⟩₀
  (state  (wf 1) :type quantum-state :read-only t)
  ;; QUBITS is a bit-set. That is, the Nth bit of QUBITS is 1 if this
  ;; subsystem contains qubit N.
  (qubits 0      :type unsigned-byte :read-only t))

(defun make-subsystem-on-qubits (&rest qubits)
  "Make a subsystem (in the zero-state) for the qubits QUBITS.

Note: Duplicate qubits are ignored."
  (declare (dynamic-extent qubits))
  (loop :with qubit-set := 0
        :for q :in qubits
        :do (setf qubit-set (dpb 1 (byte 1 q) qubit-set))
        :finally (return
                   (make-subsystem :qubits qubit-set
                                   :state (let ((state (make-lisp-cflonum-vector
                                                        (expt 2 (logcount qubit-set)))))
                                            (setf (aref state 0) (cflonum 1))
                                            state)))))

(defun subsystem-num-qubits (ss)
  "How many qubits does the subsystem SS represent?"
  (logcount (subsystem-qubits ss)))

(defmethod print-object ((ss subsystem) stream)
  (print-unreadable-object (ss stream :type t :identity nil)
    (let ((qubits (subsystem-qubits ss)))
      (format stream "~Dq" (subsystem-num-qubits ss))
      (when (plusp qubits)
        (format stream ":")
        (loop :for i :below (integer-length (subsystem-qubits ss))
              :when (logbitp i qubits)
                :do (format stream " ~D" i))))))

(defun subsystem-contains-qubit-p (ss qubit)
  "Does the subsystem SS contain the qubits QUBIT?"
  (logbitp qubit (subsystem-qubits ss)))

(defun subsystem-contains-qubits-p (ss qubit-set)
  "Does the subsystem SS contain the qubits designated by the bit-set QUBIT-SET?"
  (zerop (logandc1 (subsystem-qubits ss) qubit-set)))

(defun subsystem-physical-to-logical-qubit (ss phys-qubit)
  "What logical qubit number does the physical qubit PHYS-QUBIT have in the subsystem SS?

(The  \"logical qubit\" is the relative position of the qubit in the system.)"
  (and (subsystem-contains-qubit-p ss phys-qubit)
       (logcount (ldb (byte phys-qubit 0) (subsystem-qubits ss)))))

(defun %collapse-integer-by-mask (integer mask)
  "Let INTEGER and MASK be integers of the same bit-length. Remove the bits of INTEGER that correspond to the zeros of MASK."
  (loop :with collapsed := 0
        :with j := 0
        :for i :below (integer-length mask)
        :when (logbitp i mask)
          :do (setf collapsed (dpb (ldb (byte 1 i) integer)
                                   (byte 1 j) collapsed))
              (incf j)
        :finally (return collapsed)))

;;; Maybe a future TODO:
;;;
;;;    - Allow JOIN-SUBSYSTEMS to take any number of args.
;;;
;;;    - Allow EJECT-QUBIT-FROM-SUBSYSTEM to take many qubits.

(defun join-subsystems (ss1 ss2)
  "Join two (disjoint) qubit subsystems SS1 and SS2 into a larger qubit subsystem."
  (let* ((q1 (subsystem-qubits ss1))
         (q2 (subsystem-qubits ss2))
         (new-qubits (logior q1 q2))
         (collapsed-q1 (%collapse-integer-by-mask q1 new-qubits))
         (collapsed-q2 (%collapse-integer-by-mask q2 new-qubits)))
    (unless (zerop (logand q1 q2))
      (error "The subsystems can't be joined because they share qubits."))
    (let* ((new-state-size (expt 2 (logcount new-qubits)))
           (new-state (make-lisp-cflonum-vector new-state-size)))
      (loop :with ss1-state := (subsystem-state ss1)
            :with ss2-state := (subsystem-state ss2)
            :for addr :below new-state-size
            :do (setf (aref new-state addr)
                      (* (aref ss1-state (%collapse-integer-by-mask addr collapsed-q1))
                         (aref ss2-state (%collapse-integer-by-mask addr collapsed-q2)))))
      (make-subsystem :qubits new-qubits
                      :state new-state))))

(defun eject-qubit-from-subsystem (ss qubit)
  "Eject the qubit QUBIT from the subsystem SS. Return two values:

    1. A subsystem with the qubit factored out. (If the qubit is not a part of the system, it is assumed to be in the zero state.)

    2. The state of the factored out qubit."
  (cond
    ;; The qubit isn't even a part of the subsystem you dummy!
    ((not (subsystem-contains-qubit-p ss qubit))
     (values ss (wf 1 0)))
    ;; We only have one qubit. Skip the rigamarole.
    ((= 1 (subsystem-num-qubits ss))
     (values (make-subsystem :qubits 0 :state (wf 1))
             (subsystem-state ss)))
    ;; The general case.
    (t
     ;; We are factoring ψ = ψ′ ⊗ ϕ, where ψ′ is the larger qubit
     ;; system returned by this function, and ϕ is the qubit state of
     ;; QUBIT.
     (let* ((ψ  (subsystem-state ss))
            (ψ′ (make-lisp-cflonum-vector (half (length ψ))))
            (ϕ  (make-lisp-cflonum-vector 2)))
       (declare (type quantum-state ψ ψ′ ϕ))
       ;; Calculate the ancilla state ϕ, where the full state ψ assumed to look
       ;; like
       ;;
       ;;    a0 b0 |00⟩ + a0 b1 |01⟩ + ... + a0 bn |0n⟩ + a1 b0 |10⟩ + ...
       ;;
       ;; This loop seeks the largest ai bj, hence largest ai and bj
       ;; individually, which tamps down on numerical error incurred
       ;; from dividing by near-zero.
       (loop :with abs := 0.0d0
             :for j :below (length ψ)
             :for ψⱼ := (aref ψ j)
             :for αⱼ := (abs ψⱼ)
             :when (< abs αⱼ)
               :do (let* ((i  (ldb (byte 1 qubit) j))
                          (Xi (- 1 i))
                          (Xj (logxor j (ash 1 qubit))))
                     (declare (type bit i Xi))
                     (setf abs         αⱼ
                           (aref ϕ i)  ψⱼ
                           (aref ϕ Xi) (aref ψ Xj))))

       ;; ϕ looks like a0 bj |0⟩ + a1 bj |1⟩ for some j, so we normalize out bj.
       (setf ϕ (normalize-wavefunction ϕ))
       ;; Calculate the factorized state. Still assuming ψ as above,
       ;; we pick the larger al of a0, a1 ...
       ;;
       ;; LARGER-INDEX should be clearly interpreted as
       ;; INDEX-OF-LARGER-AMPLITUDE, but that's too long to write.
       (let* ((larger-index (boolean-bit (<= (abs (aref ϕ 0))
                                             (abs (aref ϕ 1)))))
              (rescale (/ (aref ϕ larger-index)))   ; Guaranteed non-zero.
              (ratio (* rescale (aref ϕ (- 1 larger-index)))))
         (declare (type cflonum rescale ratio))
         (dotimes (j (length ψ))
           (cond
             ;; ... then examine al b0 |l0⟩ + ... al bn |ln⟩, and scale out al.
             ((= larger-index (ldb (byte 1 qubit) j))
              (setf (aref ψ′ (eject-bit j qubit)) (* rescale (aref ψ j))))
             ;; Also, we should be able to infer the value of
             ;;
             ;;     a(1-l) bj |(1-j)j⟩
             ;;
             ;; in ψ from a(1-l) / al * al bj. If this ever fails, the
             ;; ancilla is actually entangled.
             ((not (quil::double= (aref ψ j) (* ratio (aref ψ (logxor j (ash 1 qubit))))))
              (cerror "Continue anyway."
                      "Tried to disentagle qubit ~D that is actually entangled."
                      qubit)))))
       (values (make-subsystem :qubits (dpb 0 (byte 1 qubit) (subsystem-qubits ss))
                               :state ψ′)
               ϕ)))))
