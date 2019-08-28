(in-package #:qvm)

(defclass toy-qvm (pure-state-qvm)
  ((readout-povms
    :initarg :readout-povms
    :accessor readout-povms
    :initform (make-hash-table :test 'eql)
    :documentation "Noisy readout encoded as diagonal single qubit
    POVM given as a 4-element list (p(0|0) p(0|1) p(1|0) p(1|1))")))


;; TODO declare inlinabldee, turn on optimization
(defun perturb-measurement (actual-outcome p00 p01 p10 p11)
  "Given the readout error encoded in the POVM (see documentation of NOISY-QVM)
randomly sample the observed (potentially corrupted) measurement outcome."
 ; (format t "actual: ~a p0|0: ~a p1|0: ~a" actual-outcome p00 p10)
  (check-type actual-outcome bit)
  (check-probabilities p00 p01 p10 p11)
  (let ((r (random 1.0d0)))
    (ecase actual-outcome
      ((0) (if (<= r p00) 0 1))
      ((1) (if (<= r p01) 0 1)))))


(defgeneric set-readout-povm (qvm qubit povm)
  (:documentation "For a QUBIT belonging to a QVM specify a POVM to encode
possible readout errors. POVM must be a 4-element list of double-floats."))


(defun check-probabilities (p00 p01 p10 p11)
  "Make sure that the probabilities passed into the POVM are all between 0.0 and 1.0"
  (check-type p00 (double-float 0.0d0 1.0d0))
  (check-type p01 (double-float 0.0d0 1.0d0))
  (check-type p10 (double-float 0.0d0 1.0d0))
  (check-type p11 (double-float 0.0d0 1.0d0)))


(defun print-assignment-probs (qvm q)
  (let* ((povm-map (readout-povms qvm))
         (povm (gethash q povm-map)))
    (destructuring-bind (p00 p01 p10 p11) povm
      (format t "probabilities: p0|0 : ~a,  p0|1 : ~a,  p1|0 : ~a, p1|1 : ~a" p00 p01 p10 p11))))


(defun measure-bit-flip-noise (qvm q numshots)
  (let ((ones 0)
        (program "DECLARE R0 BIT; X 0; H 0; MEASURE 0 R0"))
    (loop :repeat numshots
       :do (incf ones (do-noisy-measurement qvm q program)))
    (format t "result: ~a" (float (/ ones numshots)))))


(defun estimate-assignment-probabilities (qvm q numshots)
  (let ((results-i 0)
        (results-x 0)
        (program1 "DECLARE R0 BIT; I 0; MEASURE 0 R0")
        (program2 "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
    (loop :repeat numshots
      :do (incf results-i (do-noisy-measurement qvm q program1))
      :do (incf results-x (do-noisy-measurement qvm q program2))
          )
    (let ((p00 (- 1 (float (/ results-i numshots))))
          (p11 (float (/ results-x numshots))) )
      (format t "RESULTS:~% p00: ~a~% p01: ~a~% p10: ~a~% p11: ~a~% " p00 (- 1 p11) (- 1 p00) p11))))


(defun do-noisy-measurement (qvm q program)
  "Takes a qvm, qubit and program string, runs the program and returns the measured result of the qubit q"
  (let ((parsed-program (quil:parse-quil program)))
    (load-program qvm parsed-program :supersede-memory-subsystem t))
  (run qvm)
  (let ((measured-result (dereference-mref qvm (quil:mref "R0" q))))
    measured-result))


(defun check-povm (povm)
  "Verify that the list POVM contains a valid single qubit diagonal POVM.
Also see the documentation for the READOUT-POVMS slot of NOISY-QVM."
  (destructuring-bind (p00 p01 p10 p11) povm
    (check-probabilities p00 p01 p10 p11)
    (assert (cl-quil::double= 1.0d0 (+ p00 p10)))
    (assert (cl-quil::double= 1.0d0 (+ p01 p11)))))


(defmethod set-readout-povm ((qvm toy-qvm) qubit povm)
  (format t "here")
  (check-povm povm)
  (setf (gethash qubit (readout-povms qvm)) povm)
  (format t "~a" (gethash qubit (readout-povms qvm)))
  nil)


(defun %corrupt-qvm-memory-with-povm (qvm instr)
  (check-type instr quil:measure)
  (let* ((q (quil:qubit-index (quil:measurement-qubit instr)))
           (a (quil:measure-address instr))
           (c (dereference-mref qvm a))
           (povm (gethash q (readout-povms qvm))))
      (when povm
        (destructuring-bind (p00 p01 p10 p11) povm
          (setf (dereference-mref qvm a)
                (perturb-measurement c p00 p01 p10 p11))))))


(defgeneric apply-classical-readout-noise (qvm instr)
  (:documentation "Given a QVM and a (measurement) instruction INSTR, corrupt the readout bit according to the POVM specifications of QVM.")
  ;; Pure state QVM has no readout noise.
  (:method ((qvm pure-state-qvm) (instr quil:measurement))
    (declare (ignore qvm instr))
    nil)
  ;; Readout noise only happens to the resulting classical bit (i.e.,
  ;; it's classical noise). As such, discarding that bit doesn't
  ;; warrant any sort of special treatment.
  (:method ((qvm toy-qvm) (instr quil:measure-discard))
    (declare (ignore qvm instr))
    nil)
  ;; We do have a readout bit, and we want to corrupt it.
  (:method ((qvm toy-qvm) (instr quil:measure))
    (%corrupt-qvm-memory-with-povm qvm instr))
  ;; For compiled measurements, refer to the source of that
  ;; instruction.
  (:method ((qvm toy-qvm) (instr compiled-measurement))
    (apply-classical-readout-noise qvm (source-instruction instr))))

(defmethod transition :around ((qvm toy-qvm) (instr quil:measurement))
  ;; perform actual measurement
  (let ((ret-qvm (call-next-method)))
    (apply-classical-readout-noise ret-qvm instr)
    ret-qvm))


(defun perturb-measured-bits (qvm measured-bits)
  "Randomly perturb the values of the bits in MEASURED-BITS in
accordance with any available readout POVMs on the QVM. Returns an
updated list of measured bits."
  ;; This models purely classical bit flips of the measurement record
  ;; which captures the reality of noisy low power dispersive
  ;; measurements of superconducting qubits very well. Here the
  ;; dominant source of error is misclassifying a readout signal due
  ;; to thermal noise that corrupts the signal on its return path out
  ;; of the cryostat.
  (loop :for i :below (number-of-qubits qvm)
        :for c :in measured-bits
        :collect (let ((povm (gethash i (readout-povms qvm))))
                   (if povm
                       (destructuring-bind (p00 p01 p10 p11) povm
                         (perturb-measurement c p00 p01 p10 p11))
                       c))))


(defmethod measure-all ((qvm toy-qvm))
  (declare (ignore qvm))
  (multiple-value-bind (qvm-ret measured-bits)
      (call-next-method)
    (values
     qvm-ret
     (perturb-measured-bits qvm-ret measured-bits))))

