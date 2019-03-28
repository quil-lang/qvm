;;;; api/expectation.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defgeneric perform-expectation (simulation-method state-prep operators num-qubits &key gate-noise measurement-noise)
  (:method (simulation-method state-prep operators num-qubits &key gate-noise measurement-noise)
    (declare (ignore gate-noise measurement-noise))
    (api-method-not-implemented-error 'perform-expectation)))


(defmethod perform-expectation ((simulation-method (eql 'full-density-matrix)) state-prep operators num-qubits &key gate-noise measurement-noise)
  "Let ρ be the density matrix resulting from STATE-PREP on the zero state. Then compute a list of real expectation values of the operators in OPERATORS, namely,

    tr(ρ O1), tr(ρ O2), ...    for Oi in OPERATORS.
"
  (%perform-expectation simulation-method #'mixed-state-expectation state-prep operators num-qubits gate-noise measurement-noise))

(defmethod perform-expectation ((simulation-method (eql 'pure-state)) state-prep operators num-qubits &key gate-noise measurement-noise)
  "Let F be the wavefunction resulting from STATE-PREP on the zero state. Then compute a list of real expectation values of the operators in OPERATORS, namely,

    <F| O1 |F>,  <F| O2 |F>,  ...    for Oi in OPERATORS.
"
  (%perform-expectation simulation-method #'pure-state-expectation state-prep operators num-qubits gate-noise measurement-noise))

(defun %perform-expectation (simulation-method expectation-op state-prep operators num-qubits gate-noise measurement-noise)
  (check-type state-prep quil:parsed-program)
  (dolist (o operators) (check-type o quil:parsed-program))
  (check-type num-qubits (integer 0))
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  ;; If we have nothing to compute the expectation of, then return
  ;; nothing.
  (when (null operators)
    (format-log "No operators to compute expectation of. Returning NIL.")
    (return-from %perform-expectation '()))

  ;; Otherwise, go about business.
  (let ((qvm (make-appropriate-qvm simulation-method state-prep num-qubits gate-noise measurement-noise))
        timing)
    ;; Make the initial state.
    (qvm:load-program qvm state-prep)
    (format-log "Computing initial state for expectation value ~
                 computation on ~A"
                (class-name (class-of qvm)))
    (with-timing (timing)
      (with-timeout
          (qvm:run qvm)))
    (format-log "Finished state prep in ~D ms." timing)
    (format-log "Copying prepared state.")
    (let ((prepared-wf
            (with-timing (timing)
              (qvm:copy-wavefunction (qvm::amplitudes qvm))))
          (first-time t))
      (format-log "Copied prepared state in ~D ms." timing)
      ;; Compute the expectations of the operators.
      (loop :for i :from 1
            :for op :in operators
            :collect (let (expectation)
                       (format-log "Computing the expectation value of the ~:R operator." i)
                       (with-timing (timing)
                         (setf expectation (funcall expectation-op qvm prepared-wf op first-time))
                         (setf first-time nil))
                       (format-log "Computed ~:R expectation value in ~D ms." i timing)
                       (assert (< (abs (imagpart expectation)) 1e-14))
                       (unless (zerop (imagpart expectation))
                         (warn "Non-zero but acceptable imaginary part of expectation value: ~A" expectation))
                       (realpart expectation))))))

(defun pure-state-expectation (qvm prepared-state op &optional first-time)
  (flet ((inner-product (a b)
           (declare (type qvm::quantum-state a b))
           (loop :for ai :of-type qvm::cflonum :across a
                 :for bi :of-type qvm::cflonum :across b
                 :sum (* (conjugate ai) bi))))
    (unless first-time
      (qvm:copy-wavefunction prepared-state (qvm::amplitudes qvm)))
    (qvm:load-program qvm op)
    (qvm:run qvm)
    (inner-product prepared-state
                   (qvm::amplitudes qvm))))

(defun mixed-state-expectation (qvm prepared-state op &optional first-time)
  "Computes tr(Q ρ) where Q is the Hermitian matrix corresponding to
the quil program OP, and ρ is the density matrix represented as the
amplitudes in PREPARED-STATE."
  (declare (ignore first-time))
  (destructuring-bind (rows cols) (array-dimensions (qvm::density-matrix-view qvm))
    ;; MAGICL:MAKE-MATRIX constructs a column-major matrix, whereas
    ;; PREPARED-STATE is the row-major vectorization of ρ. We prefer
    ;; to apply the transpose to OP-MATRIX below, since it is
    ;; generally smaller. Thus we compute tr(Q^T ρ^T) = tr((ρ Q)^T) = tr(ρ Q) = tr(Q ρ).
    (let ((op-matrix (magicl:transpose
                      (quil::parsed-program-to-logical-matrix op)))
          (density-matrix (magicl:make-matrix :rows rows :cols cols 
                                              :data prepared-state)))
      (reduce #'+ (magicl:matrix-diagonal
                   (quil::matrix-rescale-and-multiply op-matrix density-matrix))))))
