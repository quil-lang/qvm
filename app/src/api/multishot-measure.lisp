;;;; api/multishot-measure.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun parallel-measure (qvm &optional qubits)
  (cond
    ;; Fast path: measure all of the qubits. Note that we check for
    ;; "all" by checking that we have n distinct qubits.
    ((= (qvm:number-of-qubits qvm)
        (count-if-not (lambda (q) (eql q ':unused-qubit))
                      (remove-duplicates qubits)))
     (let ((bits (nth-value 1 (qvm:measure-all qvm))))
       (loop :for q :in qubits
             :if (eql q ':unused-qubit)
               :collect 0
             :else
               :collect (nth q bits))))
    ;; Slow path. Measure only some of the qubits.
    ;;
    ;; XXX: Debate whether we actually shouldn't just MEASURE-ALL
    ;; and take only some of the qubits. This would have
    ;; repercussions on a persistent QVM.
    (t
     (loop :for q :in qubits
           :if (eql q ':unused-qubit)
             :collect 0
           :else
             :collect (nth-value 1 (qvm:measure qvm q nil))))))

(defgeneric perform-multishot-measure (simulation-method quil num-qubits qubits num-trials relabeling)
  (:method (simulation-method quil num-qubits qubits num-trials relabeling)
    (api-method-not-implemented-error 'perform-multishot-measure)))


(defmethod perform-multishot-measure ((simulation-method (eql 'pure-state)) quil num-qubits qubits num-trials relabeling)
  (%perform-multishot-measure simulation-method quil num-qubits qubits num-trials relabeling))

(defmethod perform-multishot-measure ((simulation-method (eql 'full-density-matrix)) quil num-qubits qubits num-trials relabeling)
  (%perform-multishot-measure simulation-method quil num-qubits qubits num-trials relabeling))

(defun %perform-multishot-measure (simulation-method quil num-qubits qubits num-trials relabeling)
  (check-type quil quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type num-trials (integer 0))
  (check-type qubits alexandria:proper-list)
  (check-type relabeling (or null (vector unsigned-byte)))
  (assert (every (alexandria:conjoin #'integerp (complement #'minusp)) qubits))

  (when (or (null qubits) (zerop num-trials))
    (return-from %perform-multishot-measure nil))

  ;; Relabel the qubits according to RELABELING. This is O(N^2), but N
  ;; will always be less than 40 or so.
  (when relabeling
    (setf qubits
          (loop :for qubit :in qubits
                :collect (or (position qubit relabeling) ':unused-qubit)))
    (setf num-qubits
          (max num-qubits
               (1+ (reduce #'max
                           (remove ':unused-qubit qubits)

                           ;; Specify initial value in case all qubits
                           ;; are unused. An initial value -1 is
                           ;; incremented to 0 by outer (1+ ...) call
                           :initial-value -1)))))

  (let ((qvm (make-appropriate-qvm simulation-method quil num-qubits nil nil))
        timing)
    ;; Make the initial state.
    (qvm:load-program qvm quil)
    (format-log "Computing ~D-qubit state for multishot/measure on ~A."
                num-qubits
                (class-name (class-of qvm)))
    (with-timing (timing)
      (with-timeout
          (qvm:run qvm)))
    (format-log "Finished state computation in ~D ms." timing)
    (format-log "Copying state.")
    (let ((prepared-wf
            (with-timing (timing)
              (qvm:copy-wavefunction (qvm::amplitudes qvm))))
          (first-time t))
      (format-log "Copied prepared state in ~D ms." timing)
      (flet ((reload (qvm)
               (unless first-time
                 (qvm:copy-wavefunction prepared-wf (qvm::amplitudes qvm)))
               (setf first-time nil)))
        ;; Do the parallel measurements
        (format-log "Doing ~D ~D-qubit measurements." num-trials (length qubits))
        (prog1
            (with-timing (timing)
              (loop :repeat num-trials
                    :collect (progn
                               (reload qvm)
                               (parallel-measure qvm qubits))))
          (format-log "Done measuring in ~D ms." timing))))))
