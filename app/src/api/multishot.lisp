;;;; api/multishot.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defgeneric perform-multishot (simulation-method quil num-qubits addresses num-trials &key gate-noise measurement-noise)
  (:method (simulation-method quil num-qubits addresses num-trials &key gate-noise measurement-noise)
    (declare (ignore gate-noise measurement-noise))
    (api-method-not-implemented-error 'perform-multishot)))

(defmethod perform-multishot ((simulation-method (eql 'pure-state)) quil num-qubits addresses num-trials &key gate-noise measurement-noise)
  (%perform-multishot simulation-method quil num-qubits addresses num-trials gate-noise measurement-noise))

(defmethod perform-multishot ((simulation-method (eql 'full-density-matrix)) quil num-qubits addresses num-trials &key gate-noise measurement-noise)
  (%perform-multishot simulation-method quil num-qubits addresses num-trials gate-noise measurement-noise))

(defun valid-address-query-p (addresses)
  (cond
    ((not (hash-table-p addresses)) nil)
    (t
     (maphash (lambda (k v)
                (unless (and (stringp k)
                             (or (eq t v)
                                 (and (alexandria:proper-list-p v)
                                      (every #'integerp v)
                                      (notany #'minusp v))))
                  (return-from valid-address-query-p nil)))
              addresses)
     t)))

(defun collect-result-data (qvm addresses results)
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
                (error "Invalid multishot address query for memory named ~S." name))))
           addresses)
  results)

(defun merge-results (result-tables)
  "Merge the result tables RESULT-TABLES into a single table."
  (let ((merged-results (make-hash-table :test 'equal)))
    (dolist (table result-tables merged-results)
      (maphash (lambda (key value)
                 (setf (gethash key merged-results)
                       (nreconc value (gethash key merged-results nil))))
               table))))

(defvar *trials-required-for-parallelization* 1000
  "The number of trials required for parallelization to occur.")

(defun %perform-multishot (simulation-method quil num-qubits addresses num-trials gate-noise measurement-noise)
  (check-type simulation-method simulation-method)
  (check-type quil quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type num-trials (integer 0))
  (check-type addresses hash-table)
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (valid-address-query-p addresses) ()
          "Detected invalid address query in multishot experiment. The ~
           requested addresses should be a JSON object whose keys are ~
           DECLAREd memory names, and whose values are either the true ~
           value to request all memory, or a list of non-negative integer ~
           indexes to request some memory.")
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  (cond
    ;; If we have too many qubits or too few trials, just do things
    ;; single-threaded.
    ((or (not *single-user-mode*)
         (>= num-qubits qvm::*qubits-required-for-parallelization*)
         (< num-trials *trials-required-for-parallelization*)
         (= 1 (lparallel:kernel-worker-count)))
     (%perform-multishot/single-threaded simulation-method quil num-qubits addresses num-trials gate-noise measurement-noise))
    ;; Parallelize the loop across all cores.
    (t
     (let* ((num-tasks (lparallel:kernel-worker-count))
            (task-counts (loop :for (start . end) :in (qvm::subdivide num-trials num-tasks)
                               :collect (- end start)))
            (ch (lparallel:make-channel))
            timing
            results)
       (format-log "Running multishot experiment with ~D worker~:P" num-tasks)
       (with-timing (timing)
         ;; Submit work to everybody.
         (dolist (task-count task-counts)
           (lparallel:submit-task ch (let ((task-count task-count))
                                       (lambda ()
                                         (%perform-multishot/single-threaded
                                          simulation-method
                                          quil
                                          num-qubits
                                          addresses
                                          task-count
                                          gate-noise
                                          measurement-noise)))))
         ;; Receive and coalesce results.
         (setf results (merge-results (loop :repeat num-tasks :collect (lparallel:receive-result ch)))))
       (format-log "Finished all multishot tasks in ~D ms" timing)
       results))))

(defun %perform-multishot/single-threaded (simulation-method quil num-qubits addresses num-trials gate-noise measurement-noise)
  ;; Bail out early if there's no work to actually do.
  (when (or (zerop (hash-table-count addresses))
            (zerop num-trials)
            (loop :for v :being :the :hash-values :of addresses
                  :always (null v)))
    (return-from %perform-multishot/single-threaded
      (load-time-value (make-hash-table) t)))

  (let ((qvm (make-appropriate-qvm simulation-method quil num-qubits gate-noise measurement-noise))
        (trial-results (make-hash-table :test 'equal
                                        :size (hash-table-count addresses)))
        timing)
    (qvm:load-program qvm quil :supersede-memory-subsystem t)
    (format-log "Running experiment with ~D trial~:P on ~A"
                num-trials
                (class-name (class-of qvm)))
    (with-timing (timing)
      (dotimes (trial num-trials)
        ;; Reset the program counter.
        (setf (qvm::pc qvm) 0)

        ;; Reset the amplitudes, but only if running more than one trial.
        (unless (= 1 num-trials)
          ;; Reset the amplitudes.
          (qvm::reset-quantum-state qvm))

        ;; Run the program.
        (with-timeout (qvm:run qvm))

        ;; Collect all of the memory that the user requests.
        (collect-result-data qvm addresses trial-results)))

    (format-log "Finished in ~D ms" timing)
    ;; We collected everything in reverse. So, reverse that.
    (maphash (lambda (k v)
               (setf (gethash k trial-results) (nreverse v)))
             trial-results)
    trial-results))
