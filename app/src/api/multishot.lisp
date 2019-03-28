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

  ;; Bail out early if there's no work to actually do.
  (when (or (zerop (hash-table-count addresses))
            (zerop num-trials)
            (loop :for v :being :the :hash-values :of addresses
                  :always (null v)))
    (return-from %perform-multishot (load-time-value (make-hash-table) t)))

  (let ((qvm (make-appropriate-qvm simulation-method quil num-qubits gate-noise measurement-noise))
        (trial-results (make-hash-table :test 'equal
                                        :size (hash-table-count addresses)))
        timing)
    (qvm:load-program qvm quil :supersede-memory-subsystem t)
    (format-log ':debug "Running experiment with ~D trial~:P on ~A"
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

    (format-log ':debug "Finished in ~D ms" timing)
    ;; We collected everything in reverse. So, reverse that.
    (maphash (lambda (k v)
               (setf (gethash k trial-results) (nreverse v)))
             trial-results)
    trial-results))
