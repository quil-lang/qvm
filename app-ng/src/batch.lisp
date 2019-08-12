(in-package #:qvm-app-ng)

(defun print-classical-memory (qvm)
  "Print all of the QVM's classical memory to *STANDARD-OUTPUT*."
  (let ((memories (qvm::classical-memories qvm)))
    (format t "Classical memory (low -> high indexes):")
    (cond
      ((zerop (hash-table-count memories))
       (format t "~&    No memory."))
      (t
       (maphash (lambda (name mv)
                  (format t "~&    ~A:" name)
                  (loop :for i :below (qvm::memory-view-length mv)
                        :do (format t " ~A" (qvm::memory-view-ref mv i))))
                memories)))
    (terpri)))

(defun start-batch-mode (&key
                           qubits
                           (simulation-method (error "No simulation method provided."))
                           (allocation-method (error "No allocation method provided.")))
  (let* ((program (safely-read-quil))
         (qubits-needed (cl-quil:qubits-needed program)))
    ;; Sanity checks.
    (when qubits
      (assert (>= qubits qubits-needed) ()
              "Computation restricted to ~D qubit~:P but ~D are needed."
              qubits qubits-needed))

    (let* ((qvm (with-timing (alloc-time)
                    (make-qvm qubits-needed :allocation allocation-method)
                  (format-log "Allocated memory of a ~D-qubit QVM in ~D ms."
                              qubits-needed alloc-time))))
      (load-program qvm program :supersede-memory-subsystem t)
      (qvm:with-random-state ((qvm:seeded-random-state nil))
        (with-timing (exec-time)
            (run qvm)
          (format-log "Executed program in ~D ms." exec-time)))
      (format-log "Classical memory and ~D-qubit state." qubits-needed)
      (print-classical-memory qvm)
      (format t "~&Amplitudes:")
      (let ((nq (qvm:number-of-qubits qvm)))
        (qvm:map-amplitudes
         qvm
         (let ((i 0))
           (lambda (z)
             (format t
                     "~%    |~v,'0B>: ~/QVM-APP-NG::PPRINT-COMPLEX/, ~64TP=~5F%"
                     nq
                     i
                     z
                     (* 100 (qvm:probability z)))
             (incf i)))))
      (terpri)))
  (quit-nicely))
