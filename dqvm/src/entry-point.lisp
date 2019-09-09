;;;; src/entry-point.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(defun %main (argv)
  (when (zerop +rank+)
    (format-log :debug "Command line arguments: \"~{~A~^ ~}\"" argv)

    (unless (rest argv)
      (format uiop/stream:*stderr* "Usage: ~A QUIL-FILE~%" (first argv))
      (uiop:quit -1)))

  (let (qvm)
    (labels ((make-parsed-program-from-code-vector (code-vector)
               "Create a parsed program from an array of gate applications."
               (make-instance 'quil:parsed-program :executable-code code-vector))

             (make-dqvm (number-of-qubits code-vector)
               "Instantiate a DISTRIBUTED-QVM object, load the program given by CODE-VECTOR, and set up its thread pools."
               (let ((qvm (make-distributed-qvm :number-of-qubits number-of-qubits
                                                :block-size (expt 2 (get-maximum-arity code-vector)))))
                 (qvm:load-program qvm (make-parsed-program-from-code-vector code-vector))
                 (qvm:prepare-for-parallelization)
                 qvm)))

      (declare (inline make-parsed-program-from-code-vector make-dqvm))

      (cond
        ((zerop +rank+)
         (let ((filename (second argv)))

           (format-log :info "Reading Quil file ~S" filename)

           (let* ((program (quil:read-quil-file filename))
                  (number-of-qubits (quil:qubits-needed program)))

             (format-log :info "Read program ~S using ~D total qubits. Code: ~{~A~^, ~}"
                         filename number-of-qubits
                         (map 'list #'instruction->string
                              (quil:parsed-program-executable-code program)))

             (setf qvm (make-dqvm (bcast :value number-of-qubits) (bcast-instructions program))))))

        (t
         (format-log :info "Waiting for instructions")
         (setf qvm (make-dqvm (bcast) (bcast-instructions))))))

    ;; (reset-wavefunction-debug qvm)

    (qvm:with-random-state ((qvm:seeded-random-state (get-random-seed)))
      (qvm:run qvm))

    (save-wavefunction qvm "wavefunction.dat")

    (format-log :info "Finished program execution.")))

(defun entry-point (argv)
  (let (;; (*print-pretty* t)
        ;; (*print-case* :downcase)
        ;; (*print-addresses* t)
        (qvm:*transition-verbose* t)) ; XXX turn this on/off via command line args.

    (uiop/stream:setup-stderr)

    ;; (regex-trace:regex-trace "^%MPI-.?(SEND|RECV|WAIT)" :print (mpi-comm-rank))

    (unless (mpi-initialized)
      (mpi-init :thread-support :mpi-thread-multiple))

    (mpi::%mpi-comm-set-errhandler +mpi-comm-world+ +mpi-errors-are-fatal+)

    (setf +rank+ (mpi-comm-rank))

    (setup-logger "Welcome to the Rigetti Distributed Quantum Virtual Machine")
    (unwind-protect
         (with-profiling-maybe ("DQVM2" "QVM" "QUIL" "MPI" "CFFI")
           (%main argv))
      (mpi-finalize)
      (uiop:quit))))
