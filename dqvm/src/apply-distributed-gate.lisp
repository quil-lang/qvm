;;;; src/apply-distributed-gate.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(defvar *default-blocks-per-chunk* (expt 2 16)
  "Number of blocks for the ranks to collectively work on during a single step within APPLY-DISTRIBUTED-GATE.")

(defun apply-distributed-gate (qvm instr &key (blocks-per-chunk *default-blocks-per-chunk*))
  (let* ((addresses (addresses qvm))
         (amplitudes (amplitudes qvm))
         (scratch (scratch qvm))

         (global-addresses (global-addresses addresses))
         (number-of-processes (number-of-processes global-addresses))

         (next-permutation (qubit-permutation instr))
         (block-size (block-size addresses))
         (blocks-per-process (blocks-per-process addresses))

         ;; XXX Allocate once and reuse across subsequent calls to
         ;; apply-distributed-gate.
         (requests (make-instance 'requests :total (* 2 number-of-processes)))
         (all-recv-offsets (make-offset-arrays global-addresses))
         (all-send-offsets (make-offset-arrays global-addresses)))

    (loop :for block-index :from 0 :below (1+ blocks-per-process) :by blocks-per-chunk
          :for start-offset := (* block-index block-size)
          :for end-offset := (* (+ block-index blocks-per-chunk) block-size) :do

            (reset-offset-arrays all-recv-offsets all-send-offsets)

            (non-blocking-receive qvm next-permutation start-offset end-offset all-recv-offsets requests)
            (non-blocking-send qvm next-permutation start-offset end-offset all-send-offsets requests)

            (wait-all requests))

    (compute-matrix-vector-products (quil:gate-matrix instr) scratch amplitudes 0 (length scratch))

    (update-permutation next-permutation addresses)

    qvm))

(defun non-blocking-receive (qvm next-permutation start-offset end-offset all-recv-offsets requests)
  "Iterate over the addresses that should be in the current chunk after applying the next instruction. Start requests to receive the required amplitudes from the ranks where they are currently stored.

The arguments START-OFFSET and END-OFFSET specify the offsets within the local portion of the wavefunction that will be inspected during this call.
The parameter ALL-RECV-OFFSETS is the instance of OFFSET-ARRAYS which will hold the relevant offsets where data should be received.
Finally, REQUESTS is an instance of the REQUESTS class that keeps track of MPI_Request objects."
  (loop :with addresses := (addresses qvm)
        :with global-addresses := (global-addresses addresses)
        :with next-addresses := (make-addresses-like addresses :permutation next-permutation)
        :with effective-end-offset := (min end-offset (* (block-size addresses) (number-of-blocks addresses)))

        :for offset :from start-offset :below effective-end-offset
        :for next-address := (get-address-by-offset next-addresses offset)
        :for source-rank := (get-rank-by-address global-addresses next-address) :do
          (offset-arrays-push offset source-rank all-recv-offsets)

        :finally (post-mpi-irecv qvm all-recv-offsets requests)))

(defun non-blocking-send (qvm next-permutation start-offset end-offset all-send-offsets requests)
  "Iterate over all ranks and find which addresses within the current rank are needed by another rank. Aggregate that information, then start sending amplitudes.

The arguments START-OFFSET and END-OFFSET specify the offsets within the local portion of the wavefunction that will be inspected during this call.
The parameter ALL-SEND-OFFSETS is the instance of OFFSET-ARRAYS which will hold the relevant offsets of data that should be sent elsewhere.
Finally, REQUESTS is an instance of the REQUESTS class that keeps track of MPI_Request objects."
  (loop :with addresses := (addresses qvm)
        :with number-of-processes := (number-of-processes addresses)

        :for count :from 0 :below number-of-processes
        :for target-rank := (mod (+ count (rank qvm)) number-of-processes)
        :for target-addresses := (make-addresses-like addresses :rank target-rank :permutation next-permutation) :do

          (loop :for target-offset :from start-offset :below end-offset
                :for target-address := (get-address-by-offset target-addresses target-offset)
                :while target-address :do

                  (alexandria:when-let ((source-offset (offset addresses target-address)))
                    (offset-arrays-push source-offset target-rank all-send-offsets))

                :finally (post-mpi-isend qvm all-send-offsets requests
                                         :start target-rank
                                         :end (1+ target-rank)))))
