;;;; src/apply-distributed-gate.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(defparameter *default-blocks-per-chunk* (expt 2 16)
  "Number of blocks for the ranks to collectively work on during a single step within APPLY-DISTRIBUTED-GATE.")

(defun apply-distributed-gate (qvm instr &key (blocks-per-chunk *default-blocks-per-chunk*))
  (let* ((addresses (addresses qvm))
         (amplitudes (amplitudes qvm))
         (scratch (scratch qvm))

         (global-addresses (global-addresses addresses))
         (number-of-processes (number-of-processes global-addresses))

         (next-permutation (qubit-permutation instr))
         (addresses (addresses qvm))
         (block-size (block-size addresses))
         (blocks-per-process (blocks-per-process addresses))
         (requests (make-instance 'requests :total (* 2 number-of-processes))))

    (loop :for block-index :from 0 :below (1+ blocks-per-process) :by blocks-per-chunk
          :for start-offset := (* block-index block-size)
          :for end-offset := (* (+ block-index blocks-per-chunk) block-size) :do

            (non-blocking-receive qvm next-permutation start-offset end-offset requests)
            (non-blocking-send qvm next-permutation start-offset end-offset requests)

            (wait-all requests))

    (compute-matrix-vector-products (quil:gate-matrix instr) scratch amplitudes 0 (length scratch))

    (update-permutation next-permutation addresses)

    qvm))

(defmethod make-offset-arrays (global-addresses)
  "Create N empty arrays to store offsets."
  (let* ((number-of-processes (number-of-processes global-addresses))
         (blocks-per-process (blocks-per-process global-addresses))
         (remainder-blocks (remainder-blocks global-addresses))
         (block-size (block-size global-addresses))

         (dimension (* (+ blocks-per-process
                          (boolean-bit remainder-blocks))
                       block-size)))

    (flet ((make-empty-array ()
             (make-array dimension :element-type '(unsigned-byte 32) :fill-pointer 0)))

      (make-array number-of-processes
                  :initial-contents (loop :repeat number-of-processes :collect (make-empty-array))))))

(defmethod make-offset-arrays ((addresses addresses))
  (make-offset-arrays (global-addresses addresses)))

(defun non-blocking-receive (qvm next-permutation start-offset end-offset requests)
  "Iterate over the addresses that should be in the current chunk after applying the next instruction. Start requests to receive the required amplitudes from the ranks where they are currently stored."
  (let* ((addresses (addresses qvm))
         (global-addresses (global-addresses addresses))
         (all-recv-offsets (make-offset-arrays addresses)))

    (when (< start-offset (number-of-addresses addresses))

      (loop :with next-addresses := (make-addresses-like addresses :rank (rank addresses) :permutation next-permutation)
            :for offset :from start-offset :below end-offset :do

              (let ((next-address (get-address-by-offset next-addresses offset)))
                (unless next-address
                  (return))

                (let* ((source-rank (get-rank-by-address global-addresses next-address))
                       (recv-offsets (aref all-recv-offsets source-rank)))
                  (vector-push offset recv-offsets))))

      (post-mpi-irecv qvm all-recv-offsets requests))))

(defun non-blocking-send (qvm next-permutation start-offset end-offset requests)
  "Iterate over all ranks and find which addresses within the current rank are needed by another rank. Aggregate that information, then start sending amplitudes."
  (loop :with addresses := (addresses qvm)
        :with all-send-offsets := (make-offset-arrays addresses)
        :with number-of-processes := (number-of-processes addresses)

        :for count :from 0 :below number-of-processes
        :for target-rank := (mod (+ count (rank qvm)) number-of-processes)
        :for send-offsets := (aref all-send-offsets target-rank)
        :for target-addresses := (make-addresses-like addresses :rank target-rank :permutation next-permutation) :do

          (loop :for target-offset :from start-offset :below end-offset :do

            (let ((target-address (get-address-by-offset target-addresses target-offset)))
              (unless target-address
                (return))

              (alexandria:when-let ((source-offset (offset addresses target-address)))
                (vector-push source-offset send-offsets))))

        :finally (post-mpi-isend qvm all-send-offsets requests)))
