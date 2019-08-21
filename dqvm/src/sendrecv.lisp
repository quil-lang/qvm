;;;; sendrecv.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(defmethod post-mpi-request ((qvm distributed-qvm) all-offsets action requests)
  "Post non-blocking MPI_Isend/MPI_Irecv requests for the amplitudes specified in ALL-OFFSETS, to be stored in the scratch array of QVM. The associated MPI_Requests are handled by REQUESTS."
  (check-type all-offsets array)
  (check-type action (member send recv))
  (check-type requests requests)

  (let ((number-of-processes (number-of-processes qvm))
        (ptr-data (static-vector-pointer (ecase action
                                           (send (amplitudes qvm))
                                           (recv (scratch qvm)))))
        (tag (qvm::pc qvm)))

    (loop :for target-rank :from 0 :below number-of-processes
          :for offsets := (aref all-offsets target-rank)
          :for count := (length offsets)
          :when (plusp count) :do

            (cffi:with-foreign-array (foreign-offsets offsets `(:array :int ,count))
              ;; XXX store the offsets in a foreign array to begin with.
              (with-mpi-type-indexed-block (type-indexed-block count 1 foreign-offsets +mpi-cflonum+)
                (funcall (ecase action
                           (send #'mpi::%mpi-isend)
                           (recv #'mpi::%mpi-irecv))
                         ptr-data 1 (cffi:mem-ref type-indexed-block 'mpi:mpi-datatype)
                         target-rank tag +mpi-comm-world+
                         (get-next-request requests)))))))

(defun post-mpi-irecv (qvm all-recv-offsets requests)
  "Post non-blocking MPI_Irecv requests for the amplitudes specified in ALL-RECV-OFFSETS, to be stored in the scratch array of QVM. The associated MPI_Requests are handled by REQUESTS."
  (post-mpi-request qvm all-recv-offsets 'recv requests))

(defun post-mpi-isend (qvm all-send-offsets requests)
  "Post non-blocking MPI_Isend requests for the amplitudes specified in ALL-SEND-OFFSETS, to be stored in the scratch array of QVM. The associated MPI_Requests are handled by REQUESTS."
  (post-mpi-request qvm all-send-offsets 'send requests))
