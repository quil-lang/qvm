;;;; worker.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Lauren Capelluto

(declaim (optimize (speed 0) safety debug (compilation-speed 0)))

(in-package #:dqvm.worker)

;;; This file deals with the business of the worker nodes.

(defstruct (worker (:conc-name nil)
                   (:predicate workerp))
  ;; The (MPI) rank of the worker.
  rank
  ;; The memory area in which work (arithmetic) occurs.
  work-area
  ;; The area from/to which data is copied.
  copy-area)

(defun swap-areas (n)
  "Swap the work and copy areas of the node N."
  (rotatef (work-area n) (copy-area n))
  nil)

(defun area-length (cluster)
  "The number of amplitudes in each area."
  (/ (expt 2 (qubit-count cluster))
     +worker-count+))

(defun operating-arity (cluster qubits)
  (declare (ignore cluster))
  (length qubits))

(defun block-length (cluster qubits)
  (expt 2 (operating-arity cluster qubits)))

(defun number-of-blocks (cluster qubits)
  (let ((total-qubits (qubit-count cluster))
        (operator-qubits (length qubits)))
    (expt 2 (- total-qubits operator-qubits))))

(defun blocks-per-worker (cluster qubits)
  (/ (number-of-blocks cluster qubits)
     +worker-count+))

(defun receive-cluster-information ()
  (deserialize-cluster (receive-string :source +master-rank+)))

(defun allocate-worker (cluster)
  "Allocate memory for the worker suitable for the cluster CLUSTER."
  (make-worker :rank +my-rank+
               :work-area (let ((v (qvm::make-lisp-cflonum-vector (area-length cluster))))
                            ;; Set to |000...00>.
                            (when (= 1 +my-rank+)
                              (setf (aref v 0) (qvm:cflonum 1)))
                            v)
               :copy-area (qvm::make-lisp-cflonum-vector (area-length cluster))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Reorg Amplitudes ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mysort (list f)
  (cond
    ((null list) nil)
    ((null (cdr list)) list)
    ((null (cdr (cdr list))) (if (funcall f (first list) (second list))
                                 list
                                 (list (second list) (first list))))
    (t (error "unsupported sort!"))))

(defun iterate-worker-addresses (cluster worker-rank qubits f &key (restart 0))
  ;; RESTART allows one to restart a computation in the middle of loop
  ;; structures
  (check-type cluster cluster)
  (check-type worker-rank unsigned-byte)
  (check-type qubits list)
  (check-type f function)
  (check-type restart unsigned-byte)
  (let ((sorted-qubits (mysort qubits '<)))
    (labels ((inject-it-all (number)
               (loop :for q :in sorted-qubits
                     :do (setf number (qvm::inject-bit number q)))
               number))
      (cond
        ((null qubits)
         ;; no sanity check needed (as we see in the ASSERT below)
         ;; because the block size is just 1
         (let ((start (+ restart (* (area-length cluster)
                                    (1- worker-rank)))))
           (loop :for actual :from restart
                 :for address :from start :below (+ start (area-length cluster) (- restart))
                 :do (funcall f actual address))))
        (t
         ;; sanity check so that we don't restart in the middle of a
         ;; block.
         (let ((actual restart))
           (multiple-value-bind (starting-block block-offset)
               (floor restart (block-length cluster qubits))
             (cond
               ((not (zerop block-offset))
                (block ESCAPE
                  (let ((elements-to-skip block-offset))
                    (iterate-worker-blocks
                     cluster worker-rank qubits
                     (lambda (blk)
                       (qvm::map-reordered-amplitudes
                        (inject-it-all blk)
                        (lambda (combo address)
                          (declare (ignore combo))
                          (cond
                            ((plusp elements-to-skip)
                             (decf elements-to-skip))
                            (t
                             (funcall f actual address)
                             (incf actual))))
                        (apply #'qvm::nat-tuple qubits))
                       (return-from ESCAPE))
                     :restart starting-block)))
                (iterate-worker-addresses cluster worker-rank qubits f :restart (+ restart block-offset)))
               (t
                (iterate-worker-blocks
                 cluster worker-rank qubits
                 (lambda (blk)
                   (qvm::map-reordered-amplitudes
                    (inject-it-all blk)
                    (lambda (combo address)
                      (declare (ignore combo))
                      (funcall f actual address)
                      (incf actual))
                    (apply #'qvm::nat-tuple qubits)))
                 :restart starting-block))))))))))

(defun iterate-worker-blocks (cluster worker-rank qubits f &key (restart 0))
  (let* ((nblocks (blocks-per-worker cluster qubits))
         (start (+ restart (* nblocks (1- worker-rank)))))
    (loop :for block :from start :below (+ start nblocks (- restart)) :do
      (funcall f block))))

(defun address->global-block (cluster address &key (qubits (operating-qubits cluster)))
  (check-type qubits list)
  (check-type cluster cluster)
  (check-type address unsigned-byte)
  (let ((qubits (mysort qubits '>)))
    (labels ((eject-all-bits (address)
               (let ((blk address))
                 (loop :for q :in qubits
                       :do (setf blk (qvm::eject-bit blk q)))
                 blk)))
      (eject-all-bits address))))

(defun address->worker-rank (cluster address &key (qubits (ordering cluster)))
  (check-type cluster cluster)
  (check-type address unsigned-byte)
  (check-type qubits list)
  (let ((blk (address->global-block cluster address :qubits qubits)))
    (1+ (floor blk (blocks-per-worker cluster qubits)))))

(defun mpi-send-complex-array (array dest &key (comm mpi:*standard-communicator*)
                                               (tag 0)
                                               (mode :basic))
  (declare (type (vector qvm:cflonum *) array)
           (type mpi::int dest tag)
           (type mpi:mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode))
  (let ((send-function
          (ecase mode
            (:basic #'mpi::%mpi-send)
            (:buffered #'mpi::%mpi-bsend)
            (:synchronous #'mpi::%mpi-ssend)
            (:ready #'mpi::%mpi-rsend))))
    ;; WARNING: We compute the length of the array here (as opposed to
    ;; computing (LENGTH VEC) below) because ARRAY may have a fill
    ;; pointer or the like. We *will* get a wrong answer if we try to
    ;; compute the length of the VEC below!
    (let ((count (* 2 ; two doubles per complex
                    8 ; 8 octets per double
                    (length array))))
      (with-raw-vector (vec ptr array)
        (funcall send-function ptr count mpi:+mpi-byte+ dest tag comm)))))

(defun send-complex-array (array dest)
  (mpi-send-complex-array array dest)
  (values))

(defun mpi-recv-complex-array (array source &key (comm mpi:*standard-communicator*)
                                                 (tag mpi:+mpi-any-tag+))
  (declare (type (simple-array qvm:cflonum (*)) array)
           (type mpi::int source tag)
           (type mpi:mpi-comm comm))
  (with-raw-vector (vec ptr array)
    (declare (ignore vec))
    (let ((type mpi:+mpi-byte+)
          (count (* 2 8 (length array))))
      ;; TODO check the mpi-status
      (mpi::%mpi-recv ptr count type source tag comm mpi:+mpi-status-ignore+))))

(defun receive-complex-array (&key (source mpi:+mpi-any-source+)
                                   (tag mpi:+mpi-any-tag+)
                                   (comm mpi:*standard-communicator*))
  "Receive a string of indeterminate size and return it, along with the rank of the node who sent it."
  (flet ((process-array (num-bytes sender-rank sent-tag)
           (declare (ignore sent-tag))
           (let ((array (make-array (/ num-bytes 8 2) :element-type 'qvm:cflonum
                                                      :initial-element (qvm:cflonum -8008))))
             (mpi-recv-complex-array array sender-rank :comm comm :tag tag)
             (values array sender-rank))
           #+ignore
           (static-vectors:with-static-vector (v (/ num-bytes 8 2) :element-type 'qvm:cflonum
                                                                   :initial-element (qvm:cflonum 0))
             (mpi-recv-complex-array v sender-rank :comm comm :tag tag)
             (values
              (copy-seq v)              ; RIP in piece 2018 memory
              sender-rank))))
    (call-with-probed-size #'process-array :source source :tag tag :comm comm)))

;;; XXX: This is really inefficient.
(defun send-message (target-worker payload)
  (etypecase payload
    (string
     (with-raw-vector (vec ptr payload)
       (declare (ignore ptr))
       (mpi:mpi-send vec target-worker)))
    (t
     (let ((string (with-total-readability (prin1-to-string payload))))
       (with-raw-vector (vec ptr string)
         (declare (ignore ptr))
         (mpi:mpi-send vec target-worker)))))
  (values))

(declaim (type (and fixnum unsigned-byte) *intermediate-send-list-length-limit*))
(defparameter *intermediate-send-list-length-limit* (floor (* 1/2 1024 1024))
  "The limit of the intermediate send lists before they get fired off.")

(defun intermediate-send-list-filledp (sl)
  (= *intermediate-send-list-length-limit* (length sl)))

(defun reset-intermediate-send-list (sl)
  (setf (fill-pointer sl) 0))

(defun compute-send-lists (cluster worker)
  ;; Allocate the size-limited send lists.
  (check-type cluster cluster)
  (check-type (operating-qubits cluster) list)
  (let ((send-lists (make-array (1+ +worker-count+))))
    (setf (aref send-lists 0) (rank worker))
    (loop :for i :from 1 :to +worker-count+ :do
      (setf (aref send-lists i)
            (make-array *intermediate-send-list-length-limit* :fill-pointer 0
                                                              :adjustable nil
                                                              :element-type 'qvm:cflonum)))
    (flet ((flush-send-list (target-worker-rank)
             ;; worker-rank is the send-list of the worker to fire off.
             #+ignore
             (if (= target-worker (rank worker))
                 (write-amplitudes-to-copy-area cluster worker target-worker (aref send-lists target-worker))
                 (send-message target-worker (aref send-lists target-worker)))
             (format-locked "Flushing ~D elements~%" (length (aref send-lists target-worker-rank)))
             (send-complex-array (aref send-lists target-worker-rank) target-worker-rank)
             (reset-intermediate-send-list (aref send-lists target-worker-rank))))
      (iterate-worker-addresses
       cluster
       (rank worker)
       (ordering cluster)
       (lambda (actual-index address)
         (let* ((target-worker (address->worker-rank cluster address :qubits (operating-qubits cluster)))
                (target-send-list (aref send-lists target-worker)))
           (vector-push (aref (work-area worker) actual-index) target-send-list)
           (when (intermediate-send-list-filledp target-send-list)
             (flush-send-list target-worker)))))
      ;; Flush the remaining sendlists.
      (loop :for i :from 1 :to +worker-count+
            :for send-list := (aref send-lists i)
            :when (plusp (length send-list))
              :do (flush-send-list i)))
    nil))

(defun send-amplitudes (cluster worker)
  (compute-send-lists cluster worker))

#+IGNORE
(defun compute-send-lists (cluster worker)
  (let ((send-lists (make-array (1+ +worker-count+) :initial-element '())))
    (setf (aref send-lists 0) (rank worker))
    (iterate-worker-addresses
     cluster
     (rank worker)
     (ordering cluster)
     (lambda (actual-index address)
       (let ((target-worker (address->worker-rank cluster address :qubits (operating-qubits cluster))))
         ;; We are pushing in reverse order.
         (push (aref (work-area worker) actual-index)
               (aref send-lists target-worker)))))
    (loop :for i :from 1 :below (length send-lists)
          :do (setf (aref send-lists i) (reverse (aref send-lists i))))
    ;; return it
    ;; (format-locked "send list(#~A): ~A~%" (rank worker) send-lists)
    send-lists))

#+IGNORE
(defun send-amplitudes (cluster worker)
  (let ((send-lists (compute-send-lists cluster worker)))
    (print-usage)
    (loop :for target-worker :from 1 :to +worker-count+ :do
      ;; write our own amplitudes write away
      (if (= target-worker (rank worker))
          (write-amplitudes-to-copy-area cluster worker target-worker (aref send-lists target-worker))
          (send-message target-worker (aref send-lists target-worker))))))

(defun write-amplitudes-to-copy-area (cluster worker sender-rank amplitudes &key (start 0))
  ;; amplitudes is a VECTOR now
  (let ((operating-qubits (operating-qubits cluster))
        (amplitudes-written 0))
    (check-type operating-qubits list)
    (iterate-worker-addresses
     cluster
     sender-rank
     (ordering cluster)
     (lambda (actual-index address)
       ;; Check if we've exhausted the AMPLITUDES
       (when (= amplitudes-written (length amplitudes))
         (return-from write-amplitudes-to-copy-area actual-index))
       (when (= (rank worker) (address->worker-rank cluster address :qubits operating-qubits))
         (let* ((global-blk (address->global-block cluster address :qubits operating-qubits))
                (local-blk (- global-blk (* (blocks-per-worker cluster operating-qubits)
                                            (1- (rank worker)))))
                (offset (loop :with offset := 0
                              :for qubit :in operating-qubits
                              :do (setf offset (logior (ash offset 1)
                                                       (ldb (byte 1 qubit) address)))
                              :finally (return offset)))
                (copy-area-index (+ offset (* local-blk
                                              (block-length cluster operating-qubits)))))
           (assert (not (minusp copy-area-index)))
           (assert (not (null amplitudes)))
           (setf (aref (copy-area worker) copy-area-index)
                 (aref amplitudes amplitudes-written))
           (incf amplitudes-written))))
     :restart start))
  nil)

(defun receive-amplitudes (cluster worker)
  ;; All except for self.
  (loop :with restarts := (make-array (1+ +worker-count+) :initial-element 0)
        :with remaining := (area-length cluster)
        :while (plusp remaining)
        :do (multiple-value-bind (amplitudes sender) (receive-complex-array)
              ;; (format-locked "parsing this from ~A: ~S~%" sender string-payload)
              (decf remaining (length amplitudes))
              (format-locked "Received ~D (~D remain) elements from #~D~%" (length amplitudes) remaining sender)
              (assert (not (null (aref restarts sender))))
              (setf (aref restarts sender)
                    (write-amplitudes-to-copy-area cluster worker sender amplitudes :start (aref restarts sender))))))

(defun reorganize-amplitudes (cluster worker)
  (flet ((start-send ()
           (with-errors-printed-verbosely
             (send-amplitudes cluster worker)
             (print-usage nil "send amplitudes")))
         (start-recv ()
           (with-errors-printed-verbosely
             (receive-amplitudes cluster worker)
             (print-usage nil "receive amplitudes"))))
    (let ((threads (list (bt:make-thread #'start-send :name "Sender")
                         (bt:make-thread #'start-recv :name "Receiver"))))
      (mapc #'bt:join-thread threads)
      (everybody-synchronize "Waiting for send/recv threads to finish")
      (swap-areas worker))))


(defun multiply-blocks-by-matrix (matrix cluster worker)
  (declare (optimize speed (safety 0) (debug 0)))
  (let* ((block-length (block-length cluster (operating-qubits cluster)))
         (work-area (work-area worker))
         (column (qvm::make-lisp-cflonum-vector block-length)))
    (declare (type (simple-array qvm:cflonum (*)) work-area)
             (type (and fixnum unsigned-byte) block-length))
    (loop :for blk :below (the fixnum (area-length cluster)) :by block-length :do
      (replace column work-area :start2 blk :end2 (+ blk block-length))
      (qvm::matrix-multiply matrix column)
      (setf (subseq work-area blk (+ blk block-length)) column))))

(defun instruction-matrix (InStRuCtIoN &optional env)
  (qvm::magicl-matrix-to-quantum-operator
   (quil::lookup-gate-in-environment instruction env)))

(defun execute-instruction (cluster worker instruction &optional env)
  ;; Fill copy area with sentinels for debugging.
  (fill (copy-area worker) (qvm:cflonum -31337))
  ;; Reorganize the amplitudes.
  (reorganize-amplitudes cluster worker)
  ;; Multiply the matrices.
  (multiply-blocks-by-matrix (instruction-matrix instruction env) cluster worker))

(defun instruction-string (instruction)
  (with-output-to-string (s)
    (quil:print-instruction instruction s)))

(defun dynamic-usage ()
  (round (/ (sb-kernel:dynamic-usage) 1024 1024)))

(defun print-usage (&optional (gc t) (prefix ""))
  (when gc
    (sb-ext:gc :full t))
  (format-locked "~A: Space usage: ~A MB~%" prefix (dynamic-usage))
  nil)

(defun %main-worker ()
  "Main entry point to the worker This is only to be called by DQVM:%MAIN."
  ;; Wait for master for sync.
  (assert (worker-node-p))
  (format-locked "Waiting for cluster information~%")
  (let ((cluster (receive-cluster-information)))
    (format-locked "Received cluster info: ~A~%" cluster)
    (let ((worker (allocate-worker cluster)))
      ;; wait for everybody to initialize
      (everybody-synchronize "Waiting for everybody to allocate")

      (format-locked "qubits: ~D~%" (qubit-count cluster))
      (format-locked "area length=~D~%" (area-length cluster))
      ;; wait for master on when to execute
      (format-locked "Beginning program execution~%")
      (loop :named INSTRUCTION-LOOP :do
        ;; Get the operating-qubits update
        (everybody-synchronize "Waiting for cluster information for operating qubits update [x]")
        (setf cluster (receive-cluster-information))
        (when (null (current-instruction cluster))
          (return-from INSTRUCTION-LOOP))

        (let ((instruction (string->instruction (current-instruction cluster))))
          (format-locked "Executing instruction: ~A~%" (instruction->string instruction))
          (print-usage nil "pre instruction")
          (execute-instruction cluster worker instruction nil))
        (print-usage nil "post instruction")

        (everybody-synchronize "Waiting for everybody to finish executing an instruction")
        ;; get the ordering update
        (setf cluster (receive-cluster-information)))
                                        ;(everybody-synchronize "Waiting for everybody to finish all execution")
      ;; We are all done. Reverse it all.
                                        ;(setf cluster (receive-cluster-information))
      (format-locked "Reorganizing amplitudes.~%")
      (reorganize-amplitudes cluster worker)
      (everybody-synchronize "Waiting for everybody to reorganize for final result")
      ;; Print everything out.
      (let ((*print-pretty* nil)
            (wa (work-area worker)))
        (print-usage nil "all done")
        (format-locked "Work area start/end/#nonzero: ~S/~S/~D~%"
                       (aref wa 0)
                       (aref wa (1- (length wa)))
                       (count-if-not #'zerop wa)))
      nil)))
