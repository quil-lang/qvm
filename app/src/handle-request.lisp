;;;; handle-request.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun process-quil (quil)
  "Prepare the PARSED-PROGRAM QUIL for more efficient execution. Currently this only includes remapping the qubits to a minimal sequential set from 0 to (num-qubits-used - 1). Return two values: the processed Quil code and the mapping vector.

The mapping vector V specifies that the qubit as specified in the program V[i] has been mapped to qubit i."
  (let* ((mapping (quil::compute-qubit-mapping quil))
         (trivial-mapping-p
           (loop :for x :across mapping
                 :for i :from 0
                 :always (= x i))))
    (unless trivial-mapping-p
      (format-log "Mapping qubits: ~{~A~^, ~}"
                  (loop :for x :across mapping
                        :for i :from 0
                        :when (/= i x)
                          :collect (format nil "~D -> ~D" x i)))
      (quil::transform 'quil::compress-qubits quil))
    (values quil mapping)))

(defun get-random-state (arg)
  (etypecase arg
    (null (qvm:seeded-random-state nil))
    (unsigned-byte (qvm:seeded-random-state arg))))

(defun check-required-fields (hash-table &rest fields)
  (dolist (field fields t)
    (when (null (nth-value 1 (gethash field hash-table)))
      (error "Expected the field ~A to exist." field))))

(defun get-quil-instrs-field (hash-table)
  (or (gethash "quil-instructions" hash-table)
      (gethash "compiled-quil" hash-table)
      (gethash "uncompiled-quil" hash-table)))

(defun check-for-quil-instrs-field (hash-table)
  (unless (get-quil-instrs-field hash-table)
    (error "Expected a QUIL-INSTRUCTIONS type field to exist.")))

(defun handle-post-request (request)
  (when (null tbnl:*session*)
    (tbnl:start-session))

  (let* ((api-key (tbnl:header-in* ':X-API-KEY request))
         (user-id (tbnl:header-in* ':X-USER-ID request))
         (data (hunchentoot:raw-post-data :request request
                                          :force-text t))
         (js (let* ((js (ignore-errors (yason:parse data))))
               (unless (and (hash-table-p js)
                            (check-required-fields js "type"))
                 (error "Invalid request."))
               js))
         (type (gethash "type" js))
         (gate-noise (gethash "gate-noise" js))
         (measurement-noise (gethash "measurement-noise" js)))
    (qvm:with-random-state ((get-random-state (gethash "rng-seed" js)))
      ;; Basic logging
      (format-log "Got ~S request from API key/User ID: ~S / ~S" type api-key user-id)
      ;; Dispatch
      (ecase (keywordify type)
        ;; For simple tests.
        ((:ping)
         (handle-ping))

        ;; Get the version of everything.
        ((:version)
         (handle-version))

        ;; Get misc info about the simulation mode, shared memory, etc.
        ((:info)
         (handle-info))

        ;; Multishot experiments.
        ((:multishot)
         (check-required-fields js "addresses" "trials")
         (check-for-quil-instrs-field js)
         (let* ((addresses (gethash "addresses" js))
                (num-trials (gethash "trials" js))
                (isns (get-quil-instrs-field js))
                (quil (let ((quil::*allow-unresolved-applications* t))
                        (process-quil (safely-parse-quil-string isns))))
                (num-qubits (cl-quil:qubits-needed quil))
                (results (perform-multishot *simulation-method* quil num-qubits addresses num-trials
                                            :gate-noise gate-noise
                                            :measurement-noise measurement-noise)))
           (check-type results hash-table)
           (with-output-to-string (s)
             (yason:encode results s))))

        ;; Multishot with final measure.
        ((:multishot-measure)
         (check-required-fields js "qubits" "trials")
         (check-for-quil-instrs-field js)
         (let* ((quil:*allow-unresolved-applications* t)
                (qubits (gethash "qubits" js))
                (num-trials (gethash "trials" js)))
           (multiple-value-bind (quil relabeling)
               (process-quil (safely-parse-quil-string
                              (get-quil-instrs-field js)))
             (let* ((num-qubits (cl-quil:qubits-needed quil))
                    (results (perform-multishot-measure
                              *simulation-method*
                              quil
                              num-qubits
                              qubits
                              num-trials
                              relabeling)))
               (with-output-to-string (s)
                 (encode-list-as-json-list results s))))))

        ;; Expectation value computation.
        ((:expectation)
         (check-required-fields js "state-preparation" "operators")
         (let* ((quil:*allow-unresolved-applications* t)
                (state-prep (safely-parse-quil-string
                             (gethash "state-preparation" js)))
                (operators (map 'list #'safely-parse-quil-string
                                (gethash "operators" js)))
                (num-qubits (loop :for p :in (cons state-prep operators)
                                  :maximize (cl-quil:qubits-needed p)))
                (results (perform-expectation *simulation-method* state-prep operators num-qubits
                                              :gate-noise gate-noise
                                              :measurement-noise measurement-noise)))
           (with-output-to-string (s)
             (encode-list-as-json-list results s))))

        ;; Wavefunction computation.
        ((:wavefunction)
         (check-for-quil-instrs-field js)
         (let* ((isns (get-quil-instrs-field js))
                (quil (let ((quil:*allow-unresolved-applications* t))
                        (process-quil (safely-parse-quil-string isns))))
                (num-qubits (cl-quil:qubits-needed quil)))
           (let ((qvm (perform-wavefunction *simulation-method* quil num-qubits
                                            :gate-noise gate-noise
                                            :measurement-noise measurement-noise))
                 send-response-time)
             (with-timing (send-response-time)
               (setf (tbnl:content-type*) "application/octet-stream")
               (setf (tbnl:header-out ':ACCEPT) "application/octet-stream")
               (setf (tbnl:content-length*)
                     (qvm:octets-required-for-quantum-state (qvm::amplitudes qvm)))
               (let ((reply-stream (tbnl:send-headers)))
                 ;; Write out the wavefunction.
                 (qvm:map-amplitudes
                  qvm
                  (lambda (z) (write-complex-double-float-as-binary z reply-stream)))))
             (format-log "Response sent in ~D ms." send-response-time))))

        ((:probabilities)
         (check-for-quil-instrs-field js)
         (let* ((isns (get-quil-instrs-field js))
                (quil (let ((quil:*allow-unresolved-applications* t))
                        (process-quil (safely-parse-quil-string isns))))
                (num-qubits (cl-quil:qubits-needed quil)))
           (let (send-response-time)
             (multiple-value-bind (qvm probabilities)
                 (perform-probabilities *simulation-method* quil num-qubits
                                        :gate-noise gate-noise
                                        :measurement-noise measurement-noise)
               (declare (ignore qvm))
               (with-timing (send-response-time)
                 (setf (tbnl:content-type*) "application/octet-stream")
                 (setf (tbnl:header-out ':ACCEPT) "application/octet-stream")
                 (setf (tbnl:content-length*)
                       (* qvm::+octets-per-flonum+ (length probabilities)))
                 (let ((reply-stream (tbnl:send-headers)))
                   (map nil
                        (lambda (x) (write-double-float-as-binary x reply-stream))
                        probabilities))))
             (format-log "Response sent in ~D ms." send-response-time))))

        ((:run-for-effect)
         (check-for-quil-instrs-field js)
         (let* ((isns (get-quil-instrs-field js))
                (quil (let ((quil:*allow-unresolved-applications* t))
                        (process-quil (safely-parse-quil-string isns))))
                (num-qubits (cl-quil:qubits-needed quil)))
           (perform-run-for-effect *simulation-method* quil num-qubits
                                   :gate-noise gate-noise
                                   :measurement-noise measurement-noise)
           (load-time-value
            (with-output-to-string (s)
              (yason:encode t s)))))))))

(defun handle-post-request-and-cleanup (request)
  "Call HANDLE-POST-REQUEST and do any necessary cleanup after (like gc)."
  (unwind-protect (handle-post-request request)
    (when (eq *allocation-description* 'qvm:c-allocation)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Trigger the garbage collector to ensure that foreign memory is freed
      ;; (see issue #198).
      ;;
      ;; TODO: This is a temporary fix and is not 100% satisfactory because it
      ;; potentially stops all threads for GC after each request.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (tg:gc :full t))))
