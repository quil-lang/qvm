;;;; handle-request.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun extract-json-payload (request)
  (let ((js (ignore-errors (yason:parse
                            (hunchentoot:raw-post-data :request request
                                                       :force-text t)))))
    (unless  (hash-table-p js)
      (error "Invalid JSON payload."))
    js))

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
    (null          (qvm:seeded-random-state nil))
    (unsigned-byte (qvm:seeded-random-state arg))))

(defun check-required-fields (hash-table &rest fields)
  (dolist (field fields t)
    (unless (nth-value 1 (gethash field hash-table))
      (error "Expected the field ~A to exist." field))))

(defun get-quil-instrs-field (hash-table)
  (or (gethash "quil-instructions" hash-table)
      (gethash "compiled-quil" hash-table)
      (gethash "uncompiled-quil" hash-table)))

(defun check-for-quil-instrs-field (hash-table)
  (unless (get-quil-instrs-field hash-table)
    (error "Expected a QUIL-INSTRUCTIONS type field to exist.")))

(defun %execute (qvm)
  (let (timing)
    (format-log "Running experiment on ~A" (class-name (class-of qvm)))
    (with-timing (timing)
      (with-timeout
        (qvm:run qvm)))
    (format-log "Finished in ~D ms" timing)
    qvm))

(defun post-request-type (type-string)
  (gethash (string-upcase type-string)
           (load-time-value
            (alexandria:alist-hash-table
             (loop :for kw :in '(:ping :version :info
                                 :multishot :multishot-measure
                                 :expectation :wavefunction)
                   :collect (list (symbol-name kw) kw))
             :test 'equal)
            t)
           ':unknown))

(defun handle-post-request (request)
  (when (null tbnl:*session*)
    (tbnl:start-session))

  (let* ((js (let ((js (extract-json-payload request)))
               (check-required-fields js "type")
               js))
         (type (post-request-type (gethash "type" js)))
         (gate-noise (gethash "gate-noise" js))
         (measurement-noise (gethash "measurement-noise" js))
         (client-ip (tbnl:real-remote-addr request))
         (persistent-qvm-key (gethash "persistent-key" js))
         (persistent-qvm-record
           (if (null persistent-qvm-key)
               nil
               (lookup-persistent-qvm-for-ip persistent-qvm-key client-ip))))
    ;; (check-type num-qubits (integer 0))
    (check-type gate-noise (or null alexandria:proper-list))
    (check-type measurement-noise (or null alexandria:proper-list))
    (assert (and (or (null gate-noise)
                     (= 3 (length gate-noise)))
                 (every #'realp gate-noise)))
    (assert (and (or (null measurement-noise)
                     (= 3 (length measurement-noise)))
                 (every #'realp measurement-noise)))

    (qvm:with-random-state ((get-random-state (gethash "rng-seed" js)))
      ;; Basic logging
      (format-log "Got a ~S request from ~S" type client-ip)
      ;; Dispatch
      (ecase type
        ((:unknown)
         (error "Unknown request type: ~A" type))

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
           (yason-encode-to-string results)))

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
                (quil (cond
                        ((null persistent-qvm-record)
                         (process-quil (safely-parse-quil-string isns)))
                        (t
                         (safely-parse-quil-string isns))))
                (num-qubits (cl-quil:qubits-needed quil))
                (qvm (cond
                       ((null persistent-qvm-record)
                        (let ((q (make-appropriate-qvm quil num-qubits gate-noise measurement-noise)))
                          (qvm:load-program q quil)
                          q))
                       (t
                        (let ((q (persistent-record-qvm persistent-qvm-record)))
                          (overwrite-execution-parameters-according-to-program
                           q quil
                           :gate-noise gate-noise
                           :measurement-noise measurement-noise)
                          q))))
                send-response-time)
           (setf qvm (%execute qvm))
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
           (format-log "Response sent in ~D ms." send-response-time)))))))
