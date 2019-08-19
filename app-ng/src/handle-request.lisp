;;;; handle-request.lisp
;;;;
;;;; Author: Robert Smith, appleby

(in-package #:qvm-app-ng)

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

(defun get-quil-instrs-field (hash-table)
  (or (gethash "quil-instructions" hash-table)
      (gethash "compiled-quil" hash-table)
      (gethash "uncompiled-quil" hash-table)))

(defun check-required-fields (hash-table &rest fields)
  (dolist (field fields t)
    (when (null (nth-value 1 (gethash field hash-table)))
      (error "Expected the field ~A to exist." field))))

(defun check-for-quil-instrs-field (hash-table)
  (unless (get-quil-instrs-field hash-table)
    (error "Expected a QUIL-INSTRUCTIONS type field to exist.")))

(defun check-persistent-qvm-token (token)
  (unless (typep token 'persistent-qvm-token)
    (error "Expected PERSISTENT-QVM-TOKEN. Got ~A" token)))

(defun handle-post-request (request)
  (when (null tbnl:*session*)
    (tbnl:start-session))

  (let* ((api-key (tbnl:header-in* ':X-API-KEY request))
         (user-id (tbnl:header-in* ':X-USER-ID request))
         (data (hunchentoot:raw-post-data :request request :force-text t))
         (js (let* ((js (ignore-errors (yason:parse data))))
               (unless (and (hash-table-p js)
                            (check-required-fields js "type"))
                 (error "Invalid request."))
               js))
         (type (gethash "type" js))
         (gate-noise (gethash "gate-noise" js))
         (measurement-noise (gethash "measurement-noise" js)))
    (qvm:with-random-state ((get-random-state (gethash "rng-seed" js)))
      (format-log "Got ~S request from API key/User ID: ~S / ~S" type api-key user-id)
      (ecase (keywordify type)
        ((:version)
         (handle-version))

        ((:delete-persistent-qvm)
         (check-required-fields js "persistent-qvm-token")
         (check-persistent-qvm-token (gethash "persistent-qvm-token" js))
         (handle-delete-persistent-qvm (gethash "persistent-qvm-token" js)))

        ((:make-persistent-qvm)
         (check-required-fields js "simulation-method" "number-of-qubits")
         (check-type (gethash "simulation-method" js) string)
         (check-type (gethash "number-of-qubits" js) alexandria:non-negative-fixnum)
         (let* ((simulation-method (parse-simulation-method (gethash "simulation-method" js)))
                (num-qubits (gethash "number-of-qubits" js))
                ;; TODO:(appleby) Pass an empty quil program to appease MAKE-APPROPRIATE-QVM so that
                ;; testing can move forward. Ultimately, noise models need to be configurable.
                (quil (quil:parse-quil ""))
                (results (handle-make-persistent-qvm simulation-method quil num-qubits
                                                     :gate-noise gate-noise
                                                     :measurement-noise measurement-noise)))
           (check-type results hash-table)
           (with-output-to-string (s)
             (yason:encode results s))))

        ((:persistent-qvm-info)
         (check-required-fields js "persistent-qvm-token")
         (check-persistent-qvm-token (gethash "persistent-qvm-token" js))
         (handle-persistent-qvm-info (gethash "persistent-qvm-token" js)))

        ((:multishot)
         (check-required-fields js "addresses" "trials")
         (check-for-quil-instrs-field js)
         (let* ((addresses (gethash "addresses" js))
                (num-trials (gethash "trials" js))
                (isns (get-quil-instrs-field js))
                (persistent-qvm-token (gethash "persistent-qvm-token" js))
                (quil (let ((quil::*allow-unresolved-applications* t))
                        (process-quil (safely-parse-quil-string isns))))
                (num-qubits (cl-quil:qubits-needed quil))
                results)
           (cond ((null persistent-qvm-token)
                  (setf results (perform-multishot *simulation-method* quil num-qubits addresses num-trials
                                                   :gate-noise gate-noise
                                                   :measurement-noise measurement-noise)))
                 (t
                  (check-persistent-qvm-token persistent-qvm-token)
                  (with-persistent-qvm (qvm) persistent-qvm-token
                    ;; TODO:(appleby) do we want :supersede-memory-subsystem here?
                    (setf results (perform-multishot-on-qvm qvm quil addresses num-trials)))))
           (check-type results hash-table)
           (with-output-to-string (s)
             (yason:encode results s))))))))
