;;;; rpc-server
;;;;
;;;; Author: Mark Skilbeck

(in-package #:qvm-app)


;; Let's Mock. Minimum requirements for the interface taken from
;; handle-request.lisp: ping, version, info, multishot,
;; multishot-measure, expectation, wavefunction.

(defun get-version-info ()
  (alexandria:alist-hash-table
   `(("qvm" . ,+QVM-VERSION+)
     ("githash" . ,+GIT-HASH+))))

(defun get-info ()
  (alexandria:plist-hash-table
   (list "simulation-method" (princ-to-string *simulation-method*)
         "shared-memory-object-name" (princ-to-string *shared-memory-object-name*)
         "qubit-limit" (princ-to-string *qubit-limit*))
   :test 'equal))

(defun multishot (request)
  (check-type request rpcq::|MultishotRequest|)
  (let* ((quil-program (process-quil
                        (safely-parse-quil-string
                         (rpcq::|MultishotRequest-quil| request))))
         (addresses (rpcq::|MultishotRequest-addresses| request))
         (trials (rpcq::|MultishotRequest-trials| request))
         (gate-noise (rpcq::|MultishotRequest-gate-noise| request))
         (measurement-noise (rpcq::|MultishotRequest-measurement-noise| request))
         (num-qubits (cl-quil:qubits-needed quil-program))
         (results (perform-multishot *simulation-method* quil-program num-qubits addresses trials
                                     :measurement-noise measurement-noise
                                     :gate-noise gate-noise)))
    (format-log "testing")
    (make-instance 'rpcq::|MultishotResponse|
                   :|results| results)))

(defun multishot-measure (request)
  (check-type request rpcq::|MultishotMeasureRequest|)
  (multiple-value-bind (quil-program relabeling)
      (process-quil (safely-parse-quil-string (rpcq::|MultishotMeasureRequest-quil| request)))
    (let* ((qubits (rpcq::|MultishotMeasureRequest-qubits| request))
           (trials (rpcq::|MultishotMeasureRequest-trials| request))
           (num-qubits (cl-quil:qubits-needed quil-program))
           (results (perform-multishot-measure *simulation-method* quil-program
                                               num-qubits qubits trials
                                               relabeling)))
      (make-instance 'rpcq::|MultishotMeasureResponse|
                     :|results| results))))

(defun expectation (request)
  (check-type request rpcq::|ExpectationRequest|)
  (let* ((state-prep (safely-parse-quil-string (rpcq::|ExpectationRequest-state-preparation| request)))
         (operators (mapcar #'safely-parse-quil-string (rpcq::|ExpectationRequest-operators| request)))
         (gate-noise (rpcq::|ExpectationRequest-gate-noise| request))
         (measurement-noise (rpcq::|ExpectationRequest-measurement-noise| request))         
         (num-qubits (loop :for p :in (cons state-prep operators)
                           :maximize (cl-quil:qubits-needed p)))
         (results (perform-expectation *simulation-method* state-prep operators num-qubits
                                       :gate-noise gate-noise
                                       :measurement-noise measurement-noise)))
    (make-instance 'rpcq::|ExpectationResponse|
                   :|results| results)))

(defun wavefunction (request)
  (check-type request rpcq::|WavefunctionRequest|)
  (let* ((quil (process-quil (safely-parse-quil-string (rpcq::|WavefunctionRequest-quil| request))))
         (gate-noise (rpcq::|WavefunctionRequest-gate-noise| request))
         (measurement-noise (rpcq::|WavefunctionRequest-measurement-noise| request))    
         (num-qubits (cl-quil:qubits-needed quil)))
    (let ((qvm (perform-wavefunction *simulation-method* quil num-qubits
                                     :gate-noise gate-noise
                                     :measurement-noise measurement-noise)))
      (make-instance 'rpcq::|WavefunctionResponse|
                     :|results| (coerce (qvm::amplitudes qvm) 'list)))))

(declaim (special *program-name*))
(defun start-rpc-server (&key
                           (port 5000)
                           (logger (make-instance 'cl-syslog:rfc5424-logger
                                                  :log-writer (cl-syslog:null-log-writer))))
  (let ((dt (rpcq:make-dispatch-table)))
    (rpcq:dispatch-table-add-handler dt 'multishot)
    (rpcq:dispatch-table-add-handler dt 'multishot-measure)
    (rpcq:dispatch-table-add-handler dt 'expectation)
    (rpcq:dispatch-table-add-handler dt 'wavefunction)
    (rpcq:dispatch-table-add-handler dt 'get-version-info)
    (rpcq:dispatch-table-add-handler dt 'get-info)
    (rpcq:start-server :dispatch-table dt
                       :listen-addresses (list (format nil "tcp://*:~a" port))
                       :logger logger
                       :timeout 60)))
