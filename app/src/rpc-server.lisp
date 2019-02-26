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

(defun missing-keys (request &rest keys)
  (loop :for key :in keys
        :unless (gethash key request)
          :collect key))

(defmacro with-required-keys ((request keys) &body body)
  ""
  `(progn
     (alexandria:if-let ((missing (apply #'missing-keys ,request ,keys)))
       (make-instance 'rpcq::|RPCError|
                      :|id| (gethash "id" ,request)
                      :|error| (format nil "The following required keys were missing: ~{~a~^, ~}" missing))
       ,@(progn body))))

(defun multishot (request)
  (check-type request hash-table)
  (with-required-keys (request '("quil" "addresses" "trials"))
    (let* ((quil-program (process-quil
                          (safely-parse-quil-string
                           (gethash "quil" request))))
           (addresses (gethash "addresses" request))
           (trials (gethash "trials" request))
           (gate-noise (gethash "gate-noise" request))
           (measurement-noise (gethash "measuremeant-noise" request))
           (num-qubits (cl-quil:qubits-needed quil-program))
           (results (perform-multishot *simulation-method* quil-program num-qubits addresses trials
                                       :measurement-noise measurement-noise
                                       :gate-noise gate-noise)))
      (alexandria:alist-hash-table
       `(("results" . ,results))))))

(defun multishot-measure (request)
  (check-type request hash-table)
  (with-required-keys (request '("quil" "qubits" "trials"))
    (multiple-value-bind (quil-program relabeling)
        (process-quil (safely-parse-quil-string (gethash "quil" request)))
      (let* ((qubits (gethash "qubits" request))
             (trials (gethash "trials" request))
             (num-qubits (cl-quil:qubits-needed quil-program))
             (results (perform-multishot-measure *simulation-method* quil-program
                                                 num-qubits qubits trials
                                                 relabeling)))
        (alexandria:alist-hash-table
         `(("results" . ,results)))))))

(defun expectation (request)
  (check-type request hash-table)
  (with-required-keys (request '("state-preparation" "operators"))
    (let* ((state-prep (safely-parse-quil-string (gethash "state-preparation" request)))
           (operators (mapcar #'safely-parse-quil-string (gethash "operators" request)))
           (gate-noise (gethash "gate-noise" request))
           (measurement-noise (gethash "measurement-noise" request))         
           (num-qubits (loop :for p :in (cons state-prep operators)
                             :maximize (cl-quil:qubits-needed p)))
           (results (perform-expectation *simulation-method* state-prep operators num-qubits
                                         :gate-noise gate-noise
                                         :measurement-noise measurement-noise)))
      (alexandria:alist-hash-table
       `(("results" . ,results))))))

(defun wavefunction (request)
  (check-type request hash-table)
  (with-required-keys (request '("quil"))
    (let* ((quil (process-quil (safely-parse-quil-string (gethash "quil" request))))
           (gate-noise (gethash "gate-noise" request))
           (measurement-noise (gethash "measurement-noise" request))    
           (num-qubits (cl-quil:qubits-needed quil)))
      (let ((qvm (perform-wavefunction *simulation-method* quil num-qubits
                                       :gate-noise gate-noise
                                       :measurement-noise measurement-noise)))
        (alexandria:alist-hash-table
         `(("results" . ,(coerce (qvm::amplitudes qvm) 'list))))))))

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
