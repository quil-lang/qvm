(in-package :qvm-app-ng)

(defun parse-qvm-token (qvm-token)
  ;; Ensure it's a STRING before attempting to canonicalize the case. Otherwise, we'll get a
  ;; not-so-helpful error message.
  (unless (typep qvm-token 'string)
    (rpc-parameter-parse-error "Invalid persistent QVM token. Expected a v4 UUID string. Got ~S"
                               qvm-token))

  (let ((canonicalized-token (canonicalize-persistent-qvm-token qvm-token)))
    (unless (valid-persistent-qvm-token-p canonicalized-token)
      (rpc-parameter-parse-error "Invalid persistent QVM token. Expected a v4 UUID. Got ~S"
                                 qvm-token))
    canonicalized-token))

(defun parse-optional-qvm-token (qvm-token)
  (and qvm-token (parse-qvm-token qvm-token)))

(defun %parse-string-to-known-symbol (parameter-name parameter-string known-symbols)
  (flet ((%error ()
           (rpc-parameter-parse-error "Invalid ~A. Expected one of: ~{~S~^, ~}. Got ~S"
                                      parameter-name
                                      (mapcar #'string-downcase known-symbols)
                                      parameter-string)))
    (unless (typep parameter-string 'string)
      (%error))
    (let ((symbol (find-symbol (string-upcase parameter-string) 'qvm-app-ng)))
      (unless (and symbol (member symbol known-symbols))
        (%error))
      symbol)))

(defun parse-simulation-method (simulation-method)
  (%parse-string-to-known-symbol "simulation-method"
                                 simulation-method
                                 **available-simulation-methods**))

(defun parse-optional-simulation-method (simulation-method)
  (and simulation-method (parse-simulation-method simulation-method)))

(defun parse-allocation-method (allocation-method)
  (%parse-string-to-known-symbol "allocation-method"
                                 allocation-method
                                 **available-allocation-methods**))

(defun parse-optional-allocation-method (allocation-method)
  (and allocation-method (parse-allocation-method allocation-method)))

(defun parse-num-qubits (num-qubits)
  (unless (typep num-qubits `(integer 0))
    (rpc-parameter-parse-error "Invalid NUM-QUBITS. Expected a non-negative integer. Got ~S"
                               num-qubits))
  num-qubits)

(defun valid-address-query-p (addresses)
  (cond
    ((not (hash-table-p addresses)) nil)
    (t
     (maphash (lambda (k v)
                (unless (and (stringp k)
                             (or (eq t v)
                                 (and (alexandria:proper-list-p v)
                                      (every #'integerp v)
                                      (notany #'minusp v))))
                  (return-from valid-address-query-p nil)))
              addresses)
     t)))

(defun parse-addresses (addresses)
  (unless (valid-address-query-p addresses)
    (rpc-parameter-parse-error
     "Invalid ADDRESSES parameter. The requested addresses should be a JSON object whose keys are ~
      DECLAREd memory names, and whose values are either the true value to request all memory, or ~
      a list of non-negative integer indexes to request some memory."))
  addresses)

(defun parse-quil-string (string)
  "Safely parse a Quil string STRING."
  (flet ((no-includes (path)
           (rpc-parameter-parse-error
            "Invalid Quil string. INCLUDE is disabled. Refusing to include ~A" path)))
    (let ((quil:*resolve-include-pathname* #'no-includes)
          (quil::*allow-unresolved-applications* t))
      (quil:parse-quil string))))

(defun parse-pauli-noise (noise)
  (unless (and (alexandria:proper-list-p noise)
               (= 3 (length noise))
               (every #'floatp noise))
    (rpc-parameter-parse-error "Invalid Pauli noise. Expected a LIST of three FLOATs. Got ~S" noise))
  noise)

(defun parse-optional-pauli-noise (noise)
  (and noise (parse-pauli-noise noise)))
