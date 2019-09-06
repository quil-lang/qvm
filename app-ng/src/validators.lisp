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

(defun parse-simulation-method (simulation-method)
  (unless (and (typep simulation-method 'string)
               (member simulation-method **available-simulation-methods** :test #'string=))
    (rpc-parameter-parse-error "Invalid SIMULATION-METHOD. Expected one of: ~{~S~^, ~}. Got ~S"
                               **available-simulation-methods**
                               simulation-method))
  (intern (string-upcase simulation-method) :qvm-app-ng))

(defun parse-optional-simulation-method (simulation-method)
  (and simulation-method (parse-simulation-method simulation-method)))

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
