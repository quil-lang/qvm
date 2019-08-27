(in-package :qvm-app-ng)

(defun check-simulation-method (simulation-method)
  (unless (and (typep simulation-method 'string)
               (member simulation-method **available-simulation-methods** :test #'string=))
    (rpc-parameter-parse-error "Invalid SIMULATION-METHOD. Expected one of: ~{~S~^, ~}. Got ~S"
                               **available-simulation-methods**
                               simulation-method)))

(defun check-non-negative (n)
  (unless (typep n `(integer 0))
    (rpc-parameter-parse-error "Invalid input parameter. Expected a non-negative integer. Got ~S"
                               n)))

(global-vars:define-global-var **qvm-token-scanner**
  (cl-ppcre:create-scanner "\\A[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-4[A-Fa-f0-9]{3}-[89ABab][A-Fa-f0-9]{3}-[A-Fa-f0-9]{12}\\z"
                           :case-insensitive-mode t))

(defun check-qvm-token (token)
  (unless (and (typep token 'persistent-qvm-token)
               (cl-ppcre:scan **qvm-token-scanner** token))
    (rpc-parameter-parse-error "Invalid persistent QVM token. Expected a v4 UUID. Got ~S"
                               token)))

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

(defun check-addresses (addresses)
  (unless (valid-address-query-p addresses)
    (rpc-parameter-parse-error
     "Invalid address parameter. The requested addresses should be a JSON object whose keys ~
      are DECLAREd memory names, and whose values are either the true value to request all ~
      memory, or a list of non-negative integer indexes to request some memory.")))

(defun parse-qvm-token (qvm-token)
  (check-qvm-token qvm-token)
  qvm-token)

(defun parse-optional-qvm-token (qvm-token)
  (and qvm-token (parse-qvm-token qvm-token)))

(defun parse-simulation-method (simulation-method)
  (check-simulation-method simulation-method)
  (intern (string-upcase simulation-method) :qvm-app-ng))

(defun parse-optional-simulation-method (simulation-method)
  (and simulation-method (parse-simulation-method simulation-method)))

(defun parse-num-qubits (num-qubits)
  (check-non-negative num-qubits)
  num-qubits)

(defun parse-addresses (addresses)
  (check-addresses addresses)
  addresses)

(defun parse-quil-string (string)
  "Safely parse a Quil string STRING."
  (flet ((no-includes (path)
           (rpc-parameter-parse-error
            "Invalid Quil string. INCLUDE is disabled. Refusing to include ~A" path)))
    (let ((quil:*resolve-include-pathname* #'no-includes)
          (quil::*allow-unresolved-applications* t))
      (quil:parse-quil string))))
