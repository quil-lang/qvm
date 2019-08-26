(in-package :qvm-app-ng)

(defun check-simulation-method (simulation-method)
  (check-type simulation-method string)
  (assert (member simulation-method **available-simulation-methods** :test #'string=)))

(defun check-non-negative (n)
  (check-type n integer)
  (assert (not (minusp n))))

(global-vars:define-global-var **qvm-token-scanner**
  (cl-ppcre:create-scanner "\\A[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-4[A-Fa-f0-9]{3}-[89ABab][A-Fa-f0-9]{3}-[A-Fa-f0-9]{12}\\z"
                           :case-insensitive-mode t))

(defun check-qvm-token (token)
  (check-type token persistent-qvm-token)
  (cl-ppcre:scan **qvm-token-scanner** token))

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
  (assert (valid-address-query-p addresses) ()
          "Detected invalid address parameter. The requested addresses should be a JSON object ~
           whose keys are DECLAREd memory names, and whose values are either the true value to ~
           request all memory, or a list of non-negative integer indexes to request some memory.")
  addresses)

(defun parse-quil-string (string)
  "Safely parse a Quil string STRING."
  (flet ((no-includes (path)
           (error "INCLUDE is disabled. Refusing to include ~A" path)))
    (let ((quil:*resolve-include-pathname* #'no-includes)
          (quil::*allow-unresolved-applications* t))
      (quil:parse-quil string))))
