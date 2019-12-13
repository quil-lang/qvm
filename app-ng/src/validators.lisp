(in-package #:qvm-app-ng)

(defun optionally (parameter-parser)
  "Combinator for parsing optional parameters.

PARAMETER-PARSER is a function-designator for a function that accepts a single required argument.

Return a function that accepts a single PARAMETER and calls PARAMETER-PARSER on it if PARAMETER is non-NIL. Otherwise, return NIL."
  (lambda (parameter)
    (and parameter (funcall parameter-parser parameter))))

(defun %parse-uuid-token (token token-type-name)
  ;; Ensure it's a STRING before attempting to canonicalize the case. Otherwise, we'll get a
  ;; not-so-helpful error message.
  (unless (typep token 'string)
    (rpc-parameter-parse-error "Invalid ~A token. Expected a v4 UUID string. Got ~S"
                               token-type-name token))

  (let ((canonicalized-token (canonicalize-uuid-string token)))
    (unless (valid-uuid-string-p canonicalized-token)
      (rpc-parameter-parse-error "Invalid ~A token. Expected a v4 UUID. Got ~S"
                                 token-type-name token))
    canonicalized-token))

(defun parse-qvm-token (qvm-token)
  (%parse-uuid-token qvm-token "persistent QVM"))

(defun parse-job-token (job-token)
  (%parse-uuid-token job-token "JOB"))

(defun %parse-string-to-known-symbol (parameter-name parameter-string known-symbols
                                      &optional (package 'qvm-app-ng))
  (flet ((%error ()
           (rpc-parameter-parse-error "Invalid ~A. Expected one of: ~{~S~^, ~}. Got ~S"
                                      parameter-name
                                      (mapcar #'string-downcase known-symbols)
                                      parameter-string)))
    (unless (typep parameter-string 'string)
      (%error))
    (let ((symbol (find-symbol (string-upcase parameter-string) package)))
      (unless (and symbol (member symbol known-symbols))
        (%error))
      symbol)))

(defun parse-simulation-method (simulation-method)
  (%parse-string-to-known-symbol 'simulation-method simulation-method +available-simulation-methods+))

(defun parse-log-level (log-level)
  ;; TODO(appleby): This is only called when parsing command-line args, but returns an
  ;; RPC-PARAMETER-PARSE-ERROR on failure. As more command line options are added, these PARSE-*
  ;; functions should be converted to return a generic PARAMETER-ERROR, which DEFINE-RPC-HANDLER
  ;; arranges to convert to an RPC-PARAMETER-PARSE-ERROR.
  (%parse-string-to-known-symbol 'log-level log-level +available-log-levels+ 'keyword))

(defun parse-allocation-method (allocation-method)
  (%parse-string-to-known-symbol 'allocation-method allocation-method +available-allocation-methods+))

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

(defun valid-memory-contents-query-p (memory-contents)
  (cond
    ((not (hash-table-p memory-contents)) nil)
    (t
     (maphash (lambda (k v)
                (unless (and (stringp k)
                             (and (alexandria:proper-list-p v)
                                  (every (lambda (entry)
                                           (and (alexandria:proper-list-p entry)
                                                (= 2 (length entry))
                                                (integerp (first entry))
                                                (not (minusp (first entry)))
                                                (typep (second entry) '(or integer real))))
                                         v)))
                  (return-from valid-memory-contents-query-p nil)))
              memory-contents)
     t)))

(defun parse-addresses (addresses)
  (unless (valid-address-query-p addresses)
    (rpc-parameter-parse-error
     "Invalid ADDRESSES parameter. The requested addresses should be a JSON object whose keys are ~
      DECLAREd memory names, and whose values are either the true value to request all memory, or ~
      a list of non-negative integer indexes to request some memory."))
  addresses)

(defun parse-memory-contents (memory-contents)
  (unless (valid-memory-contents-query-p memory-contents)
    (rpc-parameter-parse-error "Invalid MEMORY-CONTENTS."))
  memory-contents)

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

(defun parse-sub-request (sub-request)
  (unless (hash-table-p sub-request)
    (rpc-bad-request-error "Invalid create-job SUB-REQUEST: not a valid JSON object: ~A" sub-request))
  (when (member (gethash "type" sub-request) '("create-job" "run-program-async") :test #'string=)
    (rpc-bad-request-error "Invalid create-job SUB-REQUEST type field: ~S."
                           (with-output-to-string (*standard-output*)
                             (yason:encode sub-request))))
  sub-request)
