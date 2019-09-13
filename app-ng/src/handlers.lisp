(in-package :qvm-app-ng)

(defvar *rpc-handlers* '()
  "An alist of (rpc-method-name . handler-function) conses defined by DEFINE-RPC-HANDLER.")

(defun dispatch-rpc-handlers (request)
  "Called by TBNL:ACCEPTOR-DISPATCH-REQUEST to determine if any of the handlers in *RPC-HANDLERS* should handle the given REQUEST."
  (loop :for (rpc-method . handler) :in *rpc-handlers*
        :when (and (eq ':POST (tbnl:request-method request))
                   (string= "/" (tbnl:script-name request))
                   (boundp '*request-json*)
                   (string= rpc-method (gethash "type" *request-json*)))
          :do (return-from dispatch-rpc-handlers handler)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-existing-rpc-handlers (rpc-method name)
    (remove-if (lambda (handler-spec)
                 (or (eq name (cdr handler-spec))
                     (string= rpc-method (car handler-spec))))
               *rpc-handlers*))

  (defun make-json-parameter (var)
    `(,var (and (boundp '*request-json*) (json-parameter ,(string-downcase (symbol-name var))))))

  (defun make-parsed-binding (parameter-spec)
    (destructuring-bind (var parse-function) parameter-spec
      `(,var (funcall (alexandria:ensure-function ,parse-function) ,var))))

  (defun valid-rpc-handler-lambda-list-p (lambda-list)
    (every (lambda (parameter-spec)
             (and (= 2 (length parameter-spec))
                  (symbolp (first parameter-spec))))
           lambda-list)))

(defmacro define-rpc-handler ((handler-name rpc-method) lambda-list &body body)
  "Define an RPC handler named HANDLER-NAME for the RPC method RPC-METHOD.

DEFINE-RPC-HANDLER is like TBNL:DEFINE-EASY-HANDLER, but modified to dispatch on the RPC-METHOD name rather than URI path and to default parameters in LAMBDA-LIST based on JSON parameters rather than GET/POST parameters.

HANDLER-NAME is a SYMBOL and becomes the name of the FUNCTION that gets defined.

RPC-METHOD is a STRING that defines the RPC method name that is used to call this handler. DISPATCH-RPC-HANDLERS will check the caller-provided JSON request body and dispatch to this handler if the \"type\" field is STRING= to RPC-METHOD.

LAMBDA-LIST is a LIST of (VAR-NAME PARSE-FUNCTION) pairs. Each VAR-NAME will expand to an &KEY parameter in the lambda-list of HANDLER-NAME, and will be bound to the caller-provided JSON request parameters with the corresponding name. The JSON parameter name is derived by converting VAR-NAME to a lowercase string. So, e.g., if VAR-NAME is FOO-BAR, then FOO-BAR is bound in BODY to the value of (JSON-PARAMETER \"foo-bar\"). This defaulting of VAR-NAME only occurs in contexts where *REQUEST-JSON* is bound; otherwise, the value of VAR-NAME defaults to NIL. Parameter defaulting is done in this way to allow HANDLER-NAME to be conveniently called outside the server context with the caller providing the values of the keyword arguments directly.

Additionally, the specified PARSE-FUNCTION will be called to parse and validate the caller-provided value. PARSE-FUNCTION should raise an ERROR for invalid inputs, and should return a parsed value of whatever type is required in the BODY of the function. Note that PARSE-FUNCTION is called both for defaulted and non-defaulted arguments. The intention is to reduce argument unpacking/validation boilerplate in BODY and to make it difficult to forget to validate user input.

EXAMPLE:

    (define-rpc-handler (handle-frobnicate \"frobnicate\") ((number-of-times parse-non-negative))
      (frob-vigorously number-of-times))

defines a function named HANDLE-FROBNICATE that accepts a single keyword argument :NUMBER-OF-TIMES and registers it to be dispatched by DISPATCH-RPC-HANDLERS whenever a request is made to call the \"frobnicate\" RPC method. That is, when an HTTP request like the following is received:

    POST / HTTP/1.1
    Content-Type: application/json
    Host: 127.0.0.1:5000

    {
        \"type\": \"frobnicate\"
        \"number-of-times\": 42,
    }

Alternatively, the handler function can be called from lisp like so

    (handle-frobnicate :number-of-times 11)
"
  (check-type handler-name symbol)
  (check-type rpc-method string)
  (assert (valid-rpc-handler-lambda-list-p lambda-list))
  (multiple-value-bind (body-forms declarations docstring)
      (alexandria:parse-body body :documentation t)
   `(progn
      (setf *rpc-handlers* (acons ,rpc-method ',handler-name
                                  (remove-existing-rpc-handlers ,rpc-method ',handler-name)))
      (defun ,handler-name (&key ,@(mapcar (alexandria:compose #'make-json-parameter #'first)
                                           lambda-list))
        ,@(when docstring (list docstring))
        (let (,@(mapcar #'make-parsed-binding lambda-list))
          ,@(when declarations declarations)
          (qvm:with-random-state ((get-random-state (and (boundp '*request-json*)
                                                         (json-parameter "rng-seed"))))
              ,@body-forms))))))

(defun collect-memory-registers (qvm addresses)
  "Return a HASH-TABLE of exactly the QVM memory registers requested in ADDRESSES.

ADDRESSES is a HASH-TABLE where the keys are mem register names and the values are either T to indicate that we should return all indices for the corresponding register, or else a LIST of the desired indices for that register."
  (let ((results (make-hash-table :test 'equal)))
    (maphash (lambda (name indexes)
               (cond
                 ;; Give everything back.
                 ((eq indexes t)
                  (loop :with mv := (gethash name (qvm::classical-memories qvm))
                        :for idx :below (qvm::memory-view-length mv)
                        :collect (qvm:memory-ref qvm name idx) :into mem
                        :finally (push mem (gethash name results))))
                 ;; Give only some things back.
                 ((alexandria:proper-list-p indexes)
                  (loop :for idx :in indexes
                        :collect (qvm:memory-ref qvm name idx) :into mem
                        :finally (push mem (gethash name results))))
                 (t
                  (error "Invalid address parameter for memory named ~S." name))))
             addresses)
    results))

(define-rpc-handler (handle-version "version") ()
  "Return QVM-APP-NG version info as a string."
  ;; text/html rather than application/json for backwards compatibility with previous QVM-APP API.
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (*standard-output*)
     (show-version))))

(define-rpc-handler (handle-create-qvm "create-qvm") ((allocation-method #'parse-allocation-method)
                                                      (simulation-method #'parse-simulation-method)
                                                      (num-qubits #'parse-num-qubits)
                                                      (gate-noise #'parse-optional-pauli-noise)
                                                      (measurement-noise #'parse-optional-pauli-noise))
  "Create the requested persistent QVM.

SIMULATION-METHOD is a STRING naming the desired simulation method (see *AVAILABLE-SIMULATION-METHODS*).

NUM-QUBITS is a non-negative integer and represents the maximum number of qubits available on the QVM."
  (encode-json (alexandria:plist-hash-table
                `("token" ,(allocate-persistent-qvm
                            (make-requested-qvm simulation-method
                                                allocation-method
                                                num-qubits
                                                gate-noise
                                                measurement-noise)
                            allocation-method)))))

(define-rpc-handler (handle-delete-qvm "delete-qvm") ((qvm-token #'parse-qvm-token))
  "Delete a persistent QVM.

QVM-TOKEN is a valid persistent QVM token such as one returned by the CREATE-QVM RPC call."
  (delete-persistent-qvm qvm-token)
  (encode-json (format nil "Deleted persistent QVM ~D" qvm-token)))

(define-rpc-handler (handle-qvm-info "qvm-info") ((qvm-token #'parse-qvm-token))
  "Return some basic bookkeeping info about the specified QVM.

QVM-TOKEN is a valid persistent QVM token such as one returned by the CREATE-QVM RPC call."
  (encode-json (persistent-qvm-info qvm-token)))

(define-rpc-handler (handle-run-program "run-program")
                    ((qvm-token #'parse-optional-qvm-token)
                     (allocation-method #'parse-optional-allocation-method)
                     (simulation-method #'parse-optional-simulation-method)
                     (compiled-quil #'parse-quil-string)
                     (addresses #'parse-addresses)
                     (gate-noise #'parse-optional-pauli-noise)
                     (measurement-noise #'parse-optional-pauli-noise))
  "Run the requested COMPILED-QUIL program, either on a persistent QVM or an emphemeral QVM using the given SIMULATION-METHOD.

QVM-TOKEN is a valid persistent QVM token as returned by the CREATE-QVM RPC call.

SIMULATION-METHOD is a STRING naming the desired simulation method (see *AVAILABLE-SIMULATION-METHODS*).

COMPILED-QUIL is a STRING containing a valid Quil program.

ADDRESSES is a HASH-TABLE where the keys are mem register names and the values are either T to indicate that we should return all indices for the corresponding register, or else a LIST of the desired indices for that register.

The caller must provide either QVM-TOKEN or SIMULATION-METHOD, but not both."
  (when (and (null qvm-token)
             (not (and allocation-method simulation-method)))
    (rpc-parameter-parse-error
     "Either QVM-TOKEN or both ALLOCATION-METHOD and SIMULATION-METHOD must be present in the JSON request parameters."))
  (when (and qvm-token (or allocation-method
                           simulation-method
                           gate-noise
                           measurement-noise))
    (rpc-parameter-parse-error
     "QVM-TOKEN is incompatible with any of the following parameters: ALLOCATION-METHOD, SIMULATION-METHOD, GATE-NOISE, MEASUREMENT-NOISE."))
  (encode-json
   (collect-memory-registers
    (if qvm-token
        (run-program-on-persistent-qvm qvm-token compiled-quil)
        (run-program-on-qvm (make-requested-qvm simulation-method
                                                allocation-method
                                                (quil:qubits-needed compiled-quil)
                                                gate-noise
                                                measurement-noise)
                            compiled-quil))
    addresses)))
