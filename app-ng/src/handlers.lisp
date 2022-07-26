;;;; app-ng/src/handlers.lisp
;;;;
;;;; author: appleby
(in-package #:qvm-app-ng)

(defvar *rpc-handlers* (make-hash-table :test #'equal)
  "A HASH-TABLE mapping an RPC method name (a STRING) to a SYMBOL that names a handler function defined by DEFINE-RPC-HANDLER.")

(defun lookup-rpc-handler-for-request (request-params)
  (gethash (gethash "type" request-params) *rpc-handlers*))

(defun dispatch-rpc-handlers (request)
  "Called by TBNL:ACCEPTOR-DISPATCH-REQUEST to determine if any of the handlers in *RPC-HANDLERS* should handle the given REQUEST."
  (and (eq ':POST (tbnl:request-method request))
       (string= "/" (tbnl:script-name request))
       (boundp '*request-json*)
       (lookup-rpc-handler-for-request *request-json*)))

(defun dispatch-rpc-request (request-json)
  "Dispatch the RPC request found in REQUEST-JSON.

DISPATCH-RPC-REQUEST is like TBNL:ACCEPTOR-DISPATCH-REQUEST, but may be called in non-HTTP contexts (RUN-INITIAL-RPC-REQUEST) or from a nested HTTP request (HANDLE-CREATE-JOB).

Lookup the handler for the given request, and return the result of calling that handler with *REQUEST-JSON* bound to REQUEST-JSON.

Signal an error if no handler is found."
  (alexandria:if-let ((handler (lookup-rpc-handler-for-request request-json)))
    (let ((*request-json* request-json))
      (funcall handler))
    (error "No handler found for rpc request: ~S"
           (with-output-to-string (*standard-output*)
             (yason:encode request-json)))))


;;;;;;;;;;;;;;;;;;;;;;;;; DEFINE-RPC-HANDLER ;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-existing-rpc-handlers (rpc-method handler-name &optional (handler-table *rpc-handlers*))
    "Remove any entries matching RPC-METHOD and/or HANDLER-NAME from HANDLER-TABLE."
    (maphash (lambda (key value)
               (when (or (eq handler-name value)
                         (string= rpc-method key))
                 (remhash key handler-table)))
             handler-table))

  (defun register-rpc-handler (rpc-method handler-name &optional (handler-table *rpc-handlers*))
    ;; This removes entries from the table if *either* rpc-method or handler-name matches.
    (remove-existing-rpc-handlers rpc-method handler-name handler-table)
    (setf (gethash rpc-method handler-table) handler-name))

  (defun make-json-parameter (var)
    `(,var (and (boundp '*request-json*) (json-parameter ,(string-downcase (symbol-name var))))))

  (defun make-parsed-binding (parameter-spec)
    (destructuring-bind (var parse-function) parameter-spec
      `(,var (call-with-rewrapped-simple-errors 'rpc-parameter-parse-error ,parse-function ,var))))

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
       (register-rpc-handler ,rpc-method ',handler-name)
       (defun ,handler-name (&key ,@(mapcar (alexandria:compose #'make-json-parameter #'first)
                                            lambda-list))
         ,@(when docstring (list docstring))
         (let (,@(mapcar #'make-parsed-binding lambda-list))
           ,@(when declarations declarations)
           (qvm:with-random-state ((get-random-state (and (boundp '*request-json*)
                                                          (json-parameter "rng-seed"))))
             ,@body-forms))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rpc-handler (handle-version "version") ()
  "Return a TEXT-RESPONSE that contains the QVM-APP-NG version."
  ;; text/html rather than application/json for backwards compatibility with previous QVM-APP API.
  (make-text-response (string-right-trim
                       '(#\Newline)
                       (with-output-to-string (*standard-output*)
                         (show-version)))))

(define-rpc-handler (handle-qvm-memory-estimate "qvm-memory-estimate")
                    ((allocation-method #'parse-allocation-method)
                     (simulation-method #'parse-simulation-method)
                     (num-qubits #'parse-num-qubits)
                     (gate-noise (optionally #'parse-pauli-noise))
                     (measurement-noise (optionally #'parse-pauli-noise)))
  "Return an estimate of the number of bytes required to store the quantum state of a QVM of the given type.

The number returned represents the number of bytes required to store the QVM state, i.e. to store the amplitudes of the wavefunction for a PURE-STATE simulation or for the density matrix for a FULL-DENSITY-MATRIX simulation. Note that the memory required to store QVM state represents a lower-bound on the memory required for simulation, since in the course of simulating programs the QVM will allocate additional memory. For the most part, these additional allocations are small (compared to the QVM state), short-lived, and difficult to predict in advance. We also ignore any memory allocated for the classical memory subsystem of the QVM, which is bounded by QVM::**CLASSICAL-MEMORY-SIZE-LIMIT** (64K by default).

The arguments accepted by this method are exactly the same as those accepted by the CREATE-QVM method.

SIMULATION-METHOD is a STRING naming the desired simulation method (see *AVAILABLE-SIMULATION-METHODS*).

ALLOCATION-METHOD is a STRING naming the desired allocation method (see *AVAILABLE-ALLOCATION-METHODS*).

NUM-QUBITS is a non-negative integer and represents the maximum number of qubits available on the QVM.

GATE-NOISE is an optional list of three FLOATs giving the probabilities of a Pauli X, Y, or Z gate happening after a gate application or a RESET.

MEASUREMENT-NOISE is an optional list of three FLOATs giving the probabilities of an X, Y, or Z gate happening before a MEASURE.

Return a JSON-RESPONSE object that contains a HASH-TABLE with a \"bytes\" key indicating the estimated number of bytes required."
  (declare (ignore allocation-method))
  (make-json-response
   (alexandria:plist-hash-table
    `("bytes" ,(octets-required-for-qvm
                (simulation-method->qvm-type simulation-method
                                             :pauli-noise-p (or gate-noise measurement-noise))
                num-qubits))
    :test #'equal)))


;;;;;;;;;;;;;;;;;;;;;; PERSISTENT QVM HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;

(define-rpc-handler (handle-create-qvm "create-qvm")
                    ((allocation-method #'parse-allocation-method)
                     (simulation-method #'parse-simulation-method)
                     (num-qubits #'parse-num-qubits)
                     (gate-noise (optionally #'parse-pauli-noise))
                     (measurement-noise (optionally #'parse-pauli-noise)))
  "Create the requested persistent QVM.

SIMULATION-METHOD is a STRING naming the desired simulation method (see *AVAILABLE-SIMULATION-METHODS*).

ALLOCATION-METHOD is a STRING naming the desired allocation method (see *AVAILABLE-ALLOCATION-METHODS*).

NUM-QUBITS is a non-negative integer and represents the maximum number of qubits available on the QVM.

GATE-NOISE is an optional list of three FLOATs giving the probabilities of a Pauli X, Y, or Z gate happening after a gate application or a RESET.

MEASUREMENT-NOISE is similarly an optional list of three FLOATs giving the probabilities of an X, Y, or Z gate happening before a MEASURE.

Return a JSON-RESPONSE that contains a HASH-TABLE with a \"token\" key with the newly-created persistent QVM's unique ID token."
  (make-json-response (alexandria:plist-hash-table
                       `("token" ,(allocate-persistent-qvm
                                   (make-requested-qvm simulation-method
                                                       allocation-method
                                                       num-qubits
                                                       gate-noise
                                                       measurement-noise)
                                   allocation-method))
                       :test #'equal)
                      ;; TODO(appleby): is it bad netiquette to return 201 Created without setting
                      ;; the Location header? Since we use JSON-RPC semantics, it's not clear what
                      ;; Location should be. The spec seems to indicate that it's OK to not set
                      ;; Location if it's equal to the request URL. I suppose that's technically
                      ;; true, since all RPC requests are served from /, but it definitely feels
                      ;; like we're muddying the HTTP semantics.
                      :status +http-created+))

(define-rpc-handler (handle-qvm-info "qvm-info") ((qvm-token #'parse-qvm-token))
  "Return some basic bookkeeping info about the specified QVM.

QVM-TOKEN is a valid persistent QVM token returned by the CREATE-QVM RPC call."
  (make-json-response (persistent-qvm-info qvm-token)))

(define-rpc-handler (handle-delete-qvm "delete-qvm") ((qvm-token #'parse-qvm-token))
  "Delete a persistent QVM.

QVM-TOKEN is a valid persistent QVM token returned by the CREATE-QVM RPC call."
  (delete-persistent-qvm qvm-token)
  (make-json-response (format nil "Deleted persistent QVM ~A" qvm-token)))

(define-rpc-handler (handle-resume-from-wait "resume") ((qvm-token #'parse-qvm-token))
  "Resume execution of a persistent QVM that is in the WAITING state.

QVM-TOKEN is a valid persistent QVM token returned by the CREATE-QVM RPC call.

It is an error to try to resume a QVM in any state other than the WAITING state.

Return a JSON-RESPONSE that contains T on success."
  (resume-persistent-qvm qvm-token)
  (make-json-response t))

(define-rpc-handler (handle-read-memory "read-memory")
                    ((qvm-token #'parse-qvm-token)
                     (addresses #'parse-addresses))
  "Read from a persistent QVM's classical memory registers.

DANGER! DANGER! DANGER! The effects of reading from classical memory while a persistent QVM is running are undefined. Although this restriction is not (currently) enforced, for safety you should only read from a QVM's memory when it is in the READY state or the WAITING state due to executing a WAIT instruction.

QVM-TOKEN is a valid persistent QVM token returned by the CREATE-QVM RPC call.

ADDRESSES is a HASH-TABLE where the keys are mem register names and the values are either T to indicate that we should return all indices for the corresponding register, or else a LIST of the desired indices for that register.

Return a JSON-RESPONSE that contains a HASH-TABLE of the contents of the memory registers requested in the ADDRESSES request parameter."
  (make-json-response
   (with-persistent-qvm (qvm) qvm-token
     (collect-memory-registers qvm addresses))))

(define-rpc-handler (handle-write-memory "write-memory")
                    ((qvm-token #'parse-qvm-token)
                     (memory-contents #'parse-memory-contents))
  "Write to the classical memory of a persistent QVM.

DANGER! DANGER! DANGER! The effects of writing to classical memory while a persistent QVM is running are undefined. Although this restriction is not (currently) enforced, for safety you should only write to a QVM's memory when it is in the READY state or the WAITING state due to executing a WAIT instruction.

QVM-TOKEN is a valid persistent QVM token returned by the CREATE-QVM RPC call.

MEMORY-CONTENTS is a HASH-TABLE where each key is a string indicating the memory register name and the corresponding value is a LIST of (INDEX VALUE) pairs indicating that VALUE should be stored at index INDEX in the corresponding memory register.

Return a JSON-RESPONSE that contains T on success."
  (write-persistent-qvm-memory qvm-token memory-contents)
  (make-json-response t))


;;;;;;;;;;;;;;;;;;;;; PROGRAM EXECUTION HANDLERS ;;;;;;;;;;;;;;;;;;;;;

(define-rpc-handler (handle-run-program "run-program")
                    ((qvm-token (optionally #'parse-qvm-token))
                     (allocation-method (optionally #'parse-allocation-method))
                     (simulation-method (optionally #'parse-simulation-method))
                     (compiled-quil #'parse-quil-string)
                     (addresses #'parse-addresses)
                     (gate-noise (optionally #'parse-pauli-noise))
                     (measurement-noise (optionally #'parse-pauli-noise)))
  "Run the requested COMPILED-QUIL program, either on a persistent QVM or an emphemeral QVM using the given SIMULATION-METHOD.

QVM-TOKEN is a valid persistent QVM token returned by the CREATE-QVM RPC call.

SIMULATION-METHOD is a STRING naming the desired simulation method (see *AVAILABLE-SIMULATION-METHODS*).

COMPILED-QUIL is a STRING containing a valid Quil program.

ADDRESSES is a HASH-TABLE where the keys are mem register names and the values are either T to indicate that we should return all indices for the corresponding register, or else a LIST of the desired indices for that register.

The caller must provide either QVM-TOKEN or SIMULATION-METHOD, but not both.

Return a JSON-RESPONSE that contains a HASH-TABLE of the contents of the memory registers requested in the ADDRESSES request parameter."
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
  (make-json-response
   (if qvm-token
       (run-program-on-persistent-qvm qvm-token compiled-quil addresses)
       (run-program-on-qvm (make-requested-qvm simulation-method
                                               allocation-method
                                               (cl-quil:qubits-needed compiled-quil)
                                               gate-noise
                                               measurement-noise)
                           compiled-quil
                           addresses))))


;;;;;;;;;;;;;;;;;;;;;;;;; ASYNC JOB HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rpc-handler (handle-create-job "create-job") ((sub-request #'parse-sub-request))
  "Create the request async JOB.

SUB-REQUEST is a valid json object that indicates what RPC request should be run asynchronously. For example, to execute a \"run-program\" request asynchronously, you might pass the following as the SUB-REQUEST parameter:

    {
     \"type\": \"run-program\",
     \"simulation-method\": \"pure-state\",
     \"allocation-method\": \"native\",
     \"num-qubits\": 10,
     \"compiled-quil\": \"X 0\",
     \"addresses\": {\"ro\": true}
    }

It is an error to attempt to nest \"create-job\" requests by specifying \"create-job\" as the \"type\" field of SUB-REQUEST.

JOB-TOKEN is a valid JOB token returned by the CREATE-JOB RPC call.

Return a JSON-RESPONSE that contains a HASH-TABLE with a \"token\" key with the newly-created JOB's unique ID token."
  ;; TODO(appleby): should we whitelist rather than blacklist "asyncable" methods? Currently, we
  ;; allow creating a job for any RPC call other than create-job. Probably the only really useful
  ;; calls to run async are ones that perform computation (currently only run-program), although
  ;; there doesn't seem to be any harm in allowing someone to run qvm-info, say, asynchronously if
  ;; they want. Whitelisting would at least reduce the API surface for testing. Something to ponder.
  (make-json-response (alexandria:plist-hash-table
                       (list "token" (run-job (lambda ()
                                                (dispatch-rpc-request sub-request))))
                       :test #'equal)
                      :status +http-accepted+))

(define-rpc-handler (handle-job-info "job-info") ((job-token #'parse-job-token))
  "Return a JSON-RESPONSE with some basic bookkeeping info about the specified async JOB.

JOB-TOKEN is a valid JOB token returned by the CREATE-JOB RPC call."
  (make-json-response (job-info job-token)))

(define-rpc-handler (handle-delete-job "delete-job") ((job-token #'parse-job-token))
  "Delete a JOB.

JOB-TOKEN is a valid JOB token returned by the CREATE-JOB RPC call."
  (delete-job job-token)
  (make-json-response (format nil "Deleted async JOB ~A" job-token)))

(define-rpc-handler (handle-job-result "job-result") ((job-token #'parse-job-token))
  "Return a JSON-RESPONSE with result of the given async JOB.

This call will block waiting for the JOB to complete.

JOB-TOKEN is a valid JOB token returned by the CREATE-JOB RPC call."
  ;;  No need to MAKE-FOO-RESPONSE here; the JOB result will already be a RESPONSE object.
  (lookup-job-result job-token))
