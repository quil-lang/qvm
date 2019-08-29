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

(define-rpc-handler (handle-create-qvm "create-qvm") ((simulation-method #'parse-simulation-method)
                                                      (num-qubits #'parse-num-qubits))
  "Create the requested persistent QVM.

SIMULATION-METHOD is a STRING naming the desired simulation method (see *AVAILABLE-SIMULATION-METHODS*).

NUM-QUBITS is a non-negative integer and represents the maximum number of qubits available on the QVM."
  (encode-json (alexandria:plist-hash-table
                (list "token" (allocate-persistent-qvm simulation-method num-qubits)))))

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
                     (simulation-method #'parse-optional-simulation-method)
                     (compiled-quil #'parse-quil-string)
                     (addresses #'parse-addresses))
  "Run the requested COMPILED-QUIL program, either on a persistent QVM or an emphemeral QVM using the given SIMULATION-METHOD.

QVM-TOKEN is a valid persistent QVM token as returned by the CREATE-QVM RPC call.

SIMULATION-METHOD is a STRING naming the desired simulation method (see *AVAILABLE-SIMULATION-METHODS*).

COMPILED-QUIL is a STRING containing a valid Quil program.

ADDRESSES is a HASH-TABLE where the keys are mem register names and the values are either T to indicate that we should return all indices for the corresponding register, or else a LIST of the desired indices for that register.

The caller must provide either QVM-TOKEN or SIMULATION-METHOD, but not both."
  (unless (alexandria:xor (null qvm-token) (null simulation-method))
    (rpc-parameter-parse-error
     "Exactly one of QVM-TOKEN and SIMULATION-METHOD must be present in the JSON request parameters."))
  (encode-json
   (collect-memory-registers
    (if qvm-token
        (run-program-on-persistent-qvm qvm-token compiled-quil)
        (run-program-on-qvm (make-requested-qvm simulation-method (quil:qubits-needed compiled-quil))
                            compiled-quil))
    addresses)))
