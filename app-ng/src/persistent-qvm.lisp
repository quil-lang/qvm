;;;; api/persistent-qvm.lisp
;;;;
;;;; Author: appleby

(in-package :qvm-app-ng)

(deftype persistent-qvm-token () 'string)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-empty-persistent-qvms-db ()
    (make-hash-table :test 'equal)))

(global-vars:define-global-var **persistent-qvms** (make-empty-persistent-qvms-db)
  "The database of persistent QVMs. The keys are integers and the values are lists of (QVM LOCK METADATA) triples.")

(global-vars:define-global-var **persistent-qvms-lock** (bt:make-lock "Persistent QVMs DB Lock"))

(defun reset-persistent-qvms-db ()
  (bt:with-lock-held (**persistent-qvms-lock**)
    (setf **persistent-qvms** (make-empty-persistent-qvms-db))))

(defun persistent-qvms-count ()
  (bt:with-lock-held (**persistent-qvms-lock**)
    (hash-table-count **persistent-qvms**)))

(deftype persistent-qvm-status () '(member (ready dying)))

(defstruct (persistent-qvm (:constructor %make-persistent-qvm))
  qvm
  cv
  lock
  status
  metadata)

(defun %make-persistent-qvm-metadata (allocation-method)
  (alexandria:plist-hash-table (list "allocation-method" (symbol-name allocation-method)
                                     "created" (iso-time))
                               :test 'equal))

(defun make-persistent-qvm (qvm allocation-method)
  (let ((lock (bt:make-lock "PQVM Lock"))
        (cv (bt:make-condition-variable :name "PQVM CV")))
    (setf (slot-value qvm 'qvm::wait-function)
          (lambda (qvm)
            (declare (ignore qvm))
            (bt:condition-wait cv lock)))
    (%make-persistent-qvm :qvm qvm
                          :cv cv
                          :lock lock
                          :status 'ready
                          :metadata (%make-persistent-qvm-metadata allocation-method))))

(defmacro %with-locked-pqvm ((pqvm) token &body body)
  (check-type pqvm symbol)
  (alexandria:once-only (token)
    `(let ((,pqvm (%lookup-persistent-qvm-or-lose ,token)))
       (declare (ignorable ,pqvm))
       (bt:with-lock-held ((persistent-qvm-lock ,pqvm))
         ,@body))))

(defmacro with-persistent-qvm ((qvm &optional metadata cv) token &body body)
  (check-type qvm symbol)
  (check-type metadata (or null symbol))
  (check-type cv (or null symbol))
  (when (null metadata)
    (setf metadata (gensym "metadata")))
  (when (null cv)
    (setf cv (gensym "cv")))
  (alexandria:with-gensyms (pqvm)
    (alexandria:once-only (token)
      `(%with-locked-pqvm (,pqvm) ,token
         (with-slots ((,qvm qvm) (,cv cv) (,metadata metadata)) ,pqvm
           (declare (ignorable ,qvm ,cv ,metadata))
           (case (persistent-qvm-status ,pqvm)
             (dying (error "Persistent QVM ~A is marked for deletion." ,token))
             (t ,@body)))))))

(defun %insert-persistent-qvm-locked (token persistent-qvm)
  (setf (gethash token **persistent-qvms**) persistent-qvm))

(defun %remove-persistent-qvm-locked (token)
  (remhash token **persistent-qvms**))

(defun %remove-persistent-qvm (token)
  (bt:with-lock-held (**persistent-qvms-lock**)
    (remhash token **persistent-qvms**)))

(defun delete-persistent-qvm (token)
  (%with-locked-pqvm (pqvm) token
    (setf (persistent-qvm-status pqvm) 'dying))
  (%remove-persistent-qvm token))

(defun %lookup-persistent-qvm-locked (token)
  (gethash token **persistent-qvms**))

(defun %lookup-persistent-qvm (token)
  (bt:with-lock-held (**persistent-qvms-lock**)
    (%lookup-persistent-qvm-locked token)))

(defun %lookup-persistent-qvm-or-lose (token)
  (or (%lookup-persistent-qvm token)
      (error "Failed to find persistent QVM ~D" token)))

(defun canonicalize-persistent-qvm-token (token)
  "Canonicalize the TOKEN string into the case expected by VALID-PERSISTENT-QVM-TOKEN-P."
  ;; Standardize on the more common (and more readable) lowercase UUID string, even though
  ;; UUID:PRINT-OBJECT and UUID:PRINT-BYTES print them in uppercase.
  (string-downcase token))

(defun %uuid->persistent-qvm-token (uuid)
  (canonicalize-persistent-qvm-token (princ-to-string uuid)))

(defun make-persistent-qvm-token ()
  "Return a new persistent QVM token."
  (%uuid->persistent-qvm-token
   (bt:with-lock-held (**persistent-qvms-lock**)
     ;; UUID:MAKE-V4-UUID is not thread safe. If you call it without locking, you get collisions. We
     ;; reuse **PERSISTENT-QVMS-LOCK** here to avoid needing to acquire two separate locks in order
     ;; to allocate a new persistent QVM. We could potentially avoid locking by always creating a
     ;; thread-local binding for UUID:*UUID-RANDOM-STATE*, but since we only ever generate a new
     ;; token at allocation time when we already hold the **PERSISTENT-QVMS-LOCK**, it's convenient
     ;; to reuse it. In fact, at allocation time we call %MAKE-PERSISTENT-QVM-TOKEN-LOCKED. This
     ;; locking version is provided for external code (like tests) that want to safely generate a
     ;; valid persistent token without foisting the burden of thread-safe access on the caller.
     (uuid:make-v4-uuid))))

(defun %make-persistent-qvm-token-locked ()
  (%uuid->persistent-qvm-token (uuid:make-v4-uuid)))

(defun valid-persistent-qvm-token-p (token)
  "True if TOKEN is a valid string representation of a v4 UUID.

Note that this function requires that any hexadecimal digits in TOKEN are lowercased."
  ;; See RFC 4122 for UUID format.
  ;; https://tools.ietf.org/html/rfc4122#section-4.1
  ;;
  ;; We validate that token is a valid v4 UUID in printed string format. That is, as a string of
  ;; hexadecimal digits (with certain restrictions) separated by hyphens in the expected places.
  (and (typep token 'persistent-qvm-token)
       (= (length token) 36)
       (eql (aref token  8) #\-)
       (eql (aref token 13) #\-)
       (eql (aref token 14) #\4) ; version
       (eql (aref token 18) #\-)
       ;; https://tools.ietf.org/html/rfc4122#section-4.4
       ;; The two most-significant bits of the clock sequence field are #b10, meaning the
       ;; resulting hex digit of the most-significant byte is one of 8, 9, a, or b.
       (find (aref token 19) "89ab")
       (eql (aref token 23) #\-)
       (every (lambda (c)
                (find c "-0123456789abcdef"))
              token)))

(defun allocate-persistent-qvm (qvm allocation-method)
  (let ((persistent-qvm (make-persistent-qvm qvm allocation-method)))
    (bt:with-lock-held (**persistent-qvms-lock**)
      (let ((token (%make-persistent-qvm-token-locked)))
        (cond ((not (null (%lookup-persistent-qvm-locked token)))
               (error "Token collision while attempting to allocate persistent QVM: ~S" token))
              (t (%insert-persistent-qvm-locked token persistent-qvm)
                 (values token persistent-qvm)))))))

(defun persistent-qvm-info (token)
  (alexandria:plist-hash-table
   (%with-locked-pqvm (pqvm) token
     (list "qvm-type" (symbol-name (type-of (persistent-qvm-qvm pqvm)))
           "num-qubits" (qvm:number-of-qubits (persistent-qvm-qvm pqvm))
           "status" (symbol-name (persistent-qvm-status pqvm))
           "metadata" (persistent-qvm-metadata pqvm)))
   :test 'equal))

(defun run-program-on-persistent-qvm (token parsed-program)
  (with-persistent-qvm (qvm) token
    (run-program-on-qvm qvm parsed-program)))

(defun write-persistent-qvm-memory (token memory-contents)
  (with-persistent-qvm (qvm) token
    (maphash (lambda (k v)
               (loop :for (index value) :in v :do
                 (setf (qvm:memory-ref qvm k index) value)))
             memory-contents))
  nil)
