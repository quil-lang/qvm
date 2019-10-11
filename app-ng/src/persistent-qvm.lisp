;;;; persistent-qvm.lisp
;;;;
;;;; Author: appleby

(in-package :qvm-app-ng)

(deftype persistent-qvm-token () 'string)

(deftype persistent-qvm-state () '(member ready running waiting resuming dying))

(defstruct (persistent-qvm (:constructor %make-persistent-qvm))
  (qvm      (error "Must provide QVM")                                 :read-only t)
  (cv       (error "Must provide CV")                                  :read-only t)
  (lock     (error "Must provide LOCK")                                :read-only t)
  (state    (error "Must provide STATE")    :type persistent-qvm-state)
  (token    (error "Must provide TOKEN")    :type persistent-qvm-token :read-only t)
  (metadata (error "Must provide METADATA") :type hash-table           :read-only t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-empty-persistent-qvms-db ()
    (make-hash-table :test 'equal)))

(global-vars:define-global-var **persistent-qvms** (make-empty-persistent-qvms-db)
  "The database of persistent QVMs. The keys are integers and the values are lists of (QVM LOCK METADATA) triples.")

(global-vars:define-global-var **persistent-qvms-lock** (bt:make-lock "Persistent QVMs DB Lock"))

(defun reset-persistent-qvms-db ()
  "Reset the **PERSISTENT-QVMS** database."
  (bt:with-lock-held (**persistent-qvms-lock**)
    (setf **persistent-qvms** (make-empty-persistent-qvms-db))))

(defun persistent-qvms-count ()
  "Return the number of PERSISTENT-QVMS currently allocated."
  (bt:with-lock-held (**persistent-qvms-lock**)
    (hash-table-count **persistent-qvms**)))

(alexandria:define-constant +valid-pqvm-state-transitions+
    '((ready    running           dying)
      (running  ready    waiting  dying)
      (waiting  resuming          dying)
      (resuming running           dying)
      (dying                      dying))
  :test #'equal
  :documentation "An alist of valid state transitions for a PERSISTENT-QVM. The alist is keyed on the current state. The value for each key is list of states that can be transitioned to from the corresponding current state.")

(defun persistent-qvm-state= (state-a state-b)
  "Is the PERSISTENT-QVM-STATE STATE-A equal to STATE-B?"
  (check-type state-a persistent-qvm-state)
  (check-type state-b persistent-qvm-state)
  (eq state-a state-b))

(defun valid-pqvm-state-transition-p (current-state new-state)
  "Is the state transition CURRENT-STATE -> NEW-STATE valid according to +VALID-PQVM-STATE-TRANSITIONS+?"
  (check-type current-state persistent-qvm-state)
  (check-type new-state persistent-qvm-state)
  (member new-state (cdr (assoc current-state +valid-pqvm-state-transitions+))))

(defun %checked-transition-to-state-locked (pqvm new-state &key from-state)
  (check-type pqvm persistent-qvm)
  (check-type new-state persistent-qvm-state)
  (check-type from-state (or null persistent-qvm-state))
  (with-slots ((current-state state)) pqvm
    (let ((current-state-valid-p (or (null from-state)
                                     (persistent-qvm-state= from-state current-state))))
      (cond ((and current-state-valid-p (valid-pqvm-state-transition-p current-state new-state))
             (setf current-state new-state))
            (t (error "Attempting invalid state transition ~A -> ~A~
                      ~@[~&Invalid starting state: wanted ~A~]~
                      ~&Persistent QVM is ~A"
                      current-state
                      new-state
                      (and (not current-state-valid-p) from-state)
                      (persistent-qvm-token pqvm)))))))

(defun make-persistent-qvm-metadata (allocation-method)
  "Make a hash-table suitable a new PERSISTENT-QVM's METADATA slot."
  (alexandria:plist-hash-table (list "allocation-method" (symbol-name allocation-method)
                                     "created" (iso-time))
                               :test 'equal))

(defun make-persistent-qvm (qvm allocation-method token)
  "Make a PERSISTENT-QVM.

This function only returns a new PERSISTENT-QVM object. External callers probably want ALLOCATE-PERSISTENT-QVM, instead.

QVM is any QVM type (PURE-STATE-QVM, NOISY-QVM, etc).

ALLOCATION-METHOD is one of +AVAILABLE-ALLOCATION-METHODS+. This only provided so that it can be recorded in the PERSISTENT-QVM's metadata.

TOKEN is a PERSISTENT-QVM-TOKEN."
  (let* ((lock (bt:make-lock "PQVM Lock"))
         (cv (bt:make-condition-variable :name "PQVM CV"))
         (pqvm (%make-persistent-qvm :qvm qvm
                                     :cv cv
                                     :lock lock
                                     :state 'ready
                                     :token token
                                     :metadata (make-persistent-qvm-metadata allocation-method))))
    (setf (slot-value qvm 'qvm::wait-function)
          (lambda (qvm)
            (declare (ignore qvm))
            ;; LOCK must be held here or we're in trouble.
            (%checked-transition-to-state-locked pqvm 'waiting :from-state 'running)
            ;; TODO:(appleby) possible to unwind from CONDITION-WAIT? Maybe UNWIND-PROTECT here.
            (loop :while (persistent-qvm-state= 'waiting (persistent-qvm-state pqvm))
                  :do (bt:condition-wait cv lock)
                  :finally (unless (persistent-qvm-state= 'dying (persistent-qvm-state pqvm))
                             (%checked-transition-to-state-locked pqvm
                                                                  'running
                                                                  :from-state 'resuming)))))
    pqvm))

(defmacro with-locked-pqvm ((pqvm) token &body body)
  "Execute BODY with PQVM bound to the persistent QVM identified by TOKEN.

BODY is executed with the persistent QVM's lock held. No other guarantees are made about the state of the persistent QVM. It's up to the caller to check that the persistent QVM's state is consistent inside BODY. Specifically, the persistent QVM may already be marked for deletion or in the WAITING state, etc. For this reason, callers should prefer the higher-level interface WITH-PERSISTENT-QVM when they only need safe access to the persistent QVM's underlying QVM object."
  (check-type pqvm symbol)
  (alexandria:once-only (token)
    `(let ((,pqvm (%lookup-persistent-qvm-or-lose ,token)))
       (declare (ignorable ,pqvm))
       (bt:with-lock-held ((persistent-qvm-lock ,pqvm))
         ,@body))))

(defmacro with-persistent-qvm ((qvm) token &body body)
  "Execute BODY with QVM bound to the QVM object owned by the persistent QVM corresponding to TOKEN.

BODY is executed with the persistent QVM's lock held, and an error is signaled if the persistent QVM is dying."
  (check-type qvm symbol)
  (alexandria:with-gensyms (pqvm)
    (alexandria:once-only (token)
      `(with-locked-pqvm (,pqvm) ,token
         (with-slots ((,qvm qvm)) ,pqvm
           (declare (ignorable ,qvm))
           (case (persistent-qvm-state ,pqvm)
             (dying (error "Persistent QVM ~A is marked for deletion." ,token))
             (t ,@body)))))))

(defun %insert-persistent-qvm-locked (token persistent-qvm)
  (setf (gethash token **persistent-qvms**) persistent-qvm))

(defun delete-persistent-qvm (token)
  "Delete the PERSISTENT-QVM indicated by TOKEN."
  (with-locked-pqvm (pqvm) token
    (%checked-transition-to-state-locked pqvm 'dying))
  (bt:with-lock-held (**persistent-qvms-lock**)
    (remhash token **persistent-qvms**)))

(defun %lookup-persistent-qvm-locked (token)
  (gethash token **persistent-qvms**))

(defun %lookup-persistent-qvm (token)
  (bt:with-lock-held (**persistent-qvms-lock**)
    (%lookup-persistent-qvm-locked token)))

(defun %lookup-persistent-qvm-or-lose (token)
  (or (%lookup-persistent-qvm token)
      (error "Failed to find persistent QVM ~A" token)))

(defun canonicalize-persistent-qvm-token (token)
  "Canonicalize the TOKEN string into the case expected by VALID-PERSISTENT-QVM-TOKEN-P."
  (canonicalize-uuid-string token))

(defun make-persistent-qvm-token ()
  "Return a new persistent QVM token."
  (make-uuid-string))

(defun valid-persistent-qvm-token-p (token)
  "True if TOKEN is a valid string representation of a v4 UUID.

Note that this function requires that any hexadecimal digits in TOKEN are lowercased."
  (valid-uuid-string-p token))

(defun allocate-persistent-qvm (qvm allocation-method)
  "Allocate a new PERSISTENT-QVM.

QVM is a QVM object of any type (PURE-STATE-QVM, NOISY-QVM, etc.)

ALLOCATION-METHOD should be one of the +AVAILABLE-ALLOCATION-METHODS+, and is used when creating the PERSISTENT-QVM's metadata."
  (bt:with-lock-held (**persistent-qvms-lock**)
    (let ((token (make-persistent-qvm-token)))
      (cond ((not (null (%lookup-persistent-qvm-locked token)))
             (error "Token collision while attempting to allocate persistent QVM: ~S" token))
            (t (let ((persistent-qvm (make-persistent-qvm qvm allocation-method token)))
                 (%insert-persistent-qvm-locked token persistent-qvm)
                 (values token persistent-qvm)))))))

(defun persistent-qvm-info (token)
  "Return a HASH-TABLE of information about the PERSISTENT-QVM identified by TOKEN.

TOKEN is a PERSISTENT-QVM-TOKEN."
  (alexandria:plist-hash-table
   (with-locked-pqvm (pqvm) token
     (list "qvm-type" (symbol-name (type-of (persistent-qvm-qvm pqvm)))
           "num-qubits" (qvm:number-of-qubits (persistent-qvm-qvm pqvm))
           "state" (symbol-name (persistent-qvm-state pqvm))
           "metadata" (persistent-qvm-metadata pqvm)))
   :test 'equal))

(defun run-program-on-persistent-qvm (token parsed-program &optional addresses)
  "Run the given PARSED-PROGRAM on the PERSISTENT-QVM indicated by TOKEN.

The optional ADDRESSES are passed along to RUN-PROGRAM-ON-QVM and specify which memory register contents you want back.

Returns the requested memory registers or signals an error if the given PERSISTENT-QVM is not in the READY state."
  (with-locked-pqvm (pqvm) token
    (case (persistent-qvm-state pqvm)
      (ready
       (%checked-transition-to-state-locked pqvm 'running :from-state 'ready)
       (unwind-protect (run-program-on-qvm (persistent-qvm-qvm pqvm) parsed-program addresses)
         (%checked-transition-to-state-locked pqvm 'ready :from-state 'running)))
      (t
       (error "Cannot run program on Persistent QVM ~A in state ~A."
              token
              (persistent-qvm-state pqvm))))))

(defun write-persistent-qvm-memory (token memory-contents)
  "Write MEMORY-CONTENTS in the classical memory of the given PERSISTENT-QVM.

TOKEN is a PERSISTENT-QVM-TOKEN.

MEMORY-CONTENTS is a HASH-TABLE where each key is a string indicating the memory register name and the corresponding value is a LIST of (INDEX VALUE) pairs indicating that VALUE should be stored at index INDEX in the corresponding memory register.

Returns NIL."
  (with-persistent-qvm (qvm) token
    (maphash (lambda (region-name index-values)
               (mapc (lambda (index-and-value)
                       (destructuring-bind (index value) index-and-value
                         (setf (qvm:memory-ref qvm region-name index) value)))
                     index-values))
             memory-contents)))

(defun resume-persistent-qvm (token)
  "Resume the PERSISTENT-QVM indicated by TOKEN.

TOKEN is a PERSISTENT-QVM-TOKEN.

Returns NIL or signals an error if the given PERSISTENT-QVM is not in the WAITING state."
  (with-locked-pqvm (pqvm) token
    (%checked-transition-to-state-locked pqvm 'resuming :from-state 'waiting)
    (bt:condition-notify (persistent-qvm-cv pqvm)))
  nil)
