;;;; app-ng/src/job.lisp
;;;;
;;;; author: Mark Skilbeck
;;;;         appleby
(in-package #:qvm-app-ng)

;;;;;;;;;;;;;;;;;; Datatypes and Global Definitions ;;;;;;;;;;;;;;;;;;

(deftype job-token () 'string)

(adt:defdata job-status
  job-status-fresh
  job-status-running
  job-status-killed
  (job-status-finished t)
  (job-status-error condition))

(defstruct (job (:constructor %make-job (work-function)))
  "A job permits a given function to be run in a threaded (and thread-safe) environment."
  (lock (bt:make-lock) :type bt:lock :read-only t)
  ;; The job is ``performed'' by calling this function, where setting
  ;; of job status and handling of errors is accounted for. A lock
  ;; should be held by the caller when calling this function.
  (work-function nil :type function :read-only t)
  (thread nil :type (or null bt:thread))
  (status job-status-fresh :type job-status))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-empty-jobs-db ()
    (safety-hash:make-safety-hash :test 'equal)))

(global-vars:define-global-var **jobs** (make-empty-jobs-db)
  "The database of async JOBs. The keys are JOB-TOKENs and the values are JOBs.")

(defun reset-jobs-db ()
  "Reset the **JOBS** database."
  (safety-hash:clrhash **jobs**))


;;;;;;;;;;;;;;;;;;;;;;;;; Low-level Job API ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun job-status-name (job)
  (adt:match job-status (job-status job)
    (job-status-fresh "fresh")
    (job-status-running "running")
    (job-status-killed "killed")
    ((job-status-finished _) "finished")
    ((job-status-error _) "error")))

(define-condition job-error (error)
  ((job :initarg :job :reader job-error-job)
   (message :initarg :message :reader job-error-msg))
  (:report (lambda (c s)
             (write-string (job-error-msg c) s)))
  (:documentation "Error signaled because of job mismanagement."))

(defmacro with-job-lock ((job &key (wait t)) &body body)
  "Evaluate BODY with the LOCK acquired for the duration of BODY. If WAIT is T, block until the lock is available."
  (alexandria:once-only (job wait)
    `(when (bt:acquire-lock (job-lock ,job) ,wait)
       (unwind-protect
            (progn ,@body)
         (bt:release-lock (job-lock ,job))))))

(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :type nil :identity t)
    (format stream "QVM Job (~A)" (job-status-name job))))

;; The %x-unsafe variants below do not acquire a lock.

(defun %job-running-p-unsafe (job)
  (adt:match job-status (job-status job)
    (job-status-running t)
    (_ nil)))

(defun job-running-p (job)
  "Is the JOB in the RUNNING state."
  (with-job-lock (job)
    (%job-running-p-unsafe job)))

(defun %job-killed-p-unsafe (job)
  (adt:match job-status (job-status job)
    (job-status-killed t)
    (_ nil)))

(defun job-killed-p (job)
  "Is the JOB in the KILLED state?"
  (with-job-lock (job)
    (%job-killed-p-unsafe job)))

(defun %job-finished-p-unsafe (job)
  (adt:match job-status (job-status job)
    ((job-status-finished _) t)
    (_ nil)))

(defun job-finished-p (job)
  "Is the JOB in the FINISHED state?"
  (with-job-lock (job)
    (%job-finished-p-unsafe job)))

(defun make-job (fn)
  "Make a JOB object which will, under JOB-START, run FN in a thread.

The returned JOB can be repeatedly started if the JOB-STATUS is FRESH.

Note: Only interact with the job after acquiring the lock (or alternatively by using WITH-JOB-LOCK)."
  (%make-job (lambda (job)
               (adt:match job-status (job-status job)
                 (job-status-fresh
                  (setf (job-thread job)
                        (bt:make-thread
                         (lambda ()
                           (let ((result 
                                   (handler-case
                                       (job-status-finished (funcall fn))
                                     (error (c)
                                       (job-status-error c)))))
                             (with-job-lock (job)
                               (setf (job-status job) result))))))
                  (setf (job-status job) job-status-running))
                 (_ (error 'job-error :job job :message "Job cannot be started because it is not fresh."))))))

(defmacro define-job-action (action (job) &body body)
  "Define a function ACTION taking a single argument JOB, that evaluates BODY while holding JOB's lock."
  (multiple-value-bind (body decls docstr)
      (alexandria:parse-body body :documentation t)
    (declare (ignore decls))
    `(defun ,action (,job)
       ,docstr
       (with-job-lock (,job)
         ,@body))))

;;; CUSTODIAL

(define-job-action job-start (job)
  "Start the JOB and return T. If JOB is already running, do nothing and return NIL."
  (unless (%job-running-p-unsafe job)
    (funcall (job-work-function job) job)
    t))

(defun force-kill-job (job)
  "Forcefully kill JOB if running. Does not block and cannot be restarted."
  (when (%job-running-p-unsafe job)
    (bt:destroy-thread (job-thread job))
    (setf (job-status job)
          job-status-killed)))

(define-job-action kill-job (job)
  "Forcefully kill JOB if running as soon as lock is acquired. Job cannot be restarted."
  (force-kill-job job))

;;; sync/async

(defun job-result (job)
  "Inspect the result of JOB. If the job raised an error, the error will be signaled here. If JOB is in the running state, this will block."
  (when (%job-running-p-unsafe job)
    (bt:join-thread (job-thread job)))
  (adt:match job-status (job-status job)
    ((job-status-finished result) result)
    ((job-status-error c) (error c))
    (job-status-fresh (error 'job-error :job job :message "Job in fresh state, but expected a result."))
    (job-status-killed (error 'job-error :job job :message "Job in killed state, but expected a result."))
    (job-status-running (error 'job-error :job job :message "Inconsistent job state. Job should not be running at this point."))))

(defun job-peek (job)
  "Inspect the result of JOB without blocking. Returns (VALUES RESULT FINISHED) where RESULT is the job result if the job is in the finished state (i.e. FINISHED is T); if FINISHED is NIL, then a RESULT of NIL does *not* mean that is the true result of the most recent run of JOB."
  (adt:match job-status (job-status job)
    ((job-status-finished result)
     (values result t))
    (_
     (values nil nil))))


;;;;;;;;;;;;;;;;;;;;;;;;; High-level Job API ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-locked-job ((job) token &body body)
  "Execute BODY with JOB bound to the async JOB identified by TOKEN.

BODY is executed with the JOB's lock held. No other guarantees are made about the state of the JOB.

Signals an error if the lookup of TOKEN fails."
  (check-type job symbol)
  (alexandria:once-only (token)
    `(let ((,job (%lookup-job-or-lose ,token)))
       (with-job-lock (,job)
         ,@body))))

(defun %lookup-job-or-lose (token)
  (handler-case (safety-hash:gethash-or-lose token **jobs**)
    (error (c)
      (error "Failed to find job ~A~%~A" token c))))

(defun job-info (token)
  "Return a HASH-TABLE of info about the state of the JOB corresponding to TOKEN."
  (alexandria:plist-hash-table
   (with-locked-job (job) token
     ;; TODO(appleby): Unify the naming of JOB-STATUS vs PERSISTENT-QVM-STATE. The STRING-UPCASE
     ;; here is for symmetry with how persistent QVM state is reported. However, the job API calls
     ;; this field "status" vs "state" for persistent QVMs.
     (list "status" (string-upcase (job-status-name job))))
   :test 'equal))

(defun run-job (job-thunk)
  "Create and start a new async JOB running JOB-THUNK.

Return (VALUES TOKEN JOB) where TOKEN is the ID of the new job."
  (let* ((token (make-uuid-string))
         (job (make-job job-thunk)))
    (job-start job)
    (handler-case (safety-hash:insert-unique token job **jobs**)
      (error (c)
        ;; In the unlikely event of a token collision on insert, kill the job.
        (kill-job job)
        (error c)))
    (values token job)))

(defun delete-job (token)
  "Delete the JOB corresponding to TOKEN."
  (with-locked-job (job) token
    (force-kill-job job))
  (safety-hash:remhash token **jobs**))

(defun lookup-job-result (token)
  "Return the result of the JOB corresponding to TOKEN.

This call will block with the job lock held until the job finishes."
  (with-locked-job (job) token
    (job-result job)))
