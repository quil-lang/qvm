(in-package #:qvm-app-ng)

(adt:defdata job-status
  job-status-fresh
  job-status-running
  job-status-killed
  (job-status-finished t)
  (job-status-error condition))

(defstruct (job (:constructor %make-job (work-function)))
  "A job permits a given function to be run in a threaded (and thread-safe) environment."
  (lock (bt:make-lock) :type bt:lock :read-only t)
  (work-function nil :type function :read-only t
                     :documentation "The job is ``performed'' by calling this function, where setting of job status and handling of errors is accounted for. A lock should be held by the caller when calling this function.")
  (thread nil :type (or null bt:thread))
  (status job-status-fresh :type job-status))

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
  "Evaluate BODY with a lock held for JOB."
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

