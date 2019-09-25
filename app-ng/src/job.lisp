(in-package #:qvm-app-ng)

(adt:defdata job-status
  job-status-fresh
  job-status-running
  job-status-interrupted
  (job-status-finished t)
  (job-status-error condition))

(defun job-status-name (job)
  (adt:match job-status (job-status job)
    (job-status-fresh "fresh")
    (job-status-running "running")
    (job-status-interrupted "interrupted")
    ((job-status-finished result) (format nil "finished with result: ~A" result))
    ((job-status-error error) (format nil "error: ~A" error))))

(defstruct (job (:constructor %make-job))
  "A job is an data-structure that permits a given function to be run in a threaded (and thread-safe) environment."
  lock
  work-function
  thread
  (status job-status-fresh :type job-status))

(defmacro with-job-lock ((job &key (wait t)) &body body)
  "Evaluate BODY with the LOCK acquired for the duration of BODY. If WAIT is T, block until the lock is available."
  `(when (bt:acquire-lock (job-lock ,job) ,wait)
     (unwind-protect
          (progn ,@body)
       (bt:release-lock (job-lock ,job)))))

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

(defun %job-interrupted-p-unsafe (job)
  (adt:match job-status (job-status job)
    (job-status-interrupted t)
    (_ nil)))

(defun job-interrupted-p (job)
  "Is the JOB in the INTERRUPTED state?"
  (with-job-lock (job)
    (%job-interrupted-p-unsafe job)))

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

The returned JOB can be repeatedly started if the JOB-STATUS is FINISHED (or INTERRUPTED). Only interact with the job after acquiring the lock (or alternatively by using WITH-JOB-LOCK)."
  (let* ((job (%make-job))
         (thr-fn (lambda ()
                   (setf (job-status job) job-status-fresh
                         (job-thread job) (bt:make-thread
                                           (lambda ()
                                             (handler-case
                                                 ;; Reset run-specific slots
                                                 (setf (job-status job) job-status-running
                                                       (job-status job) (job-status-finished (funcall fn)))
                                               (error (c)
                                                 (setf (job-status job) (job-status-error c))))))))))
    (setf (job-work-function job) thr-fn
          (job-lock job) (bt:make-lock))
    job))

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
    (funcall (job-work-function job))
    t))

(define-job-action job-stop (job)
  "Forcefully stop JOB if running. Does not block."
  (when (%job-running-p-unsafe job)
    (bt:destroy-thread (job-thread job))
    (setf (job-status job)
          job-status-interrupted)))

;;; sync/async

(define-job-action job-result (job)
  "Inspect the result of JOB. If JOB is in the running state, this will block."
  (when (%job-running-p-unsafe job)
    (bt:join-thread (job-thread job)))
  (adt:match job-status (job-status job)
    ((job-status-finished result) result)
    (_ (error "Shouldn't get here."))))

(define-job-action job-peek (job)
  "Inspect the result of JOB without blocking. Returns (VALUES RESULT FINISHED) where RESULT is the job result if the job is in the finished state (i.e. FINISHED is T); if FINISHED is NIL, then a RESULT of NIL does *not* mean that is the true result of the most recent run of JOB."
  (adt:match job-status (job-status job)
    ((job-status-finished result)
     (values result t))
    (_
     (values nil nil))))


