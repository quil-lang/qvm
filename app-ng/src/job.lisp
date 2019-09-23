(in-package #:qvm-app-ng)

(deftype job-status ()
  '(member fraiche running finished interrupted error))

(defstruct (job (:constructor %make-job))
  "A job is an data-structure that permits a given function to be run in a threaded (and thread-safe) environment."
  lock
  threader
  thread
  %result                              ; I define this accessor below.
  (status 'fraiche :type job-status)
  error)

(defmacro with-job-lock ((job &key (wait t)) &body body)
  "Evaluate BODY with the LOCK acquired for the duration of BODY. If WAIT is T, block until the lock is available."
  `(progn
     (when (bt:acquire-lock (job-lock ,job) ,wait)
       (unwind-protect
            (progn ,@body)
         (bt:release-lock (job-lock ,job))))))

(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :type nil :identity t)
    (format stream "QVM Job (status ~A)" (job-status job))))

(defun job-running-p (job)
  "Is the JOB in the RUNNING state."
  (with-job-lock (job)
    (eq (job-status job) 'running)))

(defun job-interrupted-p (job)
  "Is the JOB in the INTERRUPTED state?"
  (with-job-lock (job)
    (eq (job-status job) 'interrupted)))

(defun job-finished-p (job)
  "Is the JOB in the FINISHED state?"
  (with-job-lock (job)
    (eq (job-status job) 'finished)))

(defun make-job (fn)
  "Make a JOB object which will, under JOB-START, run FN in a thread.

The returned JOB can be repeatedly started if the JOB-STATUS is FINISHED (or INTERRUPTED). Only interact with the job after acquiring the lock (or alternatively by using WITH-JOB-LOCK)."
  (let* ((job (%make-job))
         (thr-fn (lambda ()
                   (setf (job-thread job)
                         (bt:make-thread
                          (lambda ()
                            (handler-case
                                (progn
                                  ;; Reset run-specific slots
                                  (setf (job-%result job) nil)
                                  (setf (job-error   job) nil)
                                  (setf (job-status  job) 'running)
                                  (setf (job-%result job) (funcall fn))
                                  (setf (job-status  job) 'finished))
                              (error (c)
                                ;; TODO What happens to a thread when
                                ;; there is an error in it?
                                (setf (job-error job) c)
                                (setf (job-status job) 'error)))))))))
    (setf (job-threader job) thr-fn)
    (setf (job-lock job) (bt:make-lock))
    job))

;;; CUSTODIAL

(defun job-start (job)
  "Start the JOB and return T. If JOB is already running, do nothing and return NIL."
  (with-job-lock (job)
    (unless (job-running-p job)
      (funcall (job-threader job))
      t)))

(defun job-stop (job &key (status 'interrupted))
  "Forcefully stop JOB if running. Does not block. Set the JOB-STATUS into STATUS."
  (check-type status job-status)
  (with-job-lock (job)
    (when (job-running-p job)
      (bt:destroy-thread (job-thread job))
      (setf (job-status job) status))))

;;; sync/async

(defun job-result (job)
  "Inspect the result of JOB. If JOB is in the running state, this will block."
  (with-job-lock (job)
    (when (job-running-p job)
      (bt:join-thread (job-thread job)))
    (job-%result job)))

(defun job-peek (job)
  "Inspect the result of JOB without blocking. Returns (VALUES RESULT FINISHED) where RESULT is the job result if the job is in the finished state (i.e. FINISHED is T); if FINISHED is NIL, then a RESULT of NIL does *not* mean that is the true result of the most recent run of JOB."
  (with-job-lock (job)
    (values (job-%result job)
            (job-finished-p job))))


