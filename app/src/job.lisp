(in-package #:qvm-app)

(deftype job-status ()
  '(member fraiche running finished interrupted error))

(defstruct (job (:constructor %make-job))
  lock
  threader
  thread
  %result                              ; I define this accessor below.
  (status 'fraiche :type job-status)
  error)

(defmacro with-job-lock ((job &key (wait t)) &body body)
  `(progn
     (bt:acquire-lock (job-lock ,job) ,wait)
     ,@body
     (bt:release-lock (job-lock ,job))))

(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :type nil :identity t)
    (format stream "QVM Job")))

(defun job-running-p (job)
  (with-job-lock (job)
    (eq (job-status job) 'running)))

(defun job-interrupted-p (job)
  (with-job-lock (job)
    (eq (job-status job) 'interrupted)))

(defun job-finished-p (job)
  (with-job-lock (job)
    (eq (job-status job) 'finished)))

(defun make-job (fn)
  "Make a JOB object which will, under JOB-START, run FN in a thread."
  (let* ((job (%make-job))
         (thr-fn (lambda ()
                   (setf (job-thread job)
                         (bt:make-thread
                          (lambda ()
                            (handler-case
                                (progn
                                  (setf (job-status job) 'running)
                                  (setf (job-%result job) (funcall fn))
                                  (setf (job-status job) 'finished))
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
  "Start the JOB. If JOB is already running, do nothing."
  (with-job-lock (job)
    (unless (job-running-p job)
      (funcall (job-threader job)))))

(defun job-stop (job &key (status 'interrupted))
  "Forcefully stop JOB if running. Does not block."
  (check-type status job-status)
  ;; TODO Should this acquire the lock?
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


