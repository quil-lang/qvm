(in-package #:qvm-app)

(deftype job-status ()
  '(member fresh running finished interrupted error))

(defstruct (job (:constructor %make-job))
  threader
  thread
  %result                              ; I define this accessor below.
  (status 'fraiche :type job-status)
  error)

(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :type nil :identity t)
    (format stream "QVM Job")))

(defun job-running-p (job)
  (eq (job-status job) 'running))
(defun job-interrupted-p (job)
  (eq (job-status job) 'interrupted))
(defun job-finished-p (job)
  (eq (job-status job) 'finished))

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
    job))

;;; CUSTODIAL

(defun job-start (job)
  "Start the JOB."
  ;; TODO Should this raise an error/warning if the job is already
  ;; running?
  (unless (job-running-p job)
    (funcall (job-threader job))))

(defun job-stop (job &key (status 'interrupted))
  (check-type status job-status)
  "Forcefully stop JOB if running. Does not block."
  (when (job-running-p job)
    (bt:destroy-thread (job-thread job))
    (setf (job-status job) status)))

;;; sync/async

(defun job-result (job)
  "Inspect the result of JOB. If JOB is in the running state, this will block."
  (when (job-running-p job)
    (bt:join-thread (job-thread job)))
  (job-%result job))

(defun job-peek (job)
  "Inspect the result of JOB without blocking. Returns (VALUES RESULT FINISHED) where RESULT is the job result if the job is in the finished state (i.e. FINISHED is T); if FINISHED is NIL, then a RESULT of NIL does *not* mean that is the true result of the most recent run of JOB."
  (values (job-%result job)
          (job-finished-p job)))


