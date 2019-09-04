(in-package #:qvm)

(defparameter **jobs** (make-hash-table :test 'equal)
  "Table mapping a job ID to a job instance.")
(deftype job-status ()
  '(member fresh running finished interrupted))

(defstruct (job (:constructor %make-job))
  id
  threader
  thread
  %result                               ; I define this accessor below.
  (status 'fresh :type job-status))

(defun job-running-p (job)
  (eq (job-status job) 'running))
(defun job-interrupted-p (job)
  (eq (job-status job) 'interrupted))
(defun job-finished-p (job)
  (eq (job-status job) 'finished))

(defmethod print-object ((job job) stream)
  (format stream "job id ~A" (job-id job)))

(defun make-job (fn)
  "Make a JOB object which will, under JOB-START, run FN in a thread."
  (let* ((job (%make-job :id (princ-to-string (uuid:make-v4-uuid))))
         (thr-fn (lambda ()
                   (setf (job-thread job)
                         (bt:make-thread (lambda ()
                                           (setf (job-status job) 'running)
                                           (setf (job-%result job)
                                                 (funcall fn))
                                           (setf (job-status job) 'finished)))))))
    (setf (job-threader job) thr-fn)
    (setf (gethash (job-id job) **jobs**) job)
    job))

;;; CUSTODIAL

(defun job-start (job)
  "Start the JOB."
  (funcall (job-threader job)))

(defun job-stop (job)
  "Forcefully stop JOB if running. Does not block."
  (when (job-running-p job)
    (bt:destroy-thread (job-thread job))
    (setf (job-status job) 'interrupted)))

(defun clean-finished-jobs ()
  (loop :for job-id :being :the :hash-keys :of **jobs**
        :when (member (job-status (gethash job-id **jobs**))
                      '(finished interrupted)) :do
          (remhash job-id **jobs**)))

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

