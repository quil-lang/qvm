;;;; app-ng/src/jobbo.lisp
;;;;
;;;; author: appleby
;;;;
;;;; TODO(appleby): This file will be renamed or else merged with job.lisp once that PR lands. I've
;;;; stuck this code in a separate file for now to avoid merge conflicts.
(in-package #:qvm-app-ng)

(deftype job-token () 'string)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-empty-jobbos-db ()
    (safety-hash:make-safety-hash :test 'equal)))

(global-vars:define-global-var **jobs** (make-empty-jobbos-db)
  "The database of async JOBs. The keys are JOB-TOKENs and the values are JOBs.")

(defmacro with-locked-jobbo ((job) token &body body)
  "Execute BODY with JOB bound to the async JOB identified by TOKEN.

BODY is executed with the JOB's lock held. No other guarantees are made about the state of the JOB.

Signals an error if the lookup of TOKEN fails."
  (check-type job symbol)
  (alexandria:once-only (token)
    `(let ((,job (%lookup-jobbo-or-lose ,token)))
       (with-job-lock (,job)
         ,@body))))

(defun %lookup-jobbo-or-lose (token)
  (handler-case (safety-hash:gethash-or-lose token **jobs**)
    (error (c)
      (error "Failed to find jobbo ~A~%~A" token c))))

(defun reset-jobs-db ()
  "Reset the **JOBS** database."
  (safety-hash:clrhash **JOBS**))

(defun jobbo-info (token)
  "Return a HASH-TABLE of info about the state of the JOB corresponding to TOKEN."
  (alexandria:plist-hash-table
   (with-locked-jobbo (job) token
     ;; TODO(appleby): Unify the naming of JOB-STATUS vs PERSISTENT-QVM-STATE. The STRING-UPCASE
     ;; here is for symmetry with how persistent QVM state is reported. However, the job API calls
     ;; this field "status" vs "state" for persistent QVMs.
     (list "status" (string-upcase (job-status-name job))))
   :test 'equal))

(defun run-jobbo (job-thunk)
  "Create and start a new async JOB running JOB-THUNK.

Return (VALUES TOKEN JOB) where TOKEN is the ID of the new job."
  (let* ((token (make-uuid-string))
         (job (make-job job-thunk)))
    (job-start job)
    (safety-hash:insert-unique token job **jobs**)
    (values token job)))

(defun delete-jobbo (token)
  "Delete the JOB corresponding to TOKEN."
  (with-locked-jobbo (job) token
    (force-kill-job job))
  (safety-hash:remhash token **jobs**))

(defun jobbo-result (token)
  "Return the result of the JOB corresponding to TOKEN.

This call will block with the job lock held until the job finishes."
  (with-locked-jobbo (job) token
    (job-result job)))
