;;;; app-ng/src/package.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Mark Skilbeck
;;;;         appleby

(defpackage #:qvm-app-ng
  (:use #:cl #:qvm)

  ;; job.lisp
  (:export
   #:job-status
   #:job-status-fresh
   #:job-status-running
   #:job-status-interrupted
   #:job-status-finished
   #:job-status-error
   #:job-status-name
   #:make-job
   #:job-running-p
   #:job-interrupted-p
   #:job-finished-p
   #:job-start
   #:kill-job
   #:force-kill-job
   #:job-result
   #:job-peek))
