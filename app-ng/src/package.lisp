;;;; app-ng/src/package.lisp
;;;;
;;;; Author: Robert Smith

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
   #:job-stop
   #:job-result
   #:job-peek))
