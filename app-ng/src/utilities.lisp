;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defun keywordify (str)
  (intern (string-upcase str) :keyword))

(global-vars:define-global-var **log-lock** (bt:make-lock "Log Lock"))
(defmacro with-locked-log (() &body body)
  `(bt:with-lock-held (**log-lock**)
     ,@body))

(defmacro format-log (level-or-fmt-string &rest fmt-string-or-args)
  "Send a message to syslog. If the first argument LEVEL-OR-FMT-STRING is a
keyword it is assumed to be a non-default log level (:debug), otherwise it is a control
string followed by optional args (as in FORMAT)."
  (when (keywordp level-or-fmt-string)
    ;; Sanity check that it's a valid log level at macroexpansion
    ;; time.
    (cl-syslog:get-priority level-or-fmt-string))
  (if (keywordp level-or-fmt-string)
      `(with-locked-log ()
         (cl-syslog:format-log
          *logger*
          ',level-or-fmt-string
          "~A~@?"
          (session-info)
          ,@fmt-string-or-args))
      `(with-locked-log ()
         (cl-syslog:format-log
          *logger*
          ':debug
          "~A~@?"
          (session-info)
          ,level-or-fmt-string
          ,@fmt-string-or-args))))
