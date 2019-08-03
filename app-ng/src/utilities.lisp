;;;; utilities.lisp
;;;;
;;;; Author: appleby

(in-package #:qvm-app-ng)

(defun session-info ()
  ;; Stub implementation for FORMAT-LOG, below. See app/src/utilities.lisp for the original.
  "")

(defmacro format-log (level-or-fmt-string &rest fmt-string-or-args)
  "Send a message to syslog. If the first argument LEVEL-OR-FMT-STRING is a
keyword it is assumed to be a non-default log level (:debug), otherwise it is a control
string followed by optional args (as in FORMAT)."
  (when (keywordp level-or-fmt-string)
    ;; Sanity check that it's a valid log level at macroexpansion
    ;; time.
    (cl-syslog:get-priority level-or-fmt-string))
  (if (keywordp level-or-fmt-string)
      `(cl-syslog:format-log
        *logger*
        ',level-or-fmt-string
        "~A~@?"
        (session-info)
        ,@fmt-string-or-args)
      `(cl-syslog:format-log
        *logger*
        ':debug
        "~A~@?"
        (session-info)
        ,level-or-fmt-string
        ,@fmt-string-or-args)))
