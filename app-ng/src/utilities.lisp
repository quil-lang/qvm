;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(defmacro with-timeout (&body body)
  (let ((f (gensym "TIME-LIMITED-BODY-")))
    `(flet ((,f () ,@body))
       (declare (dynamic-extent (function ,f)))
       (if (null *time-limit*)
           (,f)
           (bt:with-timeout (*time-limit*)
             (,f))))))

(defmacro with-timing ((var) &body body)
  (let ((start (gensym "START-")))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,var (round (* 1000 (- (get-internal-real-time) ,start))
                           internal-time-units-per-second))))))

(defun keywordify (str)
  (intern (string-upcase str) :keyword))

(defun resolve-safely (filename)
  (flet ((contains-up (filename)
           (member-if (lambda (obj)
                        (or (eql ':UP obj)
                            (eql ':BACK obj)))
                      (pathname-directory filename))))
    (cond
      ((uiop:absolute-pathname-p filename)
       (error "Not allowed to specify absolute paths to INCLUDE."))

      ((uiop:relative-pathname-p filename)
       ;; Only files allowed.
       (unless (uiop:file-pathname-p filename)
         (error "INCLUDE requires a pathname to a file."))
       (when (contains-up filename)
         (error "INCLUDE can't refer to files above."))
       (if (null *safe-include-directory*)
           filename
           (merge-pathnames filename *safe-include-directory*)))

      (t
       (error "Invalid pathname: ~S" filename)))))

(defun safely-parse-quil-string (string)
  "Safely parse a Quil string STRING."
  (flet ((parse-it (string)
           (let* ((quil::*allow-unresolved-applications* t)
                  (parse-results
                    (quil:parse-quil string)))
             parse-results)))
    (if (null *safe-include-directory*)
        (parse-it string)
        (let ((quil:*resolve-include-pathname* #'resolve-safely))
          (parse-it string)))))

(defun parse-simulation-method (simulation-method)
  (unless (member simulation-method *available-simulation-methods* :test #'string-equal)
    (error "Invalid simulation method: ~S" simulation-method))
  (intern (string-upcase simulation-method) :qvm-app-ng))

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
