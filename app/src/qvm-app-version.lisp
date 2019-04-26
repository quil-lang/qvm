;;;; qvm-app-version.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun system-version (system-designator)
    (let ((sys (asdf:find-system system-designator nil)))
      (if (and sys (slot-boundp sys 'asdf:version))
          (asdf:component-version sys)
          "unknown")))

  (defun git-hash (system)
    "Get the short git hash of the system SYSTEM."
    (let ((sys-path (namestring (asdf:system-source-directory system))))
      (multiple-value-bind (output err-output status)
          (uiop:run-program `("git" "-C" ,sys-path "rev-parse" "--short" "HEAD")
                            :output '(:string :stripped t)
                            :ignore-error-status t)
        (declare (ignore err-output))
        (if (not (zerop status))
            "unknown"
            output)))))

(eval-when (:compile-toplevel :load-toplevel)
  (alexandria:define-constant +QVM-VERSION+
      (system-version '#:qvm)
    :test #'string=
    :documentation "The version of the QVM itself.")

  (alexandria:define-constant +GIT-HASH+
      (git-hash '#:qvm-app)
    :test #'string=
    :documentation "The git hash of the QVM repo.")
  )

(defun latest-sdk-version ()
  "Get the latest QVM SDK version, or NIL if unavailable."
  (let* ((s (drakma:http-request "http://downloads.rigetti.com/qcs-sdk/version"
                                 :want-stream t))
         (p (yason:parse s)))
    (multiple-value-bind (version success)
        (gethash "qvm" p)
      (when success
        version))))

(defun sdk-update-available-p (current-version)
  "Test whether the current QVM version is the latest SDK
version. Second value returned indicates the latest version."
  (let ((latest (latest-sdk-version)))
    (values (not (string= latest current-version)) latest)))
