;;;; handlers.lisp
;;;;
;;;; Author: Robert Smith, appleby

(in-package #:qvm-app-ng)

(defun handle-version ()
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (*standard-output*)
     (show-version))))

(defun get-random-state (arg)
  (etypecase arg
    (null (qvm:seeded-random-state nil))
    (unsigned-byte (qvm:seeded-random-state arg))))

(defun check-required-fields (hash-table &rest fields)
  (dolist (field fields t)
    (when (null (nth-value 1 (gethash field hash-table)))
      (error "Expected the field ~A to exist." field))))

(defun handle-post-request (request)
  (when (null tbnl:*session*)
    (tbnl:start-session))

  (let* ((api-key (tbnl:header-in* ':X-API-KEY request))
         (user-id (tbnl:header-in* ':X-USER-ID request))
         (data (hunchentoot:raw-post-data :request request :force-text t))
         (js (let* ((js (ignore-errors (yason:parse data))))
               (unless (and (hash-table-p js)
                            (check-required-fields js "type"))
                 (error "Invalid request."))
               js))
         (type (gethash "type" js)))
    (qvm:with-random-state ((get-random-state (gethash "rng-seed" js)))
      (format-log "Got ~S request from API key/User ID: ~S / ~S" type api-key user-id)
      (ecase (keywordify type)
        ((:version)
         (handle-version))))))
