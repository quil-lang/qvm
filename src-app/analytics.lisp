;;;; src-app/analytics.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;;; Note: In Forest 1.0, API keys no longer necessarily refer to a
;;; user. The new concept of a user ID (type: USER-ID) exists, but API
;;; keys (type: API-KEY) are kept for backwards
;;; compatibility. Unfortunately, they're both strings.

(deftype api-key ()
  "The type of an API key."
  'string)

(deftype user-id ()
  "The type of a user ID."
  'string)

(defun requests-made-key (api-key)
  "Get the Redis key for getting a user's (by API key) table corresponding to execution history."
  (check-type api-key api-key)
  (concatenate 'string "requests/" api-key))

(defun requests-made-by-id-key (user-id)
  "Get the Redis key for getting a user's (by user ID) table corresponding to execution history."
  (check-type user-id user-id)
  (concatenate 'string "requests-by-id/" user-id))

(defun current-time-field (num)
  "Get a field for the user's table corresponding to a request payload."
  (format nil "~A_~D" (get-universal-time) num))

(defun record-api-key (api-key)
  "Record the API key API-KEY that has made a request."
  (check-type api-key api-key)
  (red:SADD "api_keys" api-key))

(defun record-user-id (user-id)
  "Record the user ID USER-ID that has made a request."
  (check-type user-id user-id)
  (red:SADD "user_ids" user-id))

(defun record-request-payload (api-key request-payload &optional user-id)
  "Record that REQUEST-PAYLOAD was requested for API-KEY."
  (check-type api-key api-key)
  (check-type request-payload string)
  (check-type user-id (or null user-id))
  ;; Record API-KEY info.
  (let* ((req-key (requests-made-key api-key))
         (count   (red:HINCRBY req-key "count" 1)))
    (red:HSET req-key (current-time-field count) request-payload))
  ;; Record USER-ID info.
  (unless (null user-id)
    (let* ((req-key (requests-made-by-id-key user-id))
           (count (red:HINCRBY req-key "count" 1)))
      (red:HSET req-key (current-time-field count) request-payload))))
