;;;; src-app/analytics.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(deftype api-key ()
  "The type of an API key."
  'string)

(defun requests-made-key (api-key)
  "Get the Redis key for getting a user's table corresponding to execution history."
  (check-type api-key api-key)
  (concatenate 'string "requests/" api-key))

(defun current-time-field (num)
  "Get a field for the user's table corresponding to a request payload."
  (format nil "~A_~D" (get-universal-time) num))

(defun record-api-key (api-key)
  "Record the API key API-KEY that has made a request."
  (check-type api-key api-key)
  (red:SADD "api_keys" api-key))

(defun record-request-payload (api-key request-payload)
  "Record that REQUEST-PAYLOAD was requested for API-KEY."
  (check-type api-key api-key)
  (check-type request-payload string)
  (let* ((req-key (requests-made-key api-key))
         (count   (red:HINCRBY req-key "count" 1)))
    (red:HSET req-key (current-time-field count) request-payload)))
