;;;; app-ng/src/response.lisp
;;;;
;;;; author: appleby
;;;;
;;;; This file contains functions and data types for working with RPC responses.
(in-package #:qvm-app-ng)

(defstruct (response (:constructor nil))
  "The type of a an RPC handler response.

A RESPONSE cannot be constructed directly. Use one of the subtypes, JSON-RESPONSE or TEXT-RESPONSE.

Slots:
    DATA - The response data.

    STATUS - an HTTP-STATUS for the response.

    ENCODER - a FUNCTION of two arguments, the RESPONSE-DATA to encode and an optional stream on which to encode it."
  (data nil :read-only t)
  (status +http-ok+ :read-only t :type http-status)
  ;; Encoding could also be handled via generic function dispatch on the response type, but then you
  ;; need one subtype per desired encoding scheme. We allow the caller to specify the encoder
  ;; function as convenience, since there may be more that one way to map a lisp object to an
  ;; encoded representation. For example, a caller can pass :ENCODER #'YASON:ENCODE-PLIST, to avoid
  ;; having to first convert the plist to a HASH-TABLE for a JSON-RESPONSE. Likewise, for a
  ;; TEXT-RESPONSE, one could pass #'WRITE-LINE to automatically get a newline appended, etc.
  (encoder (error "RPC-RESPONSE :ENCODER is required") :read-only t :type function))

(defstruct (json-response (:constructor %make-json-response)
                          (:include response (encoder #'yason:encode)))
  "The type of JSON-RESPONSE.

See the documentation of RESPONSE for documentation on inherited slots.

Inherited slot option overrides:
    ENCODER - defaults to YASON:ENCODE.")

(defstruct (text-response (:constructor %make-text-response)
                          (:include response
                           (encoder #'write-string)
                           (data "" :type string)))
  "The type of plain TEXT-RESPONSE.

See the documentation of RESPONSE for documentation on inherited slots.

Inherited slot option overrides:
    ENCODER - defaults to WRITE-STRING.
    DATA - must be of type STRING.")

(defun make-json-response (data &rest args &key status encoder)
  "Make a JSON-RESPONSE."
  (declare (ignore status encoder))
  (apply #'%make-json-response :data data args))

(defun make-text-response (data &rest args &key status encoder)
  "Make a TEXT-RESPONSE."
  (declare (ignore status encoder))
  (apply #'%make-text-response :data data args))

(defun encode-response (response &optional (stream *standard-output*))
  "Encode the RESPONSE on STREAM.

This will call RESPONSE-ENCODER on the RESPONSE-DATA and STREAM."
  (funcall (response-encoder response) (response-data response) stream))

(defgeneric response-content-type (response)
  (:documentation "Return an appropriate http content-type for RESPONSE.")
  (:method ((response json-response))
    "application/json; charset=utf-8")
  (:method ((response text-response))
    "text/plain; charset=utf-8"))
