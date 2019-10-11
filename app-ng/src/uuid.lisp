;;;; uuid.lisp
;;;;
;;;; Author: appleby
(in-package #:qvm-app-ng)

;;; This file contains common utilities for working with v4 UUIDs. UUIDs are used as unique
;;; identifiers in various places, such as PERSISTENT-QVM and asynchronous JOB tokens. The utilities
;;; here help ensure consistent UUID representations (CANONICALIZE-UUID-STRING,
;;; VALID-UUID-STRING-P), plus thread-safe UUID generation (MAKE-UUID, MAKE-UUID-STRING).

(global-vars:define-global-var **make-uuid-lock** (bt:make-lock "UUID Lock"))

(defun canonicalize-uuid-string (uuid-string)
  "Canonicalize the UUID-STRING case."
  ;; Standardize on the more common (and more readable) lowercase UUID string, even though
  ;; UUID:PRINT-OBJECT and UUID:PRINT-BYTES print them in uppercase.
  (string-downcase uuid-string))

(defun make-uuid ()
  "Return a new UUID."
  (bt:with-lock-held (**make-uuid-lock**)
    ;; UUID:MAKE-V4-UUID is not thread safe. If you call it without locking, you get collisions.
    ;; We could potentially avoid locking by always creating a thread-local binding for
    ;; UUID:*UUID-RANDOM-STATE*, but it's easier to just acquire a lock here.
    (uuid:make-v4-uuid)))

(defun make-uuid-string ()
  "Return a new UUID string."
  (canonicalize-uuid-string (princ-to-string (make-uuid))))

(defun valid-uuid-string-p (uuid-string)
  "True if UUID-STRING is a valid string representation of a v4 UUID.

Note that this function requires that any hexadecimal digits in UUID-STRING are lowercased in accordance with CANONICALIZE-UUID-STRING."
  ;; See RFC 4122 for UUID format.
  ;; https://tools.ietf.org/html/rfc4122#section-4.1
  ;;
  ;; We validate that uuid-string is a valid v4 UUID in printed string format. That is, as a string
  ;; of hexadecimal digits (with certain restrictions) separated by hyphens in the expected places.
  (and (typep uuid-string 'string)
       (= (length uuid-string) 36)
       (eql (aref uuid-string  8) #\-)
       (eql (aref uuid-string 13) #\-)
       (eql (aref uuid-string 14) #\4)  ; version
       (eql (aref uuid-string 18) #\-)
       ;; https://tools.ietf.org/html/rfc4122#section-4.4
       ;; The two most-significant bits of the clock sequence field are #b10, meaning the
       ;; resulting hex digit of the most-significant byte is one of 8, 9, a, or b.
       (find (aref uuid-string 19) "89ab")
       (eql (aref uuid-string 23) #\-)
       (every (lambda (c)
                (find c "-0123456789abcdef"))
              uuid-string)))

