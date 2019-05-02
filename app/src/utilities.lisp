;;;; utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun slurp-lines (&optional (stream *standard-input*))
  (flet ((line () (read-line stream nil nil nil)))
    (with-output-to-string (s)
      (loop :for line := (line) :then (line)
            :while line
            :do (write-line line s)))))

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

(defun slurp-stream (stream)
  (with-output-to-string (s)
    (loop :for byte := (read-byte stream nil nil) :then (read-byte stream nil nil)
          :until (null byte)
          :do (write-char (code-char byte) s))))

(declaim (inline write-64-be))
(defun write-64-be (byte stream)
  "Write the 64-bit unsigned-byte BYTE to the binary stream STREAM."
  (declare (optimize speed (safety 0) (debug 0))
           (type (unsigned-byte 64) byte))
  (let ((a (ldb (byte 8 0) byte))
        (b (ldb (byte 8 8) byte))
        (c (ldb (byte 8 16) byte))
        (d (ldb (byte 8 24) byte))
        (e (ldb (byte 8 32) byte))
        (f (ldb (byte 8 40) byte))
        (g (ldb (byte 8 48) byte))
        (h (ldb (byte 8 56) byte)))
    (declare (type (unsigned-byte 8) a b c d e f g h))
    (write-byte h stream)
    (write-byte g stream)
    (write-byte f stream)
    (write-byte e stream)
    (write-byte d stream)
    (write-byte c stream)
    (write-byte b stream)
    (write-byte a stream)
    nil))

(declaim (inline write-complex-double-float-as-binary))
(defun write-complex-double-float-as-binary (z stream)
  "Take a complex double-float and write to STREAM its binary representation in big endian (total 16 octets)."
  (declare #.qvm::*optimize-dangerously-fast*
           (type (complex double-float) z))
  (let ((re (realpart z))
        (im (imagpart z)))
    (declare (type double-float re im)
             (dynamic-extent re im))
    (let ((encoded-re (ieee-floats:encode-float64 re))
          (encoded-im (ieee-floats:encode-float64 im)))
      (declare (type (unsigned-byte 64) encoded-re encoded-im)
               (dynamic-extent encoded-re encoded-im))
      (write-64-be encoded-re stream)
      (write-64-be encoded-im stream))))

(declaim (inline write-double-float-as-binary))
(defun write-double-float-as-binary (x stream)
  "Take a double-float and write to STREAM its binary representation in big endian (total 8 octets)."
  (declare #.qvm::*optimize-dangerously-fast*
           (type double-float x))
  (let ((encoded (ieee-floats:encode-float64 x)))
    (declare (type (unsigned-byte 64) encoded)
             (dynamic-extent encoded))
    (write-64-be encoded stream)))

(defun encode-list-as-json-list (list stream)
  (if (endp list)
      (format stream "[]")
      (yason:encode list stream)))

(defun yason-encode-to-string (obj)
  (with-output-to-string (s)
    (yason:encode obj s)))

(defun quit-nicely (&optional (code 0))
  ;; Portability function.
  #+sbcl
  (sb-ext:exit :code code :abort nil)
  #-sbcl
  (uiop:quit code t))

;;; Functions depending on the server state

(defun session-info ()
  (if (or (not (boundp 'tbnl:*session*))
          (null tbnl:*session*))
      ""
      (format nil
              "[~A Session:~D] "
              (tbnl:session-remote-addr tbnl:*session*)
              (tbnl:session-id tbnl:*session*))))

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
