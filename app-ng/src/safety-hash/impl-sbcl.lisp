;;;; app-ng/src/safety-hash/impl-sbcl.lisp
;;;;
;;;; Author: appleby
;;;;
;;;; The SAFETY-HASH implementation in this file relies on SBCL-specific HASH-TABLE extensions [1],
;;;; namely the :SYNCHRONIZED keyword to MAKE-HASH-TABLE, plus SB-EXT:WITH-LOCKED-HASH-TABLE for
;;;; multi-access operations.
;;;;
;;;; SB-SPROF indicates that this implementation is ~2x faster on SBCL than the one in
;;;; impl-bordeaux-threads.lisp when running a naive insert/read/delete test in a tight loop.
;;;;
;;;; [1]: http://www.sbcl.org/manual/#Hash-Table-Extensions
(in-package #:qvm-app-ng.safety-hash)

(deftype safety-hash () 'hash-table)

(defun safety-hash-p (object)
  (typep object 'safety-hash))

(defun call-with-locked-safety-hash (safety-hash function)
  (sb-ext:with-locked-hash-table (safety-hash)
    (funcall function safety-hash)))

(defmacro with-locked-safety-hash ((hash-table) safety-hash &body body)
  (check-type hash-table symbol)
  `(call-with-locked-safety-hash ,safety-hash (lambda (,hash-table) ,@body)))


;;; Standard HASH-TABLE API replacements.

(defun make-safety-hash (&rest make-hash-table-args)
  (apply #'make-hash-table :synchronized t make-hash-table-args))

(defun clrhash (safety-hash)
  (cl:clrhash safety-hash))

(defun gethash (key safety-hash &optional default)
  (cl:gethash key safety-hash default))

(defun (setf gethash) (new-value key safety-hash)
  (setf (cl:gethash key safety-hash) new-value))

(defun hash-table-count (safety-hash)
  (cl:hash-table-count safety-hash))

(defun remhash (key safety-hash)
  (cl:remhash key safety-hash))


;;; Higher-level multi-access & convenience functions.

(defun gethash-or-lose (key safety-hash)
  (multiple-value-bind (value foundp) (cl:gethash key safety-hash)
    (if foundp
        value
        (error "Failed to find key ~A in SAFETY-HASH" key))))

(defun insert-unique (key value safety-hash)
  (sb-ext:with-locked-hash-table (safety-hash)
    (if (nth-value 1 (cl:gethash key safety-hash))
        (error "Collision for key ~A in SAFETY-HASH" key)
        (setf (cl:gethash key safety-hash) value))))
