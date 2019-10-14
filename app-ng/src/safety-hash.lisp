;;;; safety-hash.lisp
;;;;
;;;; Author: appleby
;;;;
;;;;     Safety-hash!
;;;;
;;;;     Ah we can hash if we want to, we can leave your locks behind
;;;;     Cause your threads don't hash and if they don't hash
;;;;     Well they're are no threads of mine
;;;;
;;;; SAFETY-HASH implements a portable, thread-safe interface to basic CRUD operations for a
;;;; globally-accessible hash-table-like object. SAFETY-HASHes are used in QVM-APP-NG to store
;;;; PERSISTENT-QVMs and other objects which might persist between requests.
;;;;
;;;; The SAFETY-HASH implementation in this file is intended to be simple and portable (and probably
;;;; slow). This portable implementation acquires and releases a non-recursive per-object lock
;;;; around every operation. It depends on BORDEAUX-THREADS for the locking primitives.
;;;;
;;;; The SAFETY-HASH API sticks closely to the standard Common Lisp HASH-TABLE API, but doesn't
;;;; attempt to be a drop-in replacement. Some of the standard functions that operate on HASH-TABLEs
;;;; are missing, and other functions are included here which have no direct counterpart for
;;;; standard HASH-TABLEs (e.g. SAFETY-HASH-INSERT-UNIQUE). Some of the standard HASH-TABLE
;;;; functions are missing only because the rest of QVM-APP-NG has no use for them yet
;;;; (e.g. HASH-TABLE-SIZE); others (MAPHASH, WITH-HASH-TABLE-ITERATOR) are missing partially
;;;; because they are not yet needed, and partially because adding them would either mean switching
;;;; to recursive locks, or else modifying the interfaces to do something different than their
;;;; documented behavior in the standard. In such cases, the user always has an escape hatch in the
;;;; form of the WITH-LOCKED-SAFETY-HASH macro, which locks the SAFETY-HASH and binds a reference to
;;;; the underlying HASH-TABLE, which the caller can then modify as they please.
;;;;
;;;; For example, the caller is allowed to modify the current entry when iterating over a
;;;; HASH-TABLE with MAPHASH, like so
;;;;
;;;;     (maphash (lambda (key value)
;;;;                (setf (gethash key some-hash) (1+ value)))
;;;;              some-hash)
;;;;
;;;; Translating the above into SAFETY-HASH-compatible API would produce something like
;;;;
;;;;     (safety-hash-maphash (lambda (key value)
;;;;                            (setf (safety-hash-gethash key some-hash) (1+ value)))
;;;;              some-hash)
;;;;
;;;; where the call to SAFETY-HASH-GETHASH would attempt to acquire the lock already held by
;;;; SAFETY-HASH-MAPHASH. Switching to recursive locks here could work. For now, the recommend
;;;; approach is to instead use WITH-LOCKED-SAFETY-HASH and operate on the underlying HASH-TABLE
;;;; directly, like so:
;;;;
;;;;     (with-locked-safety-hash (hash-table) safety-hash
;;;;       (maphash (lambda (key value)
;;;;                  (setf (gethash key hash-table) (1+ value)))
;;;;                hash-table)
(in-package #:qvm-app-ng)

(defstruct (safety-hash (:constructor %make-safety-hash))
  (lock  (error "Must provide LOCK")  :read-only t)
  (table (error "Must provide TABLE") :read-only t))

(defun call-with-locked-safety-hash (safety-hash function)
  (bt:with-lock-held ((safety-hash-lock safety-hash))
    (funcall function (safety-hash-table safety-hash))))

#+#:warning-extreme-danger-will-robinson-use-only-in-case-of-testing
(defun call-with-locked-safety-hash (safety-hash function)
  (funcall function (safety-hash-table safety-hash)))

(defmacro with-locked-safety-hash ((hash-table) safety-hash &body body)
  (check-type hash-table symbol)
  `(call-with-locked-safety-hash ,safety-hash (lambda (hash-table) ,@body)))


;;; Standard HASH-TABLE API replacements.

(defun make-safety-hash (&rest make-hash-table-args)
  (%make-safety-hash :lock (bt:make-lock) :table (apply #'make-hash-table make-hash-table-args)))

(defun safety-hash-clrhash (safety-hash)
  (with-locked-safety-hash (hash-table) safety-hash
    (clrhash hash-table)))

(defun safety-hash-gethash (key safety-hash &optional default)
  (with-locked-safety-hash (hash-table) safety-hash
    (gethash key hash-table default)))

(defun (setf safety-hash-gethash) (new-value key safety-hash)
  (with-locked-safety-hash (hash-table) safety-hash
    (setf (gethash key hash-table) new-value)))

(defun safety-hash-table-count (safety-hash)
  (with-locked-safety-hash (hash-table) safety-hash
    (hash-table-count hash-table)))

(defun safety-hash-remhash (key safety-hash)
  (with-locked-safety-hash (hash-table) safety-hash
    (remhash key hash-table)))


;;; Higher-level multi-access & convenience functions.

(defun safety-hash-gethash-or-lose (key safety-hash)
  (with-locked-safety-hash (hash-table) safety-hash
    (multiple-value-bind (value foundp) (gethash key hash-table)
      (if foundp
          value
          (error "Failed to find key ~A in SAFETY-HASH" key)))))

(defun safety-hash-insert-unique (key value safety-hash)
  (with-locked-safety-hash (hash-table) safety-hash
    (if (nth-value 1 (gethash key hash-table))
        (error "Collision for key ~A in SAFETY-HASH ~A" key safety-hash)
        (setf (gethash key hash-table) value))))
