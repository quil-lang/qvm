;;;; app-ng/src/safety-hash/impl-sbcl.lisp
;;;;
;;;; Author: appleby
;;;;
;;;; This file defines the interface that platform-specific implementations in the impl-*.lisp files
;;;; are expected to follow, and will signal a compile-time error if any of the expected functions
;;;; and/or macros are missing. Additionally, this file provides default documentation strings for
;;;; the the expected interfaces.
(in-package #:qvm-app-ng.safety-hash)

(define-condition interface-error (simple-error) ())

(defun interface-error (type symbol)
  (error 'interface-error
         :format-control "SAFETY-HASH: Failed to implement ~A of type ~A."
         :format-arguments (list symbol type)))

(defun check-required (type name lambda-list &optional documentation)
  (declare (ignore lambda-list))
  (check-type type (member defun defmacro))
  (check-type documentation (or null string))
  (assert (or (symbolp name)
              (and (eq 'defun type) (consp name) (eq 'setf (car name)))))
  (multiple-value-bind (doctype defined-p)
      (ecase type
        (defun (values 'function (and (fboundp name)
                                      (or (consp name)
                                          (not (macro-function name))))))
        (defmacro (values 'function (and (fboundp name) (macro-function name)))))
    (unless defined-p
      (interface-error type name))
    (unless (documentation name doctype)
      (setf (documentation name doctype) documentation))))

(defun check-all-required (specs)
  (mapc (lambda (spec)
          (apply #'check-required spec))
        specs))

(check-all-required
 '((defun safety-hash-p (object)
     "Is OBJECT a SAFETY-HASH?")

   (defun call-with-locked-safety-hash (safety-hash function)
     "Call FUNCTION with the SAFETY-HASH lock held.

SAFETY-HASH is a SAFETY-HASH.

FUNCTION is a function-designator for a FUNCTION of a single argument. It will be passed a reference to the locked SAFETY-HASH.

Return the value of (FUNCALL FUNCTION SAFETY-HASH).")

   (defmacro with-locked-safety-hash ((hash-table) safety-hash &body body)
     "Lock SAFETY-HASH and execute BODY with HASH-TABLE bound to SAFETY-HASH's corresponding HASH-TABLE object.")

   (defun make-safety-hash (&rest make-hash-table-args)
     "Make a new SAFETY-HASH.

Any arguments are passed through to MAKE-HASH-TABLE when initializing the underlying HASH-TABLE object.")

   (defun clrhash (safety-hash)
     "Remove all entries from SAFETY-HASH and return SAFETY-HASH.")

   (defun gethash (key safety-hash &optional default)
     "Lookup the value of KEY in SAFETY-HASH.")

   (defun (setf gethash) (new-value key safety-hash)
     "Set the entry for KEY to NEW-VALUE in SAFETY-HASH and return NEW-VALUE.")

   (defun hash-table-count (safety-hash)
     "Return the number of entries in SAFETY-HASH.")

   (defun remhash (key safety-hash)
     "Remove the entry in SAFETY-HASH associated with KEY.

Return T if there was such an entry, or NIL if not.")

   (defun gethash-or-lose (key safety-hash)
     "Lookup KEY in SAFETY-HASH or signal an ERROR if no such key is present.

Return the value corresponding to KEY on success.")

   (defun insert-unique (key value safety-hash)
     "Insert the KEY/VALUE pair into SAFETY-HASH, or signal an ERROR if KEY is already present.

Return VALUE on success.")))
