;;;; app-ng/src/safety-hash/package.lisp
;;;;
;;;; Author: appleby
;;;;
;;;;     Safety-hash!
;;;;
;;;;     Ah we can hash if we want to, we can leave your locks behind
;;;;     Cause your threads don't hash and if they don't hash
;;;;     Well they're no threads of mine
;;;;
;;;; The SAFETY-HASH package implements a (semi-)portable, thread-safe interface to basic CRUD
;;;; operations for hash-table-like objects. SAFETY-HASHes are used in QVM-APP-NG to store
;;;; PERSISTENT-QVMs and other objects which might persist between requests.
;;;;
;;;; The SAFETY-HASH API sticks closely to the standard Common Lisp HASH-TABLE API, but doesn't
;;;; attempt to be a drop-in replacement. Only functions actually needed in the parent QVM-APP-NG
;;;; package are included. For example, some of the standard HASH-TABLE functions are missing
;;;; (HASH-TABLE-SIZE, MAPHASH, etc.), and other functions are included here which have no direct
;;;; counterpart for standard HASH-TABLEs (e.g. SAFETY-HASH:INSERT-UNIQUE).
;;;;
;;;; The SAFETY-HASH package supports the following implementations:
;;;;
;;;;   - impl-bordeaux-threads.lisp: a portable (but slow) implementation that relies on
;;;;     BORDEAUX-THREADS.
;;;;
;;;;   - impl-sbcl.lisp: an SBCL-specific version that relies on SBCL's :SYNCHRONIZED keyword to
;;;;     MAKE-HASH-TABLE, plus SB-EXT:WITH-LOCKED-HASH-TABLE for multi-access operations.
;;;;
;;;; At a minimum, CCL, ECL, and LispWorks all claim to support thread-safe hash-tables, and
;;;; therefore might also benefit from a platform-specific implementation. For links to docs for
;;;; associated implementation-specific extensions, see: https://github.com/rigetti/qvm/issues/186
(defpackage #:qvm-app-ng.safety-hash
  (:use #:cl)
  (:nicknames #:safety-hash)
  (:shadow #:clrhash
           #:gethash
           #:hash-table-count
           #:remhash)
  (:export #:safety-hash
           #:safety-hash-p
           #:make-safety-hash
           #:gethash-or-lose
           #:insert-unique
           #:call-with-locked-safety-hash
           #:with-locked-safety-hash
           ;; shadowing CL symbols
           #:clrhash
           #:gethash
           #:hash-table-count
           #:remhash))
