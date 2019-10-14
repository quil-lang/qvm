;;;; app-ng/src/safety-hash/package.lisp
;;;;
;;;; Author: appleby

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
