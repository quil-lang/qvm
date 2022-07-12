;;;; app-src/entry-point.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm-app
  (:use #:cl #:qvm)
  (:local-nicknames (#:quil #:cl-quil/frontend)))

(defpackage #:qvm-app/debugger
  (:use #:cl #:qvm-app)
  (:export #:debugger)
  (:import-from #:alexandria #:assoc-value #:once-only))
