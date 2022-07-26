;;;; src-tests/package.lisp
;;;;
;;;; Author: Nik Tezak

(fiasco:define-test-package #:qvm-app-tests
  (:use #:qvm-app)
  (:local-nicknames (#:quil #:cl-quil))
  
  ;; suite.lisp
  (:export
   #:run-qvm-app-tests))
