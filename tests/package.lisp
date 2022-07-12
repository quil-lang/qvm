;;;; tests/package.lisp
;;;;
;;;; Author: Robert Smith

(fiasco:define-test-package #:qvm-tests
  (:use #:qvm)
  (:local-nicknames (#:quil #:cl-quil/frontend))
  
  ;; suite.lisp
  (:export
   #:run-qvm-tests))
