;;;; tests/package.lisp
;;;;
;;;; Author: Robert Smith

(fiasco:define-test-package #:qvm-tests
  (:use #:qvm)
  
  ;; suite.lisp
  (:export
   #:run-tests))
