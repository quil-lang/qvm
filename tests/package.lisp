;;;; tests/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm-tests
  (:use #:cl)
  (:use #:hu.dwim.stefil
        #:qvm)
  
  ;; suite.lisp
  (:export
   #:run-tests))
