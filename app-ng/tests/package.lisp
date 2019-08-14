;;;; app-ng/tests/package.lisp
;;;;
;;;; Author: appleby

(fiasco:define-test-package #:qvm-app-ng-tests
  (:use #:qvm-app-ng #:qvm-app-ng.config)
  (:export
   #:run-qvm-app-ng-tests))
