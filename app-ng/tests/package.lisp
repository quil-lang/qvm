;;;; app-ng/tests/package.lisp
;;;;
;;;; Author: Nik Tezak
;;;;         appleby

(fiasco:define-test-package #:qvm-app-ng-tests
  (:use #:qvm-app-ng)
  (:export
   #:run-qvm-app-ng-tests))
