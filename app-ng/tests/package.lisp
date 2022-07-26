;;;; app-ng/tests/package.lisp
;;;;
;;;; Author: Nik Tezak
;;;;         appleby

(fiasco:define-test-package #:qvm-app-ng-tests
  (:use #:qvm-app-ng)
  (:local-nicknames (#:quil #:cl-quil))
  (:export
   #:run-qvm-app-ng-tests))
