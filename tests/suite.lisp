;;;; tests/suite.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defsuite qvm-test-suite)

(defun run-tests ()
  (qvm-test-suite))
