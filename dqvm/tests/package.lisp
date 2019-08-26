;;;; tests/package.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(fiasco:define-test-package #:dqvm2-tests
  (:use #:dqvm2)

  (:export
   #:run-dqvm2-tests))
