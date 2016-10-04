;;;; bench/package.lisp
;;;;
;;;; Author: Robert Smith

(benchmark:define-benchmark-package #:qvm-benchmarks
  (:use #:qvm)
  
  ;; suite.lisp
  (:export
   #:run-benchmarks))
