;;;; app-ng/src/globals.lisp
;;;;
;;;; Author: Nikolas Tezak
;;;;         Robert Smith

(in-package #:qvm-app-ng)

(global-vars:define-global-var **available-simulation-methods** '("pure-state" "full-density-matrix")
  "List of available simulation methods.")
