;;;; app-ng/src/globals.lisp
;;;;
;;;; Author: Nikolas Tezak
;;;;         Robert Smith

(in-package #:qvm-app-ng)

;; These are GLOBAL-VARS but act like constants, so name them like constants.
(global-vars:define-global-var +available-simulation-methods+ '(pure-state full-density-matrix)
  "List of available simulation methods.")
(global-vars:define-global-var +available-allocation-methods+ '(native foreign)
  "List of available allocation methods.")
