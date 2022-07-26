;;;; examples/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm-examples
  (:use #:cl #:qvm)
  (:local-nicknames (#:quil #:cl-quil))

  ;; qft.lisp
  (:export
   #:bit-reversal-circuit               ; FUNCTION
   #:qft-circuit                        ; FUNCTION
   )
  )
