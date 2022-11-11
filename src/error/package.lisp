;;;; error/package.lisp
;;;;
;;;; This subpackage houses some QVM variants useful for tracing errors through
;;;; circuits which are expected to measure out 0s in the absence of noise.

(defpackage #:qvm.error
  (:use #:cl
        #:abstract-classes
        #:qvm)
  (:import-from #:qvm #:multiprobabilistically)
  (:local-nicknames (#:quil     #:cl-quil.frontend))
  
  (:export
   #:fowler-pure-state-qvm              ; CLASS
   #:fowler-stabilizer-qvm              ; CLASS
   #:error-qvm                          ; CLASS
   #:make-error-qvm                     ; CONSTRUCTOR
   #:copy-fowler-qvm                    ; COPY-CONSTRUCTOR
   ))
