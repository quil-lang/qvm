;;;; tune-threads/package.lisp
;;;;
;;;; Author: John Lapeyre

(defpackage #:qvm-tune-threads
  (:use #:qvm
        #:cl)
  (:export
   #:scan-num-qubits               ; FUNCTION
   #:scan-num-threads              ; FUNCTION
   #:optimal-num-threads-from-scan ; FUNCTION
   #:optimal-num-threads           ; FUNCTION
   ))
