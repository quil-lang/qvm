;;;; package.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Lauren Capelluto

;;; Here we set up a tree of packages so as to separate the master and
;;; worker nodes sufficiently.

(defpackage #:dqvm.common
  (:documentation "The package containing symbols useful to both the master and the worker. These symbols should have the *same* meaning in both processes.")
  (:use #:cl)
  (:export #:+master-rank+              ; FUNCTION
           #:+my-rank+                  ; SYMBOL MACRO
           #:+worker-count+             ; SYMBOL MACRO
           #:master-node-p              ; FUNCTION
           #:worker-node-p              ; FUNCTION
           #:power-of-two-p             ; FUNCTION
           #:with-total-readability     ; MACRO
           #:with-sane-read-settings    ; MACRO
           #:with-raw-vector            ; MACRO
           #:cluster                    ; STRUCTURE
           #:make-cluster               ; FUNCTION
           #:copy-cluster               ; FUNCTION
           #:clusterp                   ; FUNCTION
           #:qubit-count                ; STRUCTURE ACCESSOR
           #:ordering                   ; STRUCTURE ACCESSOR
           #:operating-qubits           ; STRUCTURE ACCESSOR
           #:current-instruction        ; STRUCTURE ACCESSOR
           #:**cluster**                ; STATIC GLOBAL
           #:serialize-cluster          ; FUNCTION
           #:deserialize-cluster        ; FUNCTION
           #:instruction->string        ; FUNCTION
           #:string->instruction        ; FUNCTION
           #:format-locked              ; FUNCTION
           #:call-with-probed-size      ; FUNCTION
           #:receive-string             ; FUNCTION
           #:everybody-synchronize      ; FUNCTION
           #:with-errors-printed-verbosely
                                        ; MACRO
           )
  )

(defpackage #:dqvm.master
  (:documentation "The package containing symbols useful to the master.")
  (:use #:cl #:dqvm.common)
  (:export #:%main-master))

(defpackage #:dqvm.worker
  (:documentation "The package containing symbols useful to the worker.")
  (:use #:cl #:dqvm.common)
  (:export #:%main-worker))

(defpackage #:dqvm
  (:documentation "The public distributed QVM package.")
  (:use #:cl #:dqvm.common)
  (:export #:%main))
