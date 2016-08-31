;;;; src/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm
  (:use #:cl)

  ;; qvm.lisp
  (:export
   #:quantum-virtual-machine            ; CLASS
   #:number-of-qubits                   ; ACCESSOR
   #:classical-memory-size              ; ACCESSOR
   #:make-qvm                           ; FUNCTION
   #:load-program                       ; FUNCTION
   #:classical-bit                      ; ACCESSOR
   #:apply-operator                     ; FUNCTION
   #:probability                        ; FUNCTION
   )

  ;; measurement.lisp
  (:export
   #:qubit-probability                  ; FUNCTION
   #:force-measurement                  ; FUNCTION
   #:measure                            ; FUNCTION
   #:parallel-measure                   ; FUNCTION
   )

  ;; transition.lisp
  (:export
   #:invalid-gate-invocation            ; CONDITION
   #:transition-qvm                     ; GENERIC, METHOD
   )

  ;; execution.lisp
  (:export
   #:run                                ; FUNCTION
   #:run-program                        ; FUNCTION
   )

  ;; gates.lisp
  (:export
   #:operator-matrix-from-truth-table   ; FUNCTION
   #:controlled                         ; FUNCTION

   #:print-amplitudes                   ; FUNCTION
   #:print-probabilities                ; FUNCTION
   #:classical-call                     ; FUNCTION
   )

  ;; depolarizing-noise.lisp
  (:export
   #:noisy-qvm                          ; CLASS
   )
  )
