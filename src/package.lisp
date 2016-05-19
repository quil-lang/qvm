;;;; src/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm
  (:use #:cl)

  ;; qvm.lisp
  (:export
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

  ;; execution.lisp
  (:export
   #:qvm                                ; CLASS
   #:run                                ; FUNCTION
   #:run-program                        ; FUNCTION
   )

  ;; gates.lisp
  (:export
   #:operator-matrix-from-truth-table   ; FUNCTION
   #:controlled                         ; FUNCTION
   #:hadamard                           ; FUNCTION
   #:pauli-X                            ; FUNCTION
   #:pauli-Y                            ; FUNCTION
   #:pauli-Z                            ; FUNCTION
   #:qnot                               ; FUNCTION
   #:cnot                               ; FUNCTION
   #:sqrt-qnot                          ; FUNCTION
   #:nand                               ; FUNCTION
   #:rotation-x                         ; FUNCTION
   #:rotation-y                         ; FUNCTION
   #:rotation-z                         ; FUNCTION
   #:cphase                             ; FUNCTION
   #:swap                               ; FUNCTION
   #:sqrt-swap                          ; FUNCTION
   #:toffoli                            ; FUNCTION
   #:fredkin                            ; FUNCTION

   ;; meta-operators
   #:print-amplitudes                   ; FUNCTION
   #:print-probabilities                ; FUNCTION
   #:classical-call                     ; FUNCTION
   )

  ;; qft.lisp
  (:export
   #:bit-reversal-circuit               ; FUNCTION
   #:qft-circuit                        ; FUNCTION
   )
  )
