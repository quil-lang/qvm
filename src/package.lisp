;;;; src/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm
  (:use #:cl)

  ;; utilities.lisp
  (:export
   #:count-logical-cores                ; FUNCTION
   #:prepare-for-parallelization        ; FUNCTION
   )

  ;; wavefunction.lisp
  (:export
   #:probability                        ; FUNCTION
   #:apply-operator                     ; FUNCTION
   #:normalize-wavefunction             ; FUNCTION
   )

  ;; qvm.lisp
  (:export
   #:quantum-virtual-machine            ; CLASS
   #:number-of-qubits                   ; ACCESSOR
   #:classical-memory-size              ; ACCESSOR
   #:make-qvm                           ; FUNCTION
   #:load-program                       ; FUNCTION
   #:classical-bit                      ; ACCESSOR
   #:print-amplitudes                   ; FUNCTION
   #:print-probabilities                ; FUNCTION
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
   #:transition                         ; GENERIC, METHOD
   )

  ;; execution.lisp
  (:export
   #:run                                ; GENERIC, METHOD
   #:run-program                        ; FUNCTION
   )

  ;; gates.lisp
  (:export
   #:operator-matrix-from-truth-table   ; FUNCTION
   #:controlled                         ; FUNCTION
   )

  ;; depolarizing-noise.lisp
  (:export
   #:noisy-qvm                          ; CLASS
   )
  )
