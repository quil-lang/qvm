;;;; src/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm
  (:documentation "Package containing an implementation of a quantum virtual machine.")
  (:use #:cl
        #:abstract-classes)

  ;; utilities.lisp
  (:export
   #:count-logical-cores                ; FUNCTION
   #:prepare-for-parallelization        ; FUNCTION
   )

  ;; linear-algebra.lisp
  (:export
   #:quantum-operator                   ; TYPE
   #:quantum-state                      ; TYPE
   #:flonum                             ; TYPE, FUNCTION, COMPILER MACRO
   #:cflonum                            ; TYPE, FUNCTION, COMPILER MACRO
   #:octets-required-for-qubits         ; FUNCTION
   )

  ;; wavefunction.lisp
  (:export
   #:probability                        ; FUNCTION
   #:apply-operator                     ; FUNCTION
   #:apply-matrix-operator              ; FUNCTION
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
   #:map-amplitudes                     ; FUNCTION
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
   #:gate-operator                      ; GENERIC, METHOD
   #:apply-gate                         ; GENERIC, METHOD
   #:operator-matrix-from-truth-table   ; FUNCTION
   #:controlled                         ; FUNCTION
   )

  ;; depolarizing-noise.lisp
  (:export
   #:noisy-qvm                          ; CLASS
   )
  )
