;;;; src/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm
  (:documentation "Package containing an implementation of a quantum virtual machine.")
  (:use #:cl
        #:abstract-classes)

  ;; config.lisp
  (:export
   #:*qubits-required-for-parallelization*
                                        ; VARIABLE
   #:*transition-verbose*               ; VARIABLE
   )

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

  ;; qam.lisp
  (:export
   #:quantum-abstract-machine           ; CLASS
   #:run                                ; GENERIC
   )

  ;; classical-memory.lisp
  (:export
   #:classical-memory                   ; TYPE
   #:make-classical-memory              ; FUNCTION
   #:peek-bit                           ; FUNCTION
   #:poke-bit                           ; FUNCTION
   #:bit-range                          ; TYPE
   #:make-bit-range                     ; FUNCTION
   #:bit-range-left                     ; FUNCTION
   #:bit-range-right                    ; FUNCTION
   #:bit-range-width                    ; FUNCTION
   #:peek-bits                          ; FUNCTION
   #:poke-bits                          ; FUNCTION
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
   #:pure-state-qvm                     ; CLASS
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
   #:depolarizing-qvm                   ; CLASS
   )

  ;; noisy-qvm.lisp
  (:export
   #:noisy-qvm                          ; CLASS
   #:make-pauli-noise-map               ; FUNCTION
   #:set-noisy-gate                     ; FUNCTION
   )
  )
