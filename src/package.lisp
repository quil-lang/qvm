;;;; src/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:qvm
  (:documentation "Package containing an implementation of a quantum virtual machine.")
  (:use #:cl
        #:abstract-classes)

  (:shadowing-import-from #:mt19937
                          #:random)

  ;; config.lisp
  (:export
   #:*qubits-required-for-parallelization*
                                        ; VARIABLE
   #:*transition-verbose*               ; VARIABLE
   #:*compile-before-running*           ; VARIABLE
   )

  ;; utilities.lisp
  (:export
   #:count-logical-cores                ; FUNCTION
   #:prepare-for-parallelization        ; FUNCTION
   #:seeded-random-state                ; FUNCTION
   #:with-random-state                  ; MACRO
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
   #:measure                            ; GENERIC
   #:measure-all                        ; GENERIC
   #:number-of-qubits                   ; GENERIC
   )

  ;; classical-memory.lisp
  (:export
   #:quil-bit                           ; TYPE
   #:quil-octet                         ; TYPE
   #:qvm-integer                        ; TYPE
   #:qvm-real                           ; TYPE
   #:classical-memory                   ; TYPE
   #:make-classical-memory              ; FUNCTION
   #:classical-memory-size              ; FUNCTION
   #:memory-bit-ref                     ; ACCESSOR
   #:memory-octet-ref                   ; ACCESSOR
   #:memory-integer-ref                 ; ACCESSOR
   #:memory-real-ref                    ; ACCESSOR
   #:zero-out-classical-memory          ; FUNCTION
   #:memory-descriptors-to-qvm-memory-model
                                        ; FUNCTION
   #:classical-memory-subsystem         ; CLASS
   )

  ;; wavefunction.lisp
  (:export
   #:copy-wavefunction                  ; FUNCTION
   #:probability                        ; FUNCTION
   #:apply-operator                     ; FUNCTION
   #:apply-matrix-operator              ; FUNCTION
   #:normalize-wavefunction             ; FUNCTION
   )

  ;; compile-gate.lisp
  (:export
   #:warm-apply-matrix-operator-cache   ; FUNCTION
   )

  ;; qvm.lisp
  (:export
   #:pure-state-qvm                     ; CLASS
   #:number-of-qubits                   ; ACCESSOR
   #:make-qvm                           ; FUNCTION
   #:load-program                       ; FUNCTION
   #:memory-ref                         ; FUNCTION
   #:nth-amplitude                      ; FUNCTION
   #:map-amplitudes                     ; FUNCTION
   #:octets-required-for-quantum-state  ; FUNCTION
   )

  ;; measurement.lisp
  (:export
   #:qubit-probability                  ; FUNCTION
   #:force-measurement                  ; FUNCTION
   )

  ;; transition.lisp
  (:export
   #:invalid-instruction-encountered    ; CONDITION
   #:invalid-instruction                ; READER
   #:invalid-reason                     ; READER
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

  ;; density-qvm
  (:export
   #:density-qvm                        ; CLASS
   #:make-density-qvm                   ; FUNCTION
   )

  ;; path-simulate.lisp
  (:export
   #:path-simulate                      ; FUNCTION
   #:wavefunction-from-path-simulation  ; FUNCTION
   )

  ;; misc.lisp
  (:export
   #:program-matrix                     ; FUNCTION
   )
  )
