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
   #:parallelization-limit              ; TYPE
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

  ;; floats.lisp
  (:export
   #:flonum                             ; TYPE, FUNCTION, COMPILER MACRO
   #:cflonum                            ; TYPE, FUNCTION, COMPILER MACRO
   )

  ;; allocator.lisp
  (:export
   #:allocation-length                  ; GENERIC, METHOD
   #:allocate-vector                    ; GENERIC, METHOD
   #:lisp-allocation                    ; CLASS
   #:c-allocation                       ; CLASS
   #:posix-shared-memory-allocation     ; CLASS
   )

  ;; linear-algebra.lisp
  (:export
   #:quantum-operator                   ; TYPE
   #:quantum-state                      ; TYPE
   #:octets-required-for-qubits         ; FUNCTION
   #:make-matrix                        ; FUNCTION
   #:magicl-matrix-to-quantum-operator  ; FUNCTION
   )

  ;; qam.lisp
  (:export
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
   #:memory-index-out-of-bounds         ; CONDITION
   #:classical-memory                   ; TYPE
   #:make-classical-memory              ; FUNCTION
   #:memory-size                        ; FUNCTION
   #:memory-bit-ref                     ; ACCESSOR
   #:memory-octet-ref                   ; ACCESSOR
   #:memory-integer-ref                 ; ACCESSOR
   #:memory-real-ref                    ; ACCESSOR
   #:zero-out-classical-memory          ; FUNCTION
   #:memory-descriptors-to-qvm-memory-model
                                        ; FUNCTION
   #:classical-memory-subsystem         ; CLASS
   )

  ;; classical-memory-mixin.lisp
  (:export
   #:classical-memory-mixin             ; CLASS
   #:memory-ref                         ; FUNCTION
   #:load-program                       ; FUNCTION
   )

  ;; wavefunction.lisp
  (:export
   #:wf                                 ; FUNCTION
   #:copy-wavefunction                  ; FUNCTION
   #:probability                        ; FUNCTION
   #:apply-operator                     ; FUNCTION
   #:apply-matrix-operator              ; FUNCTION
   #:normalize-wavefunction             ; FUNCTION
   #:wavefunction-ground-state-probability
                                        ; FUNCTION
   #:wavefunction-excited-state-probability
                                        ; FUNCTION
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
   #:nth-amplitude                      ; FUNCTION
   #:map-amplitudes                     ; FUNCTION
   #:octets-required-for-quantum-state  ; FUNCTION
   )

  ;; measurement.lisp
  (:export
   #:qubit-probability                  ; DEPRECATED FUNCTION
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

  ;; apply-gate.lisp
  (:export
   #:apply-gate                         ; GENERIC, METHOD
   )

  ;; depolarizing-noise.lisp
  (:export
   #:depolarizing-qvm                   ; CLASS
   )

  ;; noisy-qvm.lisp
  (:export
   #:noisy-qvm                          ; CLASS
   #:make-pauli-noise-map               ; FUNCTION
   #:set-noisy-gate                     ; GENERIC, METHOD
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
