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
   #:boolean-bit                        ; FUNCTION
   #:count-logical-cores                ; FUNCTION
   #:defun-inlinable                    ; MACRO
   #:pdotimes                           ; MACRO
   #:prepare-for-parallelization        ; FUNCTION
   #:seeded-random-state                ; FUNCTION
   #:psum-dotimes                       ; MACRO
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
   #:amplitude-address                  ; TYPE
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
  
  ;; state-representation.lisp
  (:export
   #:pure-state                         ; CLASS
   #:density-matrix-state               ; CLASS
   #:state-elements                     ; GENERIC, METHOD 
   #:set-to-zero-state                  ; GENERIC, METHOD
   #:make-pure-state                    ; FUNCTION
   #:make-density-matrix-state          ; FUNCTION
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
   #:apply-gate-to-state                ; GENERIC, METHOD
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

  ;; noise-models
  (:export
   #:noise-predicate                    ; CLASS
   #:predicate-function                 ; READER
   #:priority                           ; READER
   #:noise-position                     ; READER
   #:name                               ; READER 
   #:make-noise-predicate               ; FUNCTION
   #:predicate-and                      ; FUNCTION
   #:predicate-or                       ; FUNCTION
   #:predicate-not                      ; FUNCTION
   #:noise-rule                         ; CLASS
   #:make-noise-rule                    ; FUNCTION
   #:noise-model                        ; CLASS
   #:noise-rules                        ; ACCESSOR
   #:make-noise-model                   ; FUNCTION
   #:add-noise-models                   ; FUNCTION
   #:multiply-noise-models              ; FUNCTION
   #:match-strict-qubits                ; FUNCTION
   #:match-any-n-qubits                 ; FUNCTION
   #:match-strict-gate                  ; FUNCTION
   #:match-any-gates                    ; FUNCTION
   #:match-all-nq-gates                 ; FUNCTION
   #:match-all-gates                    ; FUNCTION
   #:match-measure                      ; FUNCTION
   #:match-measure-at-strict            ; FUNCTION
   #:match-measure-at-any               ; FUNCTION
   #:match-instr-idxs                   ; FUNCTION
   )               
  
  ;; channel-qvm
  (:export
   #:channel-qvm                        ; CLASS
   #:noise-model                        ; ACCESSOR
   )
  
  ;; basic-noise-qvm
  (:export
   #:basic-noise-qvm                   ; CLASS
   #:tphi                              ; FUNCTION
   #:damping-kraus-map                 ; FUNCTION
   #:dephasing-kraus-map               ; FUNCTION
   #:depolarizing-kraus-map            ; FUNCTION
   #:kraus-kron                        ; FUNCTION
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

  ;; unitary-matrix-qvm.lisp
  (:export
   #:unitary-qvm
   #:make-unitary-qvm
   #:unitary-qvm-underlying-matrix
   #:parsed-program-unitary-matrix)
  )
