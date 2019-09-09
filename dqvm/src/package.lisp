;;;; src/package.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(defpackage #:dqvm2
  (:use #:common-lisp
        #:cl-mpi
        #:cl-mpi-extensions
        #:static-vectors)
  (:import-from #:qvm
                #:boolean-bit
                #:defun-inlinable
                #:number-of-qubits
                #:transition)
  (:export #:*default-block-size*
           #:*print-addresses*
           #:address-member
           #:addresses
           #:amplitudes
           #:apply-inverse-permutation
           #:apply-inverse-qubit-permutation
           #:apply-permutation
           #:apply-qubit-permutation
           #:block-member
           #:block-size
           #:blocks-per-process
           #:compose-permutations
           #:copy-global-addresses
           #:distributed-qvm
           #:do-addresses
           #:do-addresses-in-block
           #:get-address-by-offset
           #:get-block-by-address
           #:get-effective-permutation
           #:get-initial-address
           #:get-rank-by-address
           #:get-rank-by-block
           #:global-addresses
           #:global-addresses=
           #:inverse-permutation
           #:is-identity-permutation-p
           #:make-addresses
           #:make-addresses-like
           #:make-distributed-qvm
           #:make-permutation
           #:number-of-addresses
           #:number-of-blocks
           #:number-of-processes
           #:number-of-qubits
           #:offset
           #:permutation
           #:print-qubit-permutation
           #:qubit-permutation
           #:rank
           #:remainder-blocks
           #:reset-wavefunction
           #:save-wavefunction
           #:scratch
           #:transposition
           #:update-permutation
           ))

(defpackage #:dqvm2-user
  (:use #:common-lisp
        #:cl-mpi
        #:cl-mpi-extensions
        #:dqvm2))
