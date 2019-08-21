;;; dqvm2.asd
;;;
;;; Author: Juan M. Bello-Rivas

(defsystem #:dqvm2
  :description "Rigetti Distributed Quantum Virtual Machine"
  :author "Juan M. Bello Rivas <jbellorivas@rigetti.com>, Robert Smith <robert@rigetti.com>, Lauren Capelluto <lauren@rigetti.com>"
  :licence "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (
               ;; General utilities
               #:alexandria
               ;; MPI bindings and extensions
               #:cl-mpi
               #:cl-mpi-extensions
               ;; Quil parsing
               (:version #:cl-quil "1.10.0")
               ;; Logging
               #:cl-syslog
               ;; Command line argument parsing
               #:command-line-arguments
               ;; Matrix algebra
               (:version #:magicl "0.6.1")
               ;; Quantum virtual machine
               #:qvm
               ;; Allocation of C vectors
               (:version #:static-vectors "1.8.3")
               ;; Finalizers and portable GC calls
               #:trivial-garbage
               )
  :defsystem-depends-on (#:cl-mpi-asdf-integration)
  :class :mpi-program
  :build-operation :static-program-op
  :build-pathname "dqvm2"
  :entry-point "dqvm2::entry-point"
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "permutations")
               (:file "mpi")
               (:file "logging")
               (:file "global-addresses")
               (:file "addresses")
               (:file "distributed-qvm")
               (:file "linear-algebra")
               (:file "sendrecv")
               (:file "measurement")
               (:file "apply-distributed-gate")
               (:file "transition")
               (:file "entry-point"))
  :in-order-to ((asdf:test-op (asdf:test-op #:dqvm2-tests))))
