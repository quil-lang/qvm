;;;; qvm.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm
  :description "An implementation of the Quantum Abstract Machine."
  :author "Robert Smith <robert@rigetti.com>"
  :version (:read-file-form "VERSION.txt")
  :defsystem-depends-on (#+unix #:cffi-grovel)
  :depends-on (
               ;; General utilities
               #:alexandria
               ;; Abstract classes
               #:abstract-classes
               ;; IEEE-754 float parsing
               #:ieee-floats
               ;; Parallelization utilities
               #:lparallel
               ;; C foreign function interface
               #+unix
               #:cffi
               ;; Quil parsing and analysis
               (:version #:cl-quil "0.5.0")
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-tests)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               #+unix
               (:cffi-grovel-file "system-constants")
               (:file "utilities")
               (:file "linear-algebra")
               (:file "wavefunction")
               (:file "gates")
               (:file "quil")
               (:file "qvm")
               (:file "measurement")
               (:file "transition")
               (:file "depolarizing-noise")
               (:file "execution")))
