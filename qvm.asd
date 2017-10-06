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
               ;; Matrix algebra
               #:magicl
               ;; weak hash tables
               #:trivial-garbage
               ;; static globals
               #:global-vars
               ;; C foreign function interface
               #+unix
               #:cffi
               ;; Quil parsing and analysis
               (:version #:cl-quil "0.6.0")
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))

  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "config")
               #+unix
               (:cffi-grovel-file "system-constants")
               (:file "utilities")
               (:file "linear-algebra")
               (:file "qam")
               (:file "classical-memory")
               (:file "wavefunction")
               (:file "apply-gate")
               (:file "qvm")
               (:file "measurement")
               (:file "transition")
               (:file "depolarizing-noise")
               (:file "execution")))
