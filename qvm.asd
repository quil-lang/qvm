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
               (:version #:magicl "0.5.0")
               ;; weak hash tables
               #:trivial-garbage
               ;; static globals
               #:global-vars
               ;; C foreign function interface
               #:cffi
               #+unix
               (:version #:static-vectors "1.8.3")
               #:trivial-garbage
               ;; Quil parsing and analysis
               (:version #:cl-quil "0.21.0")
               ;; Portable random number generator
               #:mt19937
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel-system-constants" :if-feature :unix)
               (:cffi-grovel-file "grovel-shared-memory" :if-feature :unix)
               (:file "config")
               (:file "impl/clozure" :if-feature :clozure)
               (:file "impl/sbcl" :if-feature :sbcl)
               (:file "impl/lispworks" :if-feature :lispworks)
               (:file "shm" :if-feature :unix)
               (:file "utilities")
               (:file "linear-algebra")
               (:file "qam")
               (:file "classical-memory")
               (:file "wavefunction")
               (:file "compile-gate")
               (:file "apply-gate")
               (:file "qvm")
               (:file "measurement")
               (:file "transition")
               (:file "transition-classical-instructions")
               (:file "depolarizing-noise")
               (:file "noisy-qvm")
               (:file "execution")
               (:file "path-simulate")
               (:file "misc")))
