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
               (:version #:magicl "0.2.0")
               ;; weak hash tables
               #:trivial-garbage
               ;; static globals
               #:global-vars
               ;; C foreign function interface
               #+unix
               #:cffi
               #+unix
               (:version #:static-vectors "1.8.3")
               #:trivial-garbage
               ;; Quil parsing and analysis
               (:version #:cl-quil "0.13.1")
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               #+unix
               (:cffi-grovel-file "grovel-system-constants")
               #+unix
               (:cffi-grovel-file "grovel-shared-memory")
               (:file "config")
               #+clozure
               (:file "impl/clozure")
               #+sbcl
               (:file "impl/sbcl")
               #+lispworks
               (:file "impl/lispworks")
               #+unix
               (:file "shm")
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
               (:file "depolarizing-noise")
               (:file "noisy-qvm")
               (:file "execution")
               (:file "path-simulate")
               (:file "misc")))
