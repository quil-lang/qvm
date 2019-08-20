;;;; qvm.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm
  :description "An implementation of the Quantum Abstract Machine."
  :author "Robert Smith <robert@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :defsystem-depends-on (#:cffi-grovel)
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
               (:version #:magicl "0.6.1")
               ;; weak hash tables
               #:trivial-garbage
               ;; static globals
               #:global-vars
               ;; C foreign function interface
               #:cffi
               ;; Allocation of C vectors
               (:version #:static-vectors "1.8.3")
               ;; Finalizers and portable GC calls
               #:trivial-garbage
               ;; Quil parsing and analysis
               (:version #:cl-quil "1.10.0")
               ;; Portable random number generator
               #:mt19937
               ;; For allocation info.
               #+sbcl #:sb-introspect
               ;; Portable *features*
               #:trivial-features
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
               (:file "impl/sbcl-intrinsics" :if-feature (:and :sbcl :qvm-intrinsics))
               (:file "impl/sbcl-avx-vops" :if-feature (:and :sbcl :qvm-intrinsics :avx2))
               (:file "impl/sbcl-x86-vops" :if-feature (:and :sbcl :qvm-intrinsics))
               (:file "impl/linear-algebra-intrinsics" :if-feature :qvm-intrinsics)
               (:file "impl/prefetch-intrinsics" :if-feature :qvm-intrinsics)
               (:file "impl/lispworks" :if-feature :lispworks)
               (:file "utilities")
               (:file "floats")
               (:file "allocator")
               (:file "shm" :if-feature :unix)
               (:file "linear-algebra")
               (:file "qam")
               (:file "classical-memory")
               (:file "classical-memory-mixin")
               (:file "wavefunction")
               (:file "subsystem")
               (:file "qvm")
               (:file "compile-gate")
               (:file "apply-gate")
               (:file "measurement")
               (:file "transition")
               (:file "transition-classical-instructions")
               (:file "depolarizing-noise")
               (:file "noisy-qvm")
               (:file "density-qvm")
               (:file "stabilizer-qvm")
               (:file "execution")
               (:file "path-simulate")
               (:file "misc")))
