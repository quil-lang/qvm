;;;; qvm.asd
;;;;
;;;; Author: Robert Smith

#+(and sbcl x86-64)
#.(when (ignore-errors (sb-alien:extern-alien "avx2_supported" sb-alien:int))
    (cl:push :qvm-avx2 cl:*features*)
    (values))

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
               #:org.tfeb.hax.abstract-classes
               ;; IEEE-754 float parsing
               #:ieee-floats
               ;; Parallelization utilities
               #:lparallel
               ;; Matrix algebra
               (:version #:magicl/core "0.9.0")
               #:magicl/ext-lapack
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
               (:version #:cl-quil/frontend "1.26.0")
               ;; Portable random number generator
               #:mt19937
               ;; For allocation info.
               #+sbcl #:sb-introspect
               ;; Portable *features*
               #:trivial-features)
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
               (:file "impl/allegro" :if-feature :allegro)
               (:file "impl/clozure" :if-feature :clozure)
               (:file "impl/sbcl" :if-feature :sbcl)
               (:file "impl/sbcl-intrinsics" :if-feature (:and :sbcl :qvm-intrinsics))
               (:file "impl/sbcl-avx-vops"
                :if-feature (:and :sbcl :qvm-intrinsics :qvm-avx2))
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
               (:file "serial-kernels")
               (:file "wavefunction")
               (:file "subsystem")
               (:file "state-representation")
               (:file "qvm")
               (:file "compile-gate")
               (:file "mixed-state-qvm")
               (:file "apply-gate")
               (:file "measurement")
               (:file "transition")
               (:file "transition-classical-instructions")
               (:file "stabilizer-qvm")
               (:file "execution")
               (:file "path-simulate")
               (:file "misc")
               (:file "noise-models")
               (:file "channel-qvm")
               (:file "basic-noise-qvm")
               (:file "density-qvm")
               (:file "noisy-qvm")
               (:file "depolarizing-noise")
               (:file "unitary-qvm")
               (:module "error"
                :serial t
                :components ((:file "package")
                             (:file "fowler-noise")
                             (:file "error-qvm")))))
