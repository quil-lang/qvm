;;;; qvm-tests.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-tests
  :description "Regression tests for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil
               #:qvm
               #:qvm-examples
               #:alexandria
               #:fiasco
               #:trivial-garbage
               #:cffi)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :qvm-tests
                                           '#:run-qvm-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "utilities")
               (:file "utilities-tests")
               (:file "linear-algebra-tests")
               (:file "classical-memory-tests")
               (:file "wavefunction-tests")
               (:file "qvm-tests")
               (:file "measurement-tests")
               (:file "gate-tests")
               (:file "instruction-tests")
               (:file "modifier-tests")
               (:file "noisy-qvm-tests")
               (:file "density-qvm-tests")
               (:file "stress-tests")
               (:file "path-simulate-tests")))
