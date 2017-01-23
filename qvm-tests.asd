;;;; qvm-tests.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-tests
  :description "Regression tests for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (#:cl-quil
               #:qvm
               #:qvm-examples
               #:alexandria
               #:fiasco
               #:trivial-garbage)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :qvm-tests
                                           '#:run-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "utilities")
               (:file "utilities-tests")
               (:file "linear-algebra-tests")
               (:file "qvm-tests")
               (:file "measurement-tests")
               (:file "gate-tests")
               (:file "instruction-tests")
               (:file "noisy-qvm-tests")
               (:file "stress-tests")))
