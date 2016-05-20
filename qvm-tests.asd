;;;; qvm-tests.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-tests
  :description "Regression tests for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (#:qvm
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :qvm-tests
                                           '#:run-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "utilities-tests")
               (:file "measurement-tests")))
