;;;; dqvm2-tests.asd
;;;;
;;;; Author: Juan M. Bello-Rivas

(asdf:defsystem #:dqvm2-tests
  :description "Test suite for dqvm2."
  :author "Juan M. Bello-Rivas <jbellorivas@rigetti.com>"
  :depends-on (#:dqvm2
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :dqvm2-tests
                                           '#:run-dqvm2-tests))
  :pathname "tests/"
  :components ((:file "package")
               (:file "suite")
               (:file "permutation-tests")
               (:file "addresses-tests")
               (:file "distributed-qvm-tests")
               (:file "program-tests")))
