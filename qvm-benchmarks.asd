;;;; qvm-benchmarks.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-benchmarks
  :description "Performance tests for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (#:cl-quil
               #:qvm
               #:trivial-benchmark)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :qvm-benchmarks
                                           '#:run-benchmarks))
  :pathname "bench/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "quil-files")))
