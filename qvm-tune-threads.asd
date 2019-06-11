;;;; qvm-tune-threads.asd
;;;;
;;;; Author: John Lapeyre

(asdf:defsystem #:qvm-tune-threads
  :description "Tune the number of threads in the QVM."
  :author "John Lapeyre <jlapeyre@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (#:cl-quil
               #:qvm)
  :pathname "tune-threads/"
  :serial t
  :components (
               (:file "package")
               (:file "utilities")
               (:file "tests")
               (:file "tune-threads")))
