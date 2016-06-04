;;;; qvm.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm
  :description "An implementation of the Quantum Abstract Machine."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (
               ;; General utilities
               #:alexandria
               ;; IEEE-754 float parsing
               #:ieee-floats
               ;; Quil parsing
               #:cl-quil
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-tests)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "qvm")
               (:file "measurement")
               (:file "quil")
               (:file "execution")
               (:file "gates")))
