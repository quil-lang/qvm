;;;; qvm.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm
  :description "An implementation of the Quantum Abstract Machine."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (
               ;; General utilities.
               #:alexandria
               ;; Command line argument parsing
               #:command-line-arguments
               ;; IEEE-754 float parsing
               #:ieee-floats
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-tests)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "qvm")
               (:file "measurement")
               (:file "qil")
               (:file "execution")
               (:file "gates")
               (:file "qft")

               ;; For executable creation.
               (:file "entry-point")))
