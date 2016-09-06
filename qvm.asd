;;;; qvm.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm
  :description "An implementation of the Quantum Abstract Machine."
  :author "Robert Smith <robert@rigetti.com>"
  :version (:read-file-form "VERSION.txt")
  :depends-on (
               ;; General utilities
               #:alexandria
               ;; IEEE-754 float parsing
               #:ieee-floats
               ;; Quil parsing
               (:version #:cl-quil "0.1.2")
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-tests)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "gates")
               (:file "quil")
               (:file "qvm")
               (:file "measurement")
               (:file "transition")
               (:file "depolarizing-noise")
               (:file "execution")))
