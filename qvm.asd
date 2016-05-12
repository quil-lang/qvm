;;;; qvm.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm
  :description "An implementation of the Quantum Abstract Machine."
  :author "Robert Smith"
  :depends-on (#:alexandria #:command-line-arguments #:ieee-floats)
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
