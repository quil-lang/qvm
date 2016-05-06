;;;; qvm.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm
  :description "An implementation of the Quantum Abstract Machine."
  :author "Robert Smith"
  :depends-on (#:command-line-arguments)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "qvm")
               (:file "measurement")
               (:file "execution")
               (:file "gates")
               (:file "qft")

               ;; For executable creation.
               (:file "entry-point")))
