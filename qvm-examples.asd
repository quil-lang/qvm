;;;; qvm-examples.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-examples
  :description "Examples using the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (#:qvm)
  :pathname "examples/"
  :serial t
  :components ((:file "package")
               (:file "qft")))
