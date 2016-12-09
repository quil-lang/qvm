;;;; quil-basic.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:quil-basic
  :description "Quil BASIC interpreter loop."
  :author "Robert Smith <robert@rigetti.com>"
  :version (:read-file-form "VERSION.txt")
  :depends-on (
               #:qvm
               #:cl-quil
               #:uiop
               )
  :serial t
  :components ((:file "quil-basic")))
