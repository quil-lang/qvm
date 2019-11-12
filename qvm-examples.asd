;;;; qvm-examples.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-examples
  :description "Examples using the QVM."
  :author "Robert Smith <robert@rigetti.com>, Juan M. Bello-Rivas <jbellorivas@rigetti.com>"
  :license "Apache License 2.0 (See LICENSE.txt)"
  :depends-on (
               (:version #:cl-quil "1.13.1")
               ;; Nelder-Mead
               #:cl-grnm
               ;; Quantum Virtual Machine
               #:qvm
               ;; Application server for the QVM
               #:qvm-app
               )
  :pathname "examples/"
  :serial t
  :components ((:file "package")
               (:file "qft")
               (:file "vqe")
               (:file "qaoa")))
