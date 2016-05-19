;;;; qvm-app.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-app
  :description "Application server for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (
               ;; Command line argument parsing
               #:command-line-arguments
               ;; ASDF-companion utility library
               #:uiop
               ;; The QVM, of course.
               #:qvm
               )
  :pathname "app-src/"
  :serial t
  :components ((:file "package")
               (:file "entry-point")))
