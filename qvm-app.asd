;;;; qvm-app.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-app
  :description "Application server for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (
               ;; Quil parsing
               #:cl-quil
               ;; Command line argument parsing
               #:command-line-arguments
               ;; ASDF-companion utility library
               #:uiop
               ;; JSON parsing
               #:yason
               ;; The QVM, of course.
               #:qvm
               ;; HTTP web server
               #:hunchentoot
               )
  :pathname "app-src/"
  :serial t
  :components ((:file "package")
               (:file "entry-point")))
