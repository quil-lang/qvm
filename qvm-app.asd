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
               ;; Utilities
               #:alexandria
               ;; CL-FAD
               #:cl-fad
               ;; Redis integration
               #:cl-redis
               #:usocket
               ;; Remote Lisp connection
               #:swank
               )
  :pathname "app-src/"
  :serial t
  :components ((:file "package")
               (:file "db")
               (:file "entry-point")))
