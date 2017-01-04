;;;; qvm-app.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-app
  :description "Application server for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :version (:read-file-form "APP-VERSION.txt")
  :depends-on (
               ;; Quil parsing
               (:version #:cl-quil "0.5.1")
               ;; Command line argument parsing
               #:command-line-arguments
               ;; ASDF-companion utility library
               #:uiop
               ;; JSON parsing
               #:yason
               ;; The QVM, of course.
               #:qvm
               ;; Float encoding
               #:ieee-floats
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
  :pathname "src-app/"
  :serial t
  :components ((:file "package")
               (:file "db")
               (:file "entry-point")))
