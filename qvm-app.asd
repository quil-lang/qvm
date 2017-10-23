;;;; qvm-app.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-app
  :description "Application server for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :version (:read-file-form "APP-VERSION.txt")
  :depends-on (
               ;; QFT
               #:qvm-examples
               ;; Quil parsing
               (:version #:cl-quil "0.7.0")
               ;; Command line argument parsing
               #:command-line-arguments
               ;; ASDF-companion utility library
               #:uiop
               ;; JSON parsing
               #:yason
               ;; The QVM, of course.
               (:version #:qvm "0.11.0")
               ;; Float encoding
               #:ieee-floats
               ;; HTTP web server
               #:hunchentoot
               ;; Utilities
               #:alexandria
               ;; CL-FAD
               #:cl-fad
               ;; Remote Lisp connection
               #:swank
               )
  :pathname "src-app/"
  :serial t
  :components ((:file "package")
               (:file "profiled-qvm")
               (:file "entry-point")))
