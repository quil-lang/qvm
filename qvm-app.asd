;;;; qvm-app.asd
;;;;
;;;; Author: Robert Smith

;;; FIXME: Do this properly.
(require :sb-posix)
(sb-posix:setenv "CC" "/usr/bin/cc" 1)

(asdf:defsystem #:qvm-app
  :description "Application server for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :depends-on (
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
