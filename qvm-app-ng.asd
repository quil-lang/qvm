;;;; qvm-app-ng.asd
;;;;
;;;; Author: appleby

(asdf:defsystem #:qvm-app-ng
  :description "Application server for the QVM Next Gen."
  :author "Mike Appleby <mappleby@rigetti.com>"
  :license "GNU Affero General Public License v3.0 (See app-ng/LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :depends-on (
               ;; Quil parsing
               (:version #:cl-quil "1.10.4")
               ;; Command line argument parsing
               #:command-line-arguments
               ;; ASDF-companion utility library
               #:uiop
               ;; The QVM, of course.
               #:qvm
               ;; Utilities
               #:alexandria
               ;; Remote Lisp connection
               #:swank
               ;; Portable globals
               #:global-vars
               ;; Logging
               #:cl-syslog
               ;; HTTP requests for version info
               #:drakma
               ;; Portable *features*
               #:trivial-features)
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-app-ng-tests)))
  :pathname "app-ng/src/"
  :serial t
  :entry-point "qvm-app-ng::asdf-entry-point"
  :components ((:file "package")
               (:file "globals")
               (:file "utilities")
               (:file "qvm-app-ng-version")
               (:file "impl/sbcl" :if-feature :sbcl)
               (:file "impl/clozure" :if-feature :clozure)
               (:file "entry-point")))
