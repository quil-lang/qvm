;;;; qvm-qaas.asd
;;;;
;;;; Author: appleby

(asdf:defsystem #:qvm-qaas
  :description "Application server for the QVM-as-a-service."
  :author "Mike Appleby <mappleby@rigetti.com>"
  :license "GNU Affero General Public License v3.0 (See qaas/LICENSE.txt)"
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
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-qaas-tests)))
  :pathname "qaas-app/src/"
  :serial t
  :entry-point "qvm-qaas::asdf-entry-point"
  :components ((:file "package")
               (:file "globals")
               (:file "utilities")
               (:file "qvm-qaas-version")
               (:file "impl/sbcl" :if-feature :sbcl)
               (:file "impl/clozure" :if-feature :clozure)
               (:file "entry-point")))
