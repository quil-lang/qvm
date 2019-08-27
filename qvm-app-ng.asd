;;;; qvm-app-ng.asd
;;;;
;;;; Author: appleby

(asdf:defsystem #:qvm-app-ng
  :description "Application server for the QVM Next Gen."
  :author "Mike Appleby <mappleby@rigetti.com>"
  :license "GNU Affero General Public License v3.0 (See app-ng/LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :depends-on ((:version #:cl-quil "1.10.4")
               #:qvm
               #:alexandria
               #:bordeaux-threads
               #:cl-syslog
               #:command-line-arguments
               #:global-vars
               #:hunchentoot
               #:trivial-features
               #:uiop
               #:uuid
               #:yason)
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-app-ng-tests)))
  :pathname "app-ng/src/"
  :serial t
  :entry-point "qvm-app-ng::asdf-entry-point"
  :components ((:file "package")
               (:file "globals")
               (:file "utilities")
               (:file "logging")
               (:file "errors")
               (:file "validators")
               (:file "make-qvm")
               (:file "persistent-qvm")
               (:file "handlers")
               (:file "server")
               (:file "qvm-app-ng-version")
               (:file "impl/sbcl" :if-feature :sbcl)
               (:file "entry-point")))
