;;;; qvm-app-ng.asd
;;;;
;;;; Author: appleby

(asdf:defsystem #:qvm-app-ng
  :description "Application server for the QVM Next Gen."
  :author "Mike Appleby <mappleby@rigetti.com>"
  :license "GNU Affero General Public License v3.0 (See app-ng/LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :depends-on ((:version #:cl-quil "1.11.1")
               #:qvm
               #:alexandria
               #:bordeaux-threads
               #:cl-algebraic-data-type
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
  :components ((:module "safety-hash"
                :serial t
                :components
                ((:file "package")
                 (:file "impl-bordeaux-threads" :if-feature (:or (:not :sbcl) :qvm-app-ng-generic-safety-hash))
                 (:file "impl-sbcl" :if-feature (:and :sbcl (:not :qvm-app-ng-generic-safety-hash)))
                 (:file "interface")))
               (:file "package")
               (:file "globals")
               (:file "utilities")
               (:file "uuid")
               (:file "logging")
               (:file "http-status")
               (:file "errors")
               (:file "validators")
               (:file "job")
               (:file "make-qvm")
               (:file "persistent-qvm")
               (:file "response")
               (:file "server")
               (:file "handlers")
               (:file "qvm-app-ng-version")
               (:file "impl/sbcl" :if-feature :sbcl)
               (:file "entry-point")))
