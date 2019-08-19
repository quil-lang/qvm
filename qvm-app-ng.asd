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
               #:cl-syslog
               #:command-line-arguments
               #:drakma
               #:global-vars
               #:hunchentoot
               #:uiop
               #:swank
               #:trivial-features)
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-app-ng-tests)))
  :pathname "app-ng/src/"
  :serial t
  :entry-point "qvm-app-ng::asdf-entry-point"
  :components ((:file "package")
               (:file "globals")
               (:file "utilities")
               (:file "qvm-app-ng-version")
               (:file "configure-qvm")
               (:module "api"
                :serial t
                :components ((:file "common")
                             (:file "multishot")
                             (:file "persistent-qvm")
                             (:file "trivial-handlers")))
               (:file "server")
               (:file "handle-request")
               (:file "impl/sbcl" :if-feature :sbcl)
               (:file "impl/clozure" :if-feature :clozure)
               (:file "entry-point")))
