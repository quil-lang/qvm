;;;; qvm-qaas-tests.asd
;;;;
;;;; Author: appleby

(asdf:defsystem #:qvm-qaas-tests
  :description "Test Suite for Application server for the QVM."
  :author "Mike Appleby <mappleby@rigetti.com>"
  :license "GNU Affero General Public License v3.0 (See app/LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :depends-on (
               #:qvm-qaas
               #:uiop
               #:fiasco
               )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':qvm-qaas-tests
                                           '#:run-qvm-qaas-tests))
  :pathname "qaas-app/tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")))
