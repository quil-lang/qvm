;;;; qvm-app-tests.asd
;;;;
;;;; Author: Nik T

(asdf:defsystem #:qvm-app-tests
  :description "Test Suite for Application server for the QVM."
  :author "Nikolas Tezak <nikolas@rigetti.com>"
  :license "GNU Affero General Public License v3.0 (See app/LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :depends-on (
               #:qvm-app
               #:uiop
               #:fiasco
               )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':qvm-app-tests
                                           '#:run-qvm-app-tests))
  :pathname "app/tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")))
