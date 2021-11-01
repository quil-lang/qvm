;;;; qvm-app.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:qvm-app
  :description "Application server for the QVM."
  :author "Robert Smith <robert@rigetti.com>"
  :license "GNU Affero General Public License v3.0 (See app/LICENSE.txt)"
  :version (:read-file-form "VERSION.txt")
  :depends-on (
               ;; Quil parsing
               (:version #:cl-quil/frontend "1.26.0")
               ;; Command line argument parsing
               #:command-line-arguments
               ;; ASDF-companion utility library
               #:uiop
               ;; JSON parsing
               #:yason
               ;; The QVM, of course.
               #:qvm
               #:qvm-benchmarks
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
               ;; Portable threading
               #:bordeaux-threads
               ;; Portable gc
               #:trivial-garbage
               ;; Portable globals
               #:global-vars
               ;; Logging
               #:cl-syslog
               ;; HTTP requests for version info
               #:drakma
               ;; Portable *features*
               #:trivial-features
               ;; Regular expressions
               #:cl-ppcre)
  :in-order-to ((asdf:test-op (asdf:test-op #:qvm-app-tests)))
  :pathname "app/src/"
  :serial t
  :entry-point "qvm-app::asdf-entry-point"
  :components ((:file "package")
               (:file "globals")
               (:file "utilities")
               (:file "qvm-app-version")
               (:file "shm-info-server")
               (:file "impl/sbcl" :if-feature :sbcl)
               (:file "impl/clozure" :if-feature :clozure)
               (:file "configure-qvm")
               (:module "api"
                :serial t
                :components ((:file "common")
                             (:file "ping")
                             (:file "version")
                             (:file "info")
                             (:file "multishot")
                             (:file "multishot-measure")
                             (:file "expectation")
                             (:file "wavefunction")
                             (:file "probabilities")
                             (:file "run-for-effect")))
               (:file "benchmark-programs")
               (:file "server-abstraction")
               (:file "handle-request")
               (:file "debugger")
               (:file "entry-point")))
