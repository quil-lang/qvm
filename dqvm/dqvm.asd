;;;; dqvm.asd

(asdf:defsystem #:dqvm
  :description "The QVM except it's distributed."
  :author "Robert Smith <robert@rigetti.com>, Lauren Capelluto <lauren@rigetti.com>"
  :depends-on (
               #:qvm
               #:cl-mpi
               #:bordeaux-threads
               #:sb-concurrency
               #:command-line-arguments
               #:dissect
               )
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "master")
               (:file "worker")
               (:file "main")))
