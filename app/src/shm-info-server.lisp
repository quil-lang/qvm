;;;; shm-info-server.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(global-vars:define-global-var  **shm-info-server-thread** nil
  "Thread running the SHM info server.")

(defun stop-shm-info-server ()
  (when (and **shm-info-server-thread**
             (bt:thread-alive-p **shm-info-server-thread**))
    (bt:interrupt-thread **shm-info-server-thread**
                         (lambda ()
                           (throw 'done t)))))

;;; The rest of the logic is in the `impl` directory.
