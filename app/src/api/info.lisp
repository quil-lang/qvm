;;;; api/info.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:qvm-app)


(defun handle-info ()
  "Return a string representation of a JSON object whose key-value
pairs correspond to QVM-APP variables and their values at the time of
the INFO request."
  (let ((info
          (alexandria:plist-hash-table
           (list "simulation-method" (princ-to-string *simulation-method*)
                 "shared-memory-object-name" (princ-to-string *shared-memory-object-name*)
                 "qubit-limit" (princ-to-string *qubit-limit*))
           :test 'equal)))
    (with-output-to-string (s)
      (yason:encode info s))))
