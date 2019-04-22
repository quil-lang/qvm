;;;; api/info.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:qvm-app)

(defun handle-info ()
  "Return a string representation of a JSON object whose key-value
pairs correspond to QVM-APP variables and their values at the time of
the INFO request."
  (yason-encode-to-string
   (alexandria:plist-hash-table
    (list "qubit-limit" (princ-to-string *qubit-limit*))
    :test 'equal)))
