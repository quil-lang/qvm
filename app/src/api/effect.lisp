;;;; api/effect.lisp
;;;;
;;;; Author: Erik Davis
;;;;         Robert Smith

(in-package #:qvm-app)

(defun |POST-effect/id| (request args)
  (let ((key (cdr (assoc "id" args :test #'string=)))
        (js (extract-json-payload request)))
    (check-for-quil-instrs-field js)
    (let* ((quil (let ((quil:*allow-unresolved-applications* t))
                   (safely-parse-quil-string (get-quil-instrs-field js))))
           (num-qubits (cl-quil:qubits-needed quil))
           (qvm (lookup-persistent-qvm-for-ip key (tbnl:real-remote-addr request))))
      (overwrite-execution-parameters-according-to-program qvm quil)
      (%execute qvm)
      (load-time-value
       (yason-encode-to-string t)))))
