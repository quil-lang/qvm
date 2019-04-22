;;;; persist.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun |GET-persist| (request)
  ;; Return an array of all of the client's valid persistent keys.
  (yason:with-output-to-string* ()
    (yason:with-array ()
      (mapc #'yason:encode-array-element
            (lookup-persistent-keys-for-client
             (tbnl:real-remote-addr request))))))

(defun |POST-persist| (request)
  ;; need num-qubits, allocation, qvm-type
  (let ((new-key (generate-persistent-qvm-key)))
    (setf (lookup-persistent-qvm new-key)
          (make-persistent-record
           :client-ip (tbnl:real-remote-addr request)
           :simulation-method 'pure-state
           :allocation **default-allocation**
           :finalizer (constantly nil)
           :qvm nil))
    (yason-encode-to-string new-key)))

(defun |DELETE-persist| (request args)
  ;; Delete a client's ID.
  (let* ((key (cdr (assoc "id" args :test #'string=)))
         (client-ip (tbnl:real-remote-addr request))
         (record (lookup-persistent-qvm key)))
    (cond
      ((and (not (null record))
            (string= client-ip (persistent-record-client-ip record)))
       (remhash key **persistent-qvms**)
       ;; return the key
       (yason-encode-to-string key))
      (t
       ;; TODO: Properly set return code of the reply instead of
       ;; relying on ERROR.
       (error "id ~S not found for client ~S" key client-ip)))))
