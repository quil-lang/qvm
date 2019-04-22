;;;; app/src/persistent.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

;;; This file keeps track of persistently running QVMs.

(defstruct persistent-record
  ;; The client IP that allocated this record.
  (client-ip nil :type string :read-only t)
  ;; The method of simulation of this QVM.
  (simulation-method nil :type simulation-method :read-only t)
  ;; The kind of allocaton.
  (allocation nil :read-only t)
  (qvm nil :read-only nil))

(global-vars:define-global-var **persistent-qvms**
  (make-hash-table :test 'equal :synchronized t)
  "A table of persistent QVMs, mapping a string key to a persistent record.")

(defun generate-persistent-qvm-key ()
  "Generate a key for a persistent QVM."
  (unicly:uuid-princ-to-string (unicly:make-v4-uuid)))

(defun lookup-persistent-qvm (key)
  "Lookup the persistent record corresponding to the key KEY."
  (values (gethash key **persistent-qvms**)))

(defun (setf lookup-persistent-qvm) (new-value key)
  (setf (gethash key **persistent-qvms**) new-value))

(defun lookup-persistent-qvm-for-ip (key ip)
  "Lookup the persistent record for the key KEY coming from IP address IP."
  (let ((record (lookup-persistent-qvm key)))
    (cond
      ((and (not (null record))
            (string= ip (persistent-record-client-ip record)))
       record)
      (t
       (error "Key ~S does not designate a record for client ~S" key ip)))))

(defun lookup-persistent-keys-for-client (client-addr)
  "Return a list of persistent keys for the client with address CLIENT-ADDR."
  (loop :for key :being :the :hash-keys :of **persistent-qvms**
          :using (hash-value record)
        :when (string= client-addr (persistent-record-client-ip record))
          :collect key))
