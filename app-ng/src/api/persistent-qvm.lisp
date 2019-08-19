;;;; api/persistent-qvm.lisp
;;;;
;;;; Author: appleby

(in-package :qvm-app-ng)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-empty-persistent-qvms-db ()
    (make-hash-table)))

(defun reset-persistent-qvms-db ()
  (setf **persistent-qvms** (make-empty-persistent-qvms-db)))

(global-vars:define-global-var **persistent-qvms** (make-empty-persistent-qvms-db)
  "The database of persistent QVMs. The keys are integers and the values are lists of (QVM LOCK METADATA) triples.")

(global-vars:define-global-var **persistent-qvms-lock** (bt:make-lock "Persistent QVMs DB Lock"))

(defmacro with-persistent-qvm ((qvm &optional metadata) token &body body)
  (check-type qvm symbol)
  (check-type metadata (or null symbol))
  (when (null metadata)
    (setf metadata (gensym "metadata")))
  (alexandria:with-gensyms (lock)
    (alexandria:once-only (token)
      `(destructuring-bind (,qvm ,lock ,metadata) (%lookup-persistent-qvm-or-lose ,token)
         (declare (ignorable ,qvm))
         (bt:with-lock-held (,lock)
           (cond ((%marked-for-deletion-p ,metadata)
                  (error "Persistent QVM ~A is marked for deletion." ,token))
                 (t ,@body)))))))

(defun %insert-persistent-qvm-locked (token persistent-qvm)
  (setf (gethash token **persistent-qvms**) persistent-qvm))

(defun %delete-persistent-qvm-locked (token)
  (remhash token **persistent-qvms**))

(defun %delete-persistent-qvm (token)
  (bt:with-lock-held (**persistent-qvms-lock**)
    (remhash token **persistent-qvms**)))

(defun %lookup-persistent-qvm-locked (token)
  (gethash token **persistent-qvms**))

(defun %lookup-persistent-qvm (token)
  (bt:with-lock-held (**persistent-qvms-lock**)
    (%lookup-persistent-qvm-locked token)))

(defun %lookup-persistent-qvm-or-lose (token)
  (or (%lookup-persistent-qvm token)
      (error "Failed to find persistent QVM ~D" token)))

(defun %marked-for-deletion-p (metadata)
  (gethash "delete-pending" metadata))

(defun %mark-for-deletion (metadata)
  (setf (gethash "delete-pending" metadata) t))

(defun %persistent-qvm-token (persistent-qvm)
  ;; TODO:(appleby) maybe just use an atomic incrementing counter or UUID here? Hashing on the QVM
  ;; identity appears to be incompatible with the desire to support reconfiguration.
  (sxhash persistent-qvm))

(defun make-persistent-qvm (simulation-method quil num-qubits gate-noise measurement-noise)
  (list (make-appropriate-qvm simulation-method quil num-qubits gate-noise measurement-noise)
        (bt:make-lock (format nil "PQVM Lock"))
        (make-hash-table :test 'equal)))

(defun allocate-persistent-qvm (simulation-method quil num-qubits gate-noise measurement-noise)
  (let* ((persistent-qvm (make-persistent-qvm simulation-method
                                              quil
                                              num-qubits
                                              gate-noise
                                              measurement-noise))
         (token (%persistent-qvm-token persistent-qvm)))
    (bt:with-lock-held (**persistent-qvms-lock**)
      (cond ((not (null (%lookup-persistent-qvm-locked token)))
             (error "Token collision while attempting to allocate persistent QVM"))
            (t (%insert-persistent-qvm-locked token persistent-qvm)
               (values token persistent-qvm))))))

(defun handle-make-persistent-qvm (simulation-method quil num-qubits &key gate-noise measurement-noise)
  (let ((result (make-hash-table :test 'equal)))
    (setf (gethash "token" result)
          (allocate-persistent-qvm simulation-method quil num-qubits gate-noise measurement-noise))
    result))

(defun handle-delete-persistent-qvm (token)
  (with-persistent-qvm (qvm metadata) token
    (%mark-for-deletion metadata))
  ;; TODO:(appleby) Need to do anything special to de-allocate?
  (%delete-persistent-qvm token)
  (format nil "Deleted persistent QVM ~D" token))

(defun handle-persistent-qvm-info (token)
  (with-output-to-string (s)
    (yason:encode (with-persistent-qvm (qvm metadata) token
                    (alexandria:plist-hash-table
                     `("qvm_type" ,(symbol-name (type-of qvm))
                       "number_of_qubits" ,(qvm:number-of-qubits qvm)
                       "metadata" ,metadata)))
                  s)))
