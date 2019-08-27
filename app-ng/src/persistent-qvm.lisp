;;;; api/persistent-qvm.lisp
;;;;
;;;; Author: appleby

(in-package :qvm-app-ng)

(deftype persistent-qvm-token () 'string)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-empty-persistent-qvms-db ()
    (make-hash-table :test 'equal)))

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

(defun %remove-persistent-qvm-locked (token)
  (remhash token **persistent-qvms**))

(defun %remove-persistent-qvm (token)
  (bt:with-lock-held (**persistent-qvms-lock**)
    (remhash token **persistent-qvms**)))

(defun delete-persistent-qvm (token)
  (with-persistent-qvm (qvm metadata) token
    (%mark-for-deletion metadata))
  (%remove-persistent-qvm token))

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

(defun canonicalize-persitent-qvm-token (token)
  "Canonicalize the TOKEN string into the case expected by VALID-PERSISTENT-QVM-TOKEN-P."
  ;; Standardize on the more common (and more readable) lowercase UUID string, even though
  ;; UUID:PRINT-OBJECT and UUID:PRINT-BYTES print them in uppercase.
  (string-downcase token))

(defun make-persistent-qvm-token ()
  "Return a new persitent QVM token."
  (canonicalize-persitent-qvm-token (princ-to-string (uuid:make-v4-uuid))))

(defun valid-persistent-qvm-token-p (token)
  "True if TOKEN is a valid string representation of a v4 UUID.

Note that this function requires that any hexadecimal digits in TOKEN are lowercased."
  ;; See RFC 4122 for UUID format.
  ;; https://tools.ietf.org/html/rfc4122#section-4.1
  ;;
  ;; We validate that token is a valid v4 UUID in printed string format. That is, as a string of
  ;; hexadecimal digits (with certain restrictions) separated by hyphens in the expected places.
  (and (typep token 'persistent-qvm-token)
       (= (length token) 36)
       (eq (aref token  8) #\-)
       (eq (aref token 13) #\-)
       (eq (aref token 14) #\4) ; version
       (eq (aref token 18) #\-)
       ;; https://tools.ietf.org/html/rfc4122#section-4.4
       ;; The two most-significant bits of the clock sequence field are 10b, meaning the
       ;; resulting hex digit of the most-significant byte is one of 8, 9, a, or b.
       (or (eq (aref token 19) #\8)
           (eq (aref token 19) #\9)
           (eq (aref token 19) #\a)
           (eq (aref token 19) #\b))
       (eq (aref token 23) #\-)
       (every #'hex-char-p (remove #\- token))))

(defun make-persistent-qvm (simulation-method num-qubits)
  (list (make-requested-qvm simulation-method num-qubits)
        (bt:make-lock (format nil "PQVM Lock"))
        (make-hash-table :test 'equal)))

(defun allocate-persistent-qvm (simulation-method num-qubits)
  (let ((persistent-qvm (make-persistent-qvm simulation-method num-qubits))
        (token (make-persistent-qvm-token)))
    (bt:with-lock-held (**persistent-qvms-lock**)
      (cond ((not (null (%lookup-persistent-qvm-locked token)))
             (error "Token collision while attempting to allocate persistent QVM"))
            (t (%insert-persistent-qvm-locked token persistent-qvm)
               (values token persistent-qvm))))))

(defun persistent-qvm-info (token)
  (alexandria:plist-hash-table
   (with-persistent-qvm (qvm metadata) token
     (list "qvm-type" (symbol-name (type-of qvm))
           "num-qubits" (qvm:number-of-qubits qvm)
           "metadata" metadata))
   :test 'equal))

(defun run-program-on-persistent-qvm (token parsed-program)
  (with-persistent-qvm (qvm) token
    (run-program-on-qvm qvm parsed-program)))
