;;;; common.lisp

(in-package #:dqvm.common)

;;; This file contains definitions common to both the master and the
;;; worker.

;;; The rank of the running node.
(define-symbol-macro +my-rank+ (mpi:mpi-comm-rank))

;;; The number of worker nodes.
(define-symbol-macro +worker-count+ (1- (mpi:mpi-comm-size)))

(defconstant +master-rank+ 0
  "The rank of the master node.")

(defun master-node-p ()
  "Are we the master node?"
  (= +master-rank+ +my-rank+))

(defun worker-node-p ()
  "Are we a worker node?"
  (not (master-node-p)))

(defun power-of-two-p (x)
  "Is X a power of two?"
  (check-type x unsigned-byte)
  (zerop (logand x (1- x))))

(defmacro with-total-readability (&body body)
  "Print something with as many readability settings turned on."
  `(with-standard-io-syntax
     (let ((*print-readably* t)
           (*print-pretty* nil)
           (*package* (find-package "KEYWORD")))
       ,@body)))

(defmacro with-sane-read-settings (&body body)
  `(with-standard-io-syntax
     (let ((*read-eval* nil)
           (*package* (find-package ':keyword)))
       ,@body)))

(defstruct (cluster (:conc-name nil)
                    (:predicate clusterp))
  "Representation of global cluster information. See the variable *CLUSTER*."
  (qubit-count         0   :read-only t :type (and fixnum unsigned-byte))
  (ordering            nil :type list)
  (current-instruction nil :type (or null string))
  (operating-qubits    nil :type list))

(sb-ext:defglobal **cluster** nil
  "The global cluster. The master is responsible for synchronizing everybody on this information.")

(defun serialize-cluster (c)
  (check-type c cluster)
  (with-total-readability
    (prin1-to-string c)))

(defun deserialize-cluster (string)
  (check-type string string)
  (let ((*read-eval* nil))
    (let ((c (with-sane-read-settings
               (read-from-string string))))
      (unless (typep c 'cluster)
        (error "Received a string that didn't deserialize into a cluster. Got: ~S"
               c))
      c)))

(defun instruction->string (isn)
  (with-output-to-string (s)
    (quil:print-instruction isn s)))

(defun string->instruction (string)
  (aref (quil:parsed-program-executable-code (quil:parse-quil-string string)) 0))

(let ((lock (bt:make-lock "Printer Lock")))
  (defun format-locked (format-string &rest format-args)
    "A thread-safe version of FORMAT."
    (bt:with-lock-held (lock)
      (format t "[#~D] " +my-rank+)
      (apply #'format t format-string format-args)
      (finish-output))))

(defun call-with-probed-size (f &key (source mpi:+mpi-any-source+)
                                     (tag mpi:+mpi-any-tag+)
                                     (comm mpi:*standard-communicator*))
  "Probe for the number of bytes in the message queued, along with who is sending it and which tag it comes with. Call the function F with these three values."
  (multiple-value-bind (num-bytes sender-rank sent-tag)
      (mpi:mpi-probe source :tag tag :comm comm)
    ;; sanity check we probed who we wanted to probe
    (assert (or (eql source mpi:+mpi-any-source+)
                (eql sender-rank source)))
    (funcall f num-bytes sender-rank sent-tag)))

(defun receive-string (&key (source mpi:+mpi-any-source+)
                            (tag mpi:+mpi-any-tag+)
                            (comm mpi:*standard-communicator*))
  "Receive a string of indeterminate size and return it, along with the rank of the node who sent it."
  (flet ((process-string (num-bytes sender-rank sent-tag)
           (declare (ignore sent-tag))
           (static-vectors:with-static-vector (v num-bytes :initial-element 0)
             (mpi:mpi-recv v sender-rank :comm comm :tag tag)
             (values
              (map 'simple-string #'code-char v)
              sender-rank))))
    (call-with-probed-size #'process-string :source source :tag tag :comm comm)))

(defun everybody-synchronize (for-what)
  "A thin wrapper around MPI-BARRIER, but more descriptive."
  (check-type for-what string)
  (format-locked "BARRIER: ~A~%" for-what)
  (mpi:mpi-barrier)
  (values))

(defmacro with-errors-printed-verbosely (&body body)
  `(handler-bind ((error (lambda (c)
                           (dissect:present c)
                           (error c))))
    ,@body))

(defmacro with-raw-vector ((vec ptr array) &body body)
  (alexandria:with-gensyms (raw-vec)
    (alexandria:once-only (array)
      `(let ((,raw-vec (sb-ext:array-storage-vector ,array)))
         (sb-sys:with-pinned-objects (,raw-vec)
           (let ((,ptr (static-vectors:static-vector-pointer ,raw-vec))
                 (,vec ,raw-vec))
             ;; Allow VEC to be IGNOREd.
             ,@body))))))
