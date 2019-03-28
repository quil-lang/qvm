;;;; clozure.lisp

(in-package #:qvm-app)

(defun zap-info ()
  ;; No-op on Clozure
  )

(defun start-shm-info-server (name length)
  "Start a thread with a socket listening on the local socket
  /tmp/<NAME>. For any incoming connection, read a single octet, then
  respond with \"<length>,<offset>\" as decimal numbers in ASCII."
  (let* ((offset (qvm::shm-vector-header-size))
         (client-buffer (make-array 1 :element-type '(unsigned-byte 8)))
         (response-buffer (babel:string-to-octets
                           (format nil "~A,~A" length offset)))
         (path (merge-pathnames name "/tmp/")))
    (let ((server-socket (ccl:make-socket :address-family ':file
                                          :local-filename (namestring path)
                                          :type ':stream
                                          :connect ':passive)))
      (setf **shm-info-server-thread**
            (bt:make-thread
             (lambda ()
               (catch 'done
                 (unwind-protect
                      (loop
                        (let ((client-socket (ccl:accept-connection server-socket)))
                          (read-sequence client-buffer client-socket)
                          (write-sequence response-buffer client-socket)
                          (close client-socket)))
                   (close server-socket)
                   (delete-file path))))
             :name (format nil "Socket server on ~A for Shared Memory QVM" path))))
    (format-log "ATTENTION! Started shm info socket server on ~A" path)
    (values)))

(defun disable-debugger ()
  (setf ccl::*batch-flag* t))

(defun enable-debugger ()
  (setf ccl::*batch-flag* nil))
