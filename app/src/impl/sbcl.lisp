;;;; sbcl.lisp

(in-package #:qvm-app)

#+windows
(defun start-shm-info-server (name length)
  ;; TODO Better description?
  (error "Shared memory is not supported on Windows"))

#+unix
(defun start-shm-info-server (name length)
  "Start a thread with a socket listening on the local socket
  /tmp/<NAME>. For any incoming connection, read a single octet, then
  respond with \"<length>,<offset>\" as decimal numbers in ASCII."
  (let* ((offset (qvm::shm-vector-header-size))
         (client-buffer (make-array 1 :element-type '(unsigned-byte 8)))
         (response-buffer (babel:string-to-octets
                           (format nil "~A,~A" length offset)))
         (path (merge-pathnames name "/tmp/")))
    (let ((server-socket (make-instance 'sb-bsd-sockets:local-socket
                                        :type :stream)))
      (sb-bsd-sockets:socket-bind server-socket (namestring path))
      ;; 8 is an arbitrary backlog value
      (sb-bsd-sockets:socket-listen server-socket 8)
      (setf **shm-info-server-thread**
            (bt:make-thread
             (lambda ()
               (catch 'done
                 (unwind-protect
                      (loop
                        (let ((client-socket (sb-bsd-sockets:socket-accept server-socket)))
                          (sb-bsd-sockets:socket-receive client-socket client-buffer
                                                         (length client-buffer))
                          (sb-bsd-sockets:socket-send client-socket response-buffer
                                                      (length response-buffer))
                          (sb-bsd-sockets:socket-close client-socket)))
                   (sb-bsd-sockets:socket-close server-socket)
                   (delete-file path))))
             :name (format nil "Socket server on ~A for Shared Memory QVM" path))))
    (format-log "ATTENTION! Started shm info socket server on ~A" path)
    (values)))

(defun disable-debugger ()
  (sb-ext:disable-debugger))

(defun enable-debugger ()
  (sb-ext:enable-debugger))
