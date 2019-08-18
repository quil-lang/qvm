;;;; server.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(deftype http-status-code () `(integer 100 599))

(alexandria:define-constant +default-server-address+ "0.0.0.0"
  :test #'string=
  :documentation "The default host address on which the HTTP server will listen.")

(alexandria:define-constant +default-server-port+ 5000
  :documentation "The default port on which the HTTP server will listen.")

(defun start-server-mode (&key host port)
  (check-type host string)
  ;; A PORT of 0 tells hunchentoot to pick a random port.
  (check-type port (or null (integer 0 65535)) "The port must be between 0 and 65535.")
  (format-log "Starting server on port ~D." port)
  (unless (null *qubit-limit*)
    (format-log "Server is limited to ~D qubit~:P." *qubit-limit*))
  (start-server host port)
  ;; We could join the main thread, instead of spinning in a loop, like so:
  ;;
  ;; (bt:join (tbnl::acceptor-process (tbnl::acceptor-taskmaster *app*)))
  ;;
  ;; But those hunchentoot APIs are not exported, so it seems ill-advised for such a small win. As
  ;; far as I know, it's not even guaranteed that hunchentoot uses bordeaux-threads for the
  ;; MULTI-THREADED-TASKMASTER.
  (loop (sleep 60)))

(defun start-server (host port)
  #+forest-sdk
  (setq tbnl:*log-lisp-backtraces-p* nil
        tbnl:*log-lisp-errors-p* nil)
  (tbnl:reset-session-secret)

  (setq tbnl:*show-lisp-errors-p* *debug*
        tbnl:*show-lisp-backtraces-p* *debug*
        tbnl:*catch-errors-p* (not *debug*))
  (setf *app* (make-instance
               'vhost
               :address host
               :port port
               :taskmaster (make-instance 'tbnl:one-thread-per-connection-taskmaster)
               :dispatch-table (mapcar (alexandria:curry #'apply #'create-prefix/method-dispatcher)
                                       '(("/" :POST handle-post-request)))))
  (tbnl:start *app*))

(defclass vhost (tbnl:acceptor)
  ((dispatch-table
    :initform '()
    :initarg :dispatch-table
    :accessor dispatch-table
    :documentation "List of dispatch functions"))
  (:default-initargs
   :address (error "Host address must be specified.")
   :document-root nil
   :error-template-directory nil
   :persistent-connections-p t))

(defmethod tbnl:acceptor-status-message ((acceptor vhost) http-status-code &key error &allow-other-keys)
  (if (eql http-status-code tbnl:+http-internal-server-error+)
      (with-output-to-string (s)
        (setf (tbnl:content-type*) "application/json; charset=utf-8")
        (yason:encode
         (alexandria:plist-hash-table
          (list "error_type" "qvm_error"
                "status" error))
         s))
      (call-next-method)))

(defmethod tbnl:acceptor-log-access ((acceptor vhost) &key return-code)
  (with-locked-log ()
    (cl-syslog:format-log *logger* ':info
                          "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                          ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
                          (tbnl::remote-addr*)
                          (tbnl::header-in* :x-forwarded-for)
                          (tbnl::authorization)
                          (tbnl::iso-time)
                          (tbnl::request-method*)
                          (tbnl::script-name*)
                          (tbnl::query-string*)
                          (tbnl::server-protocol*)
                          return-code
                          (tbnl::content-length*)
                          (tbnl::referer)
                          (tbnl::user-agent))))

(defmethod tbnl:acceptor-log-message ((acceptor vhost) log-level format-string &rest format-arguments)
  (with-locked-log ()
    (cl-syslog:format-log *logger* ':err
                          "[~A~@[ [~A]~]] ~?~%"
                          (tbnl::iso-time) log-level
                          format-string format-arguments)))

(defun create-prefix/method-dispatcher (prefix method handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
starts with the string PREFIX."
  (lambda (request)
    (and (eq method (tbnl:request-method request))
         (let ((mismatch (mismatch (tbnl:script-name request) prefix
                                   :test #'char=)))
           (and (or (null mismatch)
                    (>= mismatch (length prefix)))
                handler)))))

(defmethod tbnl:acceptor-dispatch-request ((vhost vhost) request)
  (mapc (lambda (dispatcher)
          (let ((handler (funcall dispatcher request)))
            (when handler
              (return-from tbnl:acceptor-dispatch-request (funcall handler request)))))
        (dispatch-table vhost))
  (call-next-method))

(defun session-info ()
  "Return a string summary of TBNL:*SESSION*.

If TBNL:*SESSION* is unbound or NULL, return the empty string."
  (if (or (not (boundp 'tbnl:*session*))
          (null tbnl:*session*))
      ""
      (format nil
              "[~A Session:~D] "
              (tbnl:session-remote-addr tbnl:*session*)
              (tbnl:session-id tbnl:*session*))))
