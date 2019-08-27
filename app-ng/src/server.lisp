;;;; server.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app-ng)

(alexandria:define-constant +default-server-address+ "0.0.0.0"
  :test #'string=
  :documentation "The default host address on which the HTTP server will listen.")

(alexandria:define-constant +default-server-port+ 5000
  :documentation "The default port on which the HTTP server will listen.")

(defclass rpc-acceptor (tbnl:easy-acceptor)
  ()
  (:default-initargs
   :address (error "Host address must be specified.")
   :document-root nil
   :error-template-directory nil
   :persistent-connections-p t))

(defvar *rpc-acceptor* nil)

(defun start-server-mode (&key host port)
  (check-type host string)
  ;; A PORT of 0 tells hunchentoot to pick a random port.
  (check-type port (or null (integer 0 65535)) "The port must be between 0 and 65535.")
  (format-log "Starting server on port ~D." port)
  (start-server host port)
  (loop (sleep 60)))

(defun start-server (host port &optional debug)
  (setf tbnl:*log-lisp-backtraces-p* debug
        tbnl:*log-lisp-errors-p* debug
        tbnl:*show-lisp-errors-p* debug
        tbnl:*show-lisp-backtraces-p* debug
        tbnl:*catch-errors-p* (not debug))
  (setf *rpc-acceptor* (make-instance
                        'rpc-acceptor
                        :address host
                        :port port
                        :taskmaster (make-instance 'tbnl:one-thread-per-connection-taskmaster)))
  (pushnew #'dispatch-rpc-handlers tbnl:*dispatch-table*)
  (tbnl:reset-session-secret)
  (tbnl:start *rpc-acceptor*))

(defun stop-server (&optional (acceptor *rpc-acceptor*))
  (tbnl:stop acceptor))

(defun session-info ()
  "Return a string describing the server session info."
  (if (or (not (boundp 'tbnl:*session*))
          (null tbnl:*session*))
      ""
      (format nil
              "[~A Session:~D] "
              (tbnl:session-remote-addr tbnl:*session*)
              (tbnl:session-id tbnl:*session*))))

(defun encode-json (object)
  (when (boundp 'tbnl:*reply*)
    (setf (tbnl:content-type*) "application/json; charset=utf-8"))
  (with-output-to-string (s)
    (yason:encode object s)))

(defun error-response (message)
  (encode-json
   (alexandria:plist-hash-table
    (list "error_type" "qvm_error"
          "status" message))))

(defmethod tbnl:acceptor-status-message ((acceptor rpc-acceptor) http-status-code &key error &allow-other-keys)
  (if (eql http-status-code tbnl:+http-internal-server-error+)
      (error-response error)
      (call-next-method)))

(defmethod tbnl:acceptor-log-access ((acceptor rpc-acceptor) &key return-code)
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

(defmethod tbnl:acceptor-log-message ((acceptor rpc-acceptor) log-level format-string &rest format-arguments)
  (with-locked-log ()
    (cl-syslog:format-log *logger* ':err
                          "[~A~@[ [~A]~]] ~?~%"
                          (tbnl::iso-time) log-level
                          format-string format-arguments)))

(defvar *request-json* nil)

(defun json-parameter (parameter-name &optional (request-json *request-json*))
  (gethash parameter-name request-json))

(defun parse-request-json-or-lose (request)
  (let ((json (ignore-errors (yason:parse (tbnl:raw-post-data :request request :force-text t)))))
    (unless (hash-table-p json)
      (rpc-bad-request-error "Failed to parse JSON object from request body"))
    json))

(defmethod tbnl:acceptor-dispatch-request ((acceptor rpc-acceptor) request)
  (handler-case
      (let ((*request-json* (parse-request-json-or-lose request)))
        (call-next-method))
    (rpc-error (c)
      (setf (tbnl:return-code*) (rpc-error-http-status c))
      (tbnl:abort-request-handler (error-response (princ-to-string c))))))
