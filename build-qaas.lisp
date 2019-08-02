;;;; build-qaas.lisp
;;;;
;;;; This file is loaded by the Makefile to produce a qaas[.exe] binary.
;;;;

(unless *load-truename*
  (error "This file is meant to be loaded."))

(pushnew :hunchentoot-no-ssl *features*)
(pushnew :drakma-no-ssl *features*)

(require 'asdf)

(let ((*default-pathname-defaults* (make-pathname :type nil
                                                  :name nil
                                                  :defaults *load-truename*))
      (output-file (make-pathname :name "qaas"
                                  :type #+win32 "exe" #-win32 nil))
      (system-table (make-hash-table :test 'equal))
      (toplevel (lambda ()
                  (with-simple-restart (abort "Abort")
                    (funcall (read-from-string "qvm-qaas::%main")
                             sb-ext:*posix-argv*)))))
  (labels ((load-systems-table ()
             (unless (probe-file "system-index.txt")
               (error "Generate system-index.txt with 'make system-index.txt' first."))
             (setf (gethash "qvm-qaas" system-table) (merge-pathnames "qvm-qaas.asd"))
             (with-open-file (stream "system-index.txt")
               (loop
                 :for system-file := (read-line stream nil)
                 :while system-file
                 :do (setf (gethash (pathname-name system-file) system-table)
                           (merge-pathnames system-file)))))
           (local-system-search (name)
             (values (gethash name system-table))))
    (load-systems-table)
    (push #'local-system-search asdf:*system-definition-search-functions*)
    (asdf:load-system "qvm-qaas")
    (funcall (read-from-string "qvm-qaas::zap-info"))
    (funcall (read-from-string "qvm-qaas::setup-debugger"))
    (when (find "--qvm-sdk" sb-ext:*posix-argv* :test 'string=)
      (load "app/src/mangle-shared-objects.lisp"))
    (sb-ext:save-lisp-and-die output-file
                              :compression #+sb-core-compression t
                                           #-sb-core-compression nil
                              :save-runtime-options t
                              :executable t
                              :toplevel toplevel)))
