;;;; coverage-report.lisp
;;;;
;;;; Author: Zach Beane

(require :sb-cover)

(defvar *system* "qvm")

(defun system-lisp-files (system)
  (unless (typep system 'asdf:system)
    (setf system (asdf:find-system system)))
  (let ((result '()))
    (labels ((explore (thing)
               (typecase thing
                 (asdf:parent-component
                  (mapc #'explore (asdf:component-children thing)))
                 (asdf:cl-source-file
                  (push (namestring (asdf:component-pathname thing)) result)))))
      (explore system)
      result)))

#-quicklisp
(load "~/quicklisp/setup.lisp")

(format *query-io* "Compiling and loading ~A..." *system*)
(force-output *query-io*)

;; Compile system and prerequisites outside of coverage
ql-dist::(ensure-installed (release "cffi"))
ql-dist::(ensure-installed (release "fiasco"))
(ql:quickload *system* :silent t)

(declaim (optimize sb-cover:store-coverage-data))
(asdf:load-system *system* :force t)
(format *query-io* "done~%")

(let ((*compile-verbose* nil)
      (*load-verbose* nil))
  (asdf:test-system *system*))

(handler-bind ((warning #'muffle-warning))
  (let ((interesting-files (system-lisp-files *system*))
        (base (asdf:system-relative-pathname *system*
                                             "coverage-report/html/")))
    ;; SB-COVER:REPORT will happily report on everything it has
    ;; compiled with the code coverage declaration. This maphash will
    ;; restrict it to only the files in *SYSTEM*. Beware:
    ;; SB-C:*CODE-COVERAGE-INFO* is not part of the public API; switch
    ;; to :IF-MATCHES report option in the future.
    (maphash (lambda (file data)
               (declare (ignore data))
               (unless (member file interesting-files :test 'string=)
                 (remhash file sb-c:*code-coverage-info*)))
             sb-c:*code-coverage-info*)
    (sb-cover:report base)
    (let* ((cover (merge-pathnames "cover-index.html" base))
           (index (merge-pathnames "index.html" base)))
      (rename-file cover index)
      (format *query-io* "Coverage report written to ~A~%"
              index))))

