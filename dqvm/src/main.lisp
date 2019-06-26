;;;; main.lisp

(in-package #:dqvm)

;;; This file contains the main entry point to the program.

(defparameter *program-name* "dqvm")

(defparameter *option-spec*
  `((("execute" #\e)
     :type string
     :documentation "execute a Quil file")
    (("help" #\h)
     :type boolean
     :optional t
     :documentation "display help")))

(defun show-help ()
  (format t "Usage:~%")
  (format t "    mpirun -np <number of processors> ~A -e <filename> ~%" *program-name*))

(defun show-welcome ()
  (format t "~&~
*******************************
* Welcome to the Rigetti DQVM *~%~
*******************************~%")
  nil)

(defun resolve-safely (filename)
  (flet ((contains-up (filename)
           (member-if (lambda (obj)
                        (or (eql ':UP obj)
                            (eql ':BACK obj)))
                      (pathname-directory filename))))
    (cond
      ((uiop:absolute-pathname-p filename)
       (error "Not allowed to specify absolute paths to INCLUDE."))

      ((uiop:relative-pathname-p filename)
       ;; Only files allowed.
       (unless (uiop:file-pathname-p filename)
         (error "INCLUDE requires a pathname to a file."))
       (when (contains-up filename)
         (error "INCLUDE can't refer to files above."))
       filename)

      (t
       (error "Invalid pathname: ~S" filename)))))

(defun safely-read-quil (filename)
  "Safely read the Quil file designated by FILENAME."
  (flet ((read-it (file)
           (let ((quil::*allow-unresolved-applications* t))
             (quil:read-quil-file file))))

    (let ((quil:*resolve-include-pathname* #'resolve-safely))
      (read-it filename))))

(defun quit-nicely (&optional (code 0))
  (mpi:mpi-finalize)
  #+sbcl
  (sb-ext:exit :code code :abort nil)
  #-sbcl
  (uiop:quit code t))

(defun process-options (&key execute help)
  (when help
    (when (master-node-p)
      (show-help))
    (quit-nicely))

  (when (master-node-p)
    (show-welcome))

  (when execute
    (when (master-node-p)
      (assert (power-of-two-p +worker-count+)
              nil
              "There has to be a power-of-two ~
             number of workers. We have ~D."
              +worker-count+))

    (with-errors-printed-verbosely
      (unwind-protect (if (master-node-p)
                          (dqvm.master:%main-master :program (safely-read-quil execute))
                          (dqvm.worker:%main-worker))
        (quit-nicely -1)))

    (quit-nicely)))

(defun %main (argv)
  "The entry point to the program."
  (setf *program-name* (pop argv))
  (sb-ext:disable-debugger)
  
  (mpi:mpi-init :thread-support ':mpi-thread-multiple)

  (cond
    ((null argv)
     (when (master-node-p)
       (show-help))
     (quit-nicely))
    (t
     (command-line-arguments:handle-command-line
      *option-spec*
      'process-options
      :command-line argv
      :name "dqvm"
      :rest-arity nil))))
