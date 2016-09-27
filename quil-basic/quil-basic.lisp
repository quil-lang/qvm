(defpackage #:quil-basic
  (:use #:cl)
  (:export #:%main))

(in-package #:quil-basic)

(defvar *lines* nil
  "The lines of the program.")

(defun print-program ()
  (terpri)
  (loop :for i :from 0
        :for line :in *lines*
        :do (format t "~&  ~D) ~A~%" i line))
  (format t "~&  ~D)~%" (length *lines*))
  (terpri)
  nil)

(defun append-line (line)
  (setf *lines* (append *lines* (list line))))

(defun inject-line (line pos)
  (setf *lines* (append (subseq *lines* 0 pos)
                        (list line)
                        (subseq *lines* pos))))

(defun zap-line (pos)
  (setf *lines* (append (subseq *lines* 0 pos)
                        (subseq *lines* (1+ pos)))))

(defun quil-program ()
  (let* ((program-string (format nil "~{~A~^~%~}" *lines*))
         (quil::*allow-unresolved-applications* t))
    (quil:parse-quil-string program-string)))

(defun trim-whitespace (s)
  (string-trim '(#\Space #\Tab) s))

(defun bad-input (l)
  (format t "~&bad input, ignoring: ~A~%" l))

(defparameter *command-letters* (coerce "hqlwr+-csp" 'list))

(defmacro loop-without-errors (&body body)
  `(loop
     (handler-case (progn ,@body)
       (simple-error (c)
         (format t "~&Got an error: ~A~%" c))
       (error (c)
         (declare (ignore c))
         (format t "~&Got an error.~%")))))

(defun get-line ()
  (format t "? ")
  (finish-output)
  (read-line *standard-input* nil nil nil))

(defun command-loop ()
  (loop-without-errors
    (finish-output)
    (let ((line (get-line)))
      ;; End if we reach EOF.
      (when (null line)
        (return))
      
      (cond
        ;; Empty line
        ((zerop (length line))
         ;; Do nothing
         )
        
        ;; Command
        ((char= #\/ (char line 0))
         (cond
           ((or (= 1 (length line))
                (not (member (char line 1) *command-letters*
                             :test #'char-equal)))
            (bad-input line))
           (t
            (let ((command-letter (char-downcase (char line 1))))
              (case command-letter
                ;; Help
                (#\h
                 (show-help))
                ;; Quit
                (#\q
                 (write-line "Bye.")
                 (return))
                ;; Clear program
                (#\c
                 (write-line "Clearing program.")
                 (setf *lines* nil))
                ;; List/Print program
                (#\p
                 (print-program))
                ;; Save program
                (#\s
                 (let ((filename (string-left-trim '(#\Space #\Tab)
                                                   (subseq line 2))))
                   (cond
                     ((notany #'graphic-char-p filename)
                      (format t "~&Invalid file name.~%"))
                     (t
                      (with-open-file (s filename :direction ':output
                                                  :if-does-not-exist ':create
                                                  :if-exists ':supersede)
                        (dolist (line *lines*)
                          (write-line line s))
                        (format t "~&Wrote program to file '~A'.~%" filename))))))
                ;; Open a program
                (#\l
                 (let ((filename (string-left-trim '(#\Space #\Tab)
                                                   (subseq line 2))))
                   (cond
                     ((not (uiop:file-exists-p filename))
                      (format t "~&The file '~A' doesn't exist.~%" filename))
                     (t
                      (with-open-file (s filename :direction ':input
                                                  :if-does-not-exist ':error)
                        (setf *lines* nil)
                        (loop :for line := (read-line s nil nil nil)
                              :until (null line)
                              :do (push line *lines*)
                              :finally (setf *lines* (nreverse *lines*)))
                        (format t "~&Loaded program from '~A'.~%" filename))))))
                ;; Execution
                ((#\w #\r)
                 (let ((p (ignore-errors (quil-program))))
                   (if (null p)
                       (format t "Bad Quil program. Review it and retry.~%")
                       (let* ((qubits (quil:qubits-needed p))
                              (qvm (qvm:make-qvm qubits)))
                         (qvm:load-program qvm p)
                         (format t "Running program...")
                         (qvm:run qvm)
                         (format t "done!~%")
                         (case command-letter
                           (#\w
                            (loop :for i :from 0
                                  :for z :across (qvm::amplitudes qvm)
                                  :for p := (qvm::probability z)
                                  :do (format t "  |~v,'0B>: ~12F, ~12F; P=~5F%~%"
                                              qubits
                                              i
                                              (realpart z)
                                              (imagpart z)
                                              (* 100 p))))
                           (#\r
                            (loop :for i :below (qvm:classical-memory-size qvm)
                                  :for b := (qvm:classical-bit qvm i)
                                  :do (when (zerop (mod i 8))
                                        (format t "~&  ~2D to ~2D: " i (1- (+ i 8))))
                                      (format t "~A" b))
                            (terpri)))))))
                ;; Add/Zap line
                ((#\+ #\-)
                 (multiple-value-bind (line-num num-chars)
                     (parse-integer line :start 2 :junk-allowed t)
                   (if (or (null line-num)
                           (not (<= 0 line-num (length *lines*))))
                       (bad-input line)
                       (case command-letter
                         (#\+
                          (let ((rest-line (trim-whitespace
                                            (subseq line num-chars))))
                            (inject-line rest-line line-num)))
                         (#\-
                          (unless (= line-num (length *lines*))
                            (format t "Deleting line ~D: ~A~%"
                                    line-num
                                    (nth line-num *lines*))
                            (zap-line line-num))))))))))))

        ;; Presumably this is Quil code.
        (t
         (append-line line))))))

(defun show-help ()
  (write-line "Type Quil or type commands that start with the '/' character.")
  (terpri)
  (write-line "Commands:")
  (write-line "    /h             : Show this help.")
  (write-line "    /q             : Quit.")
  (write-line "    /p             : Print the program.")
  (write-line "    /c             : Clear the program.")
  (write-line "    /s <filename>  : Save the program to <filename>.")
  (write-line "    /l <filename>  : Load the program at <filename>.")
  (write-line "    /w             : Show the wavefunction.")
  (write-line "    /r             : Run the program and show classical memory.")
  (write-line "    /-<NUM>        : Delete line <NUM>.")
  (write-line "    /+<NUM> <Quil> : Add Quil instruction at line <NUM>.")
  (terpri))

(defun %main (argv)
  (declare (ignore argv))
  (sb-ext:disable-debugger)
  (setf *error-output* (make-broadcast-stream))
  (write-line "Welcome to the Quil Machine by Rigetti Computing!")
  (write-line "A tool to explore quantum programming.")
  (terpri)
  (write-line "This software is provided for personal and non-commercial")
  (write-line "use only. See LICENSE for details.")
  (terpri)
  (write-line "Copyright (c) 2016 Rigetti Computing")
  (terpri)
  (show-help)
  (finish-output)
  (handler-case (handler-bind ((warning #'muffle-warning)) (command-loop))
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (format t "~&Bye.~%")
      (uiop:quit 0)))
  (uiop:quit 0))
