;;;; tune-threads/tune-threads.lisp
;;;;
;;;; Author: John Lapeyre

(in-package #:qvm-tune-threads)

(defun findmin (list)
  "Return a list whose two elements are the minimum value of LIST and the index of that value.
If the minimum is degenerate, the fist index is returned."
  (do* ((min-val (car list))
        (min-ind 0)
        (list (cdr list) (cdr list))
        (next-val (car list) (car list))
        (ind 0 (1+ ind)))
       ((null list) (list min-val min-ind))
    (when (< next-val min-val)
      (setf min-val next-val)
      (setf min-ind (1+ ind)))))

(defmacro simple-time (num-trials &body body)
  "Execute BODY NUM-TRIALS times and return the run time in seconds."
  (alexandria:with-gensyms (start-time stop-time i)
    `(let ((,start-time (get-internal-run-time)))
       (dotimes (,i ,num-trials) ,@body)
       (let ((,stop-time (get-internal-run-time)))
         (/ (- ,stop-time ,start-time) (float internal-time-units-per-second))))))
