;;;; tune-threads/tune-threads.lisp
;;;;
;;;; Author: John Lapeyre

(in-package #:qvm-tune-threads)

(defun mean-dev-from-sums (x-sum x-sq-sum n)
  "Given a sum X-SUM and sum of squares X-SQ-SUM of N iid random samples, compute the mean and the standard deviation"
  (let ((x-mean (/ x-sum n)))
    (list x-mean (sqrt (- (/ x-sq-sum n) (expt x-mean 2))))))

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

(defun norm-min (list)
  "Normalize LIST by its minimum element"
  (let ((min (apply #'min list)))
    (map 'list #'(lambda (x) (/ x min)) list)))

(defmacro simple-time (num-trials &body body)
  "Execute BODY NUM-TRIALS times and return the run time in seconds."
  (alexandria:with-gensyms (start-time stop-time i)
    `(let ((,start-time (get-internal-run-time)))
       (dotimes (,i ,num-trials) ,@body)
       (let ((,stop-time (get-internal-run-time)))
         (/ (- ,stop-time ,start-time) (float internal-time-units-per-second))))))

(defmacro simple-time-dev (num-reps num-trials &body body)
  "Execute BODY NUM-TRIALS repeatedly in NUM-REPS blocks of NUM-TRIALS trials each,
and return the mean and standard deviation of the run time seconds.
This provides a measure of the variation in timing over NUM-TRIALS trials."
  `(let ((cum-time 0.0)
         (cum-time-sq 0.0))
     (dotimes (i ,num-reps)
       (let ((one-time (simple-time ,num-trials ,@body)))
         (setf cum-time (+ one-time cum-time))
         (setf cum-time-sq (+ (expt one-time 2) cum-time-sq))))
     (mean-dev-from-sums cum-time cum-time-sq ,num-reps)))
