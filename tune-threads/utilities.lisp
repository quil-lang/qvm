;;;; tune-threads/tune-threads.lisp
;;;;
;;;; Author: John Lapeyre

(in-package #:qvm-tune-threads)

(defun sort-hash-table-keys (h &optional (sort-func #'<))
  (sort (alexandria:hash-table-keys h) sort-func))

(defun mean-dev-from-sums (x-sum x-sq-sum n)
  "Given a sum X-SUM and sum of squares X-SQ-SUM of N iid random samples, compute the mean and the standard deviation"
  (let ((x-mean (/ x-sum n)))
    (list x-mean (sqrt (- (/ x-sq-sum n) (expt x-mean 2))))))

(defun findmin (seq)
  "Return the index of the minimum value in SEQ.
If the minimum is degenerate, the fist index is returned."
  (assert (not (alexandria:emptyp seq)))
  (loop :for item :being :the :elements :of seq
        :for i :from 0
        :with imin := 0
        :with minitem := (elt seq 0)
        :do (when (< item minitem)
              (setf imin i)
              (setf minitem item))
        :finally (return imin )))

(defun rescale-by-minimum (seq)
  "Normalize SEQ by its minimum element"
  (let ((min (alexandria:extremum seq #'<)))
    (map 'vector #'(lambda (x) (/ x min)) seq)))

(defmacro simple-time (num-trials &body body)
  "Execute BODY NUM-TRIALS times and return the run time in seconds."
  (alexandria:once-only (num-trials)
    (alexandria:with-gensyms (start-time stop-time i)
      `(let ((,start-time (get-internal-run-time)))
         (declare (type fixnum ,i))
         (dotimes (,i ,num-trials) ,@body)
         (let ((,stop-time (get-internal-run-time)))
           (/ (- ,stop-time ,start-time) (float internal-time-units-per-second)))))))

(defmacro simple-time-dev (num-reps num-trials &body body)
  "Execute BODY repeatedly in NUM-REPS blocks of NUM-TRIALS trials each,
and return the mean and standard deviation of the run time seconds.
This provides a measure of the variation in timing over NUM-TRIALS trials."
  (alexandria:once-only (num-reps num-trials)
    (alexandria:with-gensyms (cum-time cum-time-sq one-time i)
      (alexandria:once-only (num-reps)
        `(let ((,cum-time 0.0)
               (,cum-time-sq 0.0))
           (dotimes (,i ,num-reps)
             (let ((,one-time (simple-time ,num-trials ,@body)))
               (setf ,cum-time (+ ,one-time ,cum-time))
               (setf ,cum-time-sq (+ (expt ,one-time 2) ,cum-time-sq))))
           (mean-dev-from-sums ,cum-time ,cum-time-sq ,num-reps))))))
