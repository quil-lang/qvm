;;;; tune-threads/tests.lisp
;;;;
;;;; Author: John Lapeyre

(in-package #:qvm-tune-threads)

;;; At present this only tests MEAN-DEV-FROM-SUMS, but not in a test framework.

(defun rand-sums (n)
  "Return the sum and sum of squares of N random samples uniformly distributed on (0, 1)."
  (loop :for i :to n
        :for random := (random 1.0d0)
        :sum random :into sum
        :sum (expt random 2) :into sumsq
        :finally (return (values sum sumsq))))

(defun rand-mean-dev (n)
  "Return the mean and standard deviation computed from N samples.
This is meant to test MEAN-DEV-FROM-SUMS."
  (multiple-value-bind (sum sumsq ) (rand-sums n)
    (mean-dev-from-sums sum sumsq n)))

(defun rand-test (n)
  "Test the deviation of the sample mean and standard deviation of N numbers from the population values.
The numbers should approach zero with increasing N."
  (destructuring-bind (mean dev) (rand-mean-dev n)
    (list (- mean 0.5d0) (- dev (sqrt (/ 1.0d0 12))))))
