;;;; tests/parallel-tests.lisp
;;;;
;;;; Author: John Lapeyre

(in-package #:qvm-tests)

(deftest test-changing-number-of-workers ()
  "Test that preparing for parallelization is idempotent only if the number of workers does not change.

More precisely, idempotent in side-effects. The function PREPARE-FOR-PARALLELIZATION is called only for side effects."
  (qvm:prepare-for-parallelization 1)
  (if (< (count-logical-cores) 2)
      (is lparallel:*kernel*) ; For one core, we only check that the thread pool is created.
      (let* ((save-kernel-1 lparallel:*kernel*)
             (num-workers-1 (lparallel:kernel-worker-count))
             (save-kernel-1-again (progn (qvm:prepare-for-parallelization 1)
                                        lparallel:*kernel*))
             (save-kernel-2 (progn (qvm:prepare-for-parallelization 2)
                                   lparallel:*kernel*))
             (num-workers-2 (lparallel:kernel-worker-count)))
        (is (= num-workers-1 1)) ; Was one worker actually created ?
        (is (= num-workers-2 2)) ; Were two workers actually created ?
        (is (eq save-kernel-1 save-kernel-1-again)) ; Was the second preparation a no-op ?
        (is (not (eq save-kernel-1 save-kernel-2)))))) ; Did the third preparation create a new worker pool ?
