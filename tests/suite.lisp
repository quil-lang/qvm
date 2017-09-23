;;;; tests/suite.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defun run-qvm-tests (&key (headless nil))
  "Run all QVM tests. If HEADLESS is T, disable interactive debugging and quit on completion."
  (qvm:prepare-for-parallelization)
  (cond
    ((null headless)
     (run-package-tests :package ':qvm-tests
                        :verbose nil
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':qvm-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))

