;;;; tests/suite.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defun run-qvm-tests (&key (headless nil))
  "Run all QVM tests. If HEADLESS is T, disable interactive debugging and quit on completion."
  ;; Bug in Fiasco commit fe89c0e924c22c667cc11c6fc6e79419fc7c1a8b
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream
                                            *standard-output*))
  (qvm:prepare-for-parallelization)
  (qvm::enable-all-qvm-optimizations)
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

