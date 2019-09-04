;;;; tests/suite.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas
;;;;         Robert Smith

(in-package #:dqvm2-tests)

(defun run-dqvm2-tests (&key (headless nil))
  "Run all QVM tests. If HEADLESS is T, disable interactive debugging and quit on completion."
  ;; Bug in Fiasco commit fe89c0e924c22c667cc11c6fc6e79419fc7c1a8b
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream
                                            *standard-output*))
  (qvm:prepare-for-parallelization)
  (cond
    ((null headless)
     (run-package-tests :package ':dqvm2-tests
                        :verbose nil
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':dqvm2-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (qvm::boolean-bit (not successp)))))))
