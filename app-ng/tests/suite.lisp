(in-package #:qvm-app-ng-tests)

(defun run-qvm-app-ng-tests (&key (verbose nil) (headless nil))
  "Run all qvm-app-ng tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (let ((successp (run-package-tests :package ':qvm-app-ng-tests
                                     :verbose (or headless verbose)
                                     :describe-failures t
                                     :interactive (not headless))))
    (if headless
        (qvm-app-ng::quit-nicely successp)
        successp)))


