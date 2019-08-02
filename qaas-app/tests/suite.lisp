(in-package :qvm-qaas-tests)

(defun run-qvm-qaas-tests (&key (verbose nil) (headless nil))
  "Run all qvm-qaas tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (cond
    ((null headless)
     (run-package-tests :package ':qvm-qaas-tests
                        :verbose verbose
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':qvm-qaas-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))


(deftest test-update-available ()
  (multiple-value-bind (update-available-p update)
      (qvm-qaas::sdk-update-available-p "0.0.0")
    (if update-available-p
        ;; If the network is down, then update-available-p is NIL, but
        ;; we don't want to error in that case. Skip instead.
        (is update)
        (skip))))


