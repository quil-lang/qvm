;;;; bench/suite.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-benchmarks)

(defun run-benchmarks (&key (headless nil))
  "Run all QVM benchmarks. If HEADLESS is T, quit on completion."
  (cond
    ((null headless)
     (run-package-benchmarks :package ':qvm-benchmarks
                             :verbose nil))
    (t
     (let ((successp (run-package-benchmarks :package ':qvm-benchmarks
                                             :verbose t)))
       (uiop:quit (if successp 0 1))))))

