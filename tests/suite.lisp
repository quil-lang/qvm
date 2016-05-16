;;;; tests/suite.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:hu.dwim.stefil)

(defun qvm-tests::test-run-success-p (tr)
  (not (plusp (length (failure-descriptions-of tr)))))

(defun qvm-tests::print-test-summary (tr stream)
  (let* ((failure-descriptions (failure-descriptions-of tr))
         (failure-count (length failure-descriptions))
         (failed-assertion-count (count-if (of-type '(or failed-assertion missing-condition extra-condition)) failure-descriptions))
         (unexpected-error-count (count-if (of-type 'unexpected-error) failure-descriptions))
         (expected-count (count-if 'expected-p failure-descriptions)))
    (terpri stream)
    (format stream "Tests run: ~D~%" (hash-table-count (run-tests-of tr)))
    (format stream "Assertions: ~D~%" (assertion-count-of tr))
    (format stream "Failed Assertions: ~D~%" failed-assertion-count)
    (format stream "Expected Failures: ~D~%" expected-count)
    (format stream "Failures: ~D~%" failure-count)
    (format stream "Unexpected Errors: ~D~%" unexpected-error-count)
    (values)))


(in-package #:qvm-tests)

(defsuite qvm-test-suite)

(defun run-tests (&key (headless nil))
  "Run all QVM tests. If HEADLESS is T, disable interactive debugging and quit on completion."
  (cond
    ((null headless)
     (qvm-test-suite))
    (t
     (let ((tr (hu.dwim.stefil:without-debugging
                 (qvm-test-suite))))
       (print-test-summary tr *standard-output*)
       (uiop:quit (if (test-run-success-p tr) 0 1))))))

