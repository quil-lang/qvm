;;;; stress-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

;;; This file contains general stress tests, like aggressive memory
;;; allocation, to gaugue the stability of the QVM.

(deftest test-garbage-collection ()
  "Test that large QVMs get garbage collected and do so without erroring."
  (tg:gc :full t)
  (loop :with big-qubit-amount := 23
        ;; This should allocate about
        ;;
        ;;     (8 + 8) * 2^23 / 1024^2 = 128 MiB
        ;;
        ;; per iteration.
        :repeat 128
        :with qvm := nil
        :do (setf qvm (qvm:make-qvm big-qubit-amount))
        :finally (setf qvm nil)))
