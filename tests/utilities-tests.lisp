;;;; utilities-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-nat-tuple-operations ()
  "Test various operations on the NAT-TUPLE data structure."
  (flet ((members (nt)
           (let ((membership 0))
             (qvm::do-nat-tuple (i elt nt)
               (setf (ldb (byte 1 elt) membership) 1))
             membership)))
    (let ((empty (qvm::nat-tuple))
          (three (qvm::nat-tuple 2 3 4))
          (two   (qvm::nat-tuple 1 3)))
      ;; Test NAT-TUPLE-CARDINALITY
      (is (= 0 (qvm::nat-tuple-cardinality empty)))
      (is (= 3 (qvm::nat-tuple-cardinality three)))
      (is (= 2 (qvm::nat-tuple-cardinality two))))))

;; TODO: test other nat tuple functions
