;;;; utilities-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(in-suite qvm-test-suite)

(deftest nat-tuple-operations ()
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
      (is (= 2 (qvm::nat-tuple-cardinality two)))

      ;; Test DO-NAT-TUPLE, NAT-TUPLE-UNION
      (is (= (logior (members two)
                     (members three))
             (members (qvm::nat-tuple-union two three)))
          "Incorrectly computed union elements.")
      (is (= (logior (members three)
                     (members two))
             (members (qvm::nat-tuple-union three two)))
          "Incorrectly computed union elements.")
      
      ;; Test NAT-TUPLE-DIFFERENCE
      (is (= (logandc2 (members two)
                       (members three))
             (members (qvm::nat-tuple-difference two three)))
          "Incorrectly comoputed difference elements.")
      (is (= (logandc2 (members three)
                       (members two))
             (members (qvm::nat-tuple-difference three two)))))))
