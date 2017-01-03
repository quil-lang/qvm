;;;; utilities-tests.lisp
;;;;
;;;; Authors: Robert Smith
;;;;          Peter Karalekas

(in-package #:qvm-tests)

(defun set-equal (list1 list2)
  (and (= (length list1)
          (length list2))
       (loop :for item :in list2
             :always (member item list1))))

(deftest test-make-nat-tuple ()
  (let ((nt (qvm::make-nat-tuple)))
    (is (null (qvm::nat-tuple-list nt)))
    (is (= 0 (qvm::nat-tuple-membership nt)))))

(deftest test-nat-tuple-add ()
  (let* ((nt (qvm::nat-tuple 1 2 3))
         (elt 4)
         (ntadd (qvm::nat-tuple-add nt elt))
         (eltsame 3)
         (ntsame (qvm::nat-tuple-add nt eltsame)))
    (is (equal (cons elt (qvm::nat-tuple-list nt))
               (qvm::nat-tuple-list ntadd)))
    (is (= (dpb 1 (byte 1 elt) (qvm::nat-tuple-membership nt))
           (qvm::nat-tuple-membership ntadd)))
    (is (equal (qvm::nat-tuple-list nt)
               (qvm::nat-tuple-list ntsame)))
    (is (= (qvm::nat-tuple-membership nt)
           (qvm::nat-tuple-membership ntsame)))))

(deftest test-nat-tuple-cardinality ()
  (let ((empty (qvm::nat-tuple))
        (three (qvm::nat-tuple 2 3 4))
        (two   (qvm::nat-tuple 1 3)))
    (is (= 0 (qvm::nat-tuple-cardinality empty)))
    (is (= 3 (qvm::nat-tuple-cardinality three)))
    (is (= 2 (qvm::nat-tuple-cardinality two)))))

(deftest test-nat-tuple ()
  (let* ((vals (list 1 2 3))
         (nt (apply #'qvm::nat-tuple vals)))
    (is (equal (reverse vals) (qvm::nat-tuple-list nt)))
    (is (every (lambda (bit) (logbitp bit (qvm::nat-tuple-membership nt))) vals))))

(deftest test-nat-tuple-complement ()
  (let* ((n 6)
         (nt (qvm::nat-tuple 0 2 4))
         (ntcomp (qvm::nat-tuple-complement n nt))
         (ntfull (qvm::nat-tuple-complement n (qvm::nat-tuple))))
    (is (null (intersection (qvm::nat-tuple-list nt)
                            (qvm::nat-tuple-list ntcomp))))
    (is (set-equal (qvm::nat-tuple-list ntfull)
                   (union (qvm::nat-tuple-list nt)
                          (qvm::nat-tuple-list ntcomp))))))
