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

(defun nat-tuple-list (nt)
  (coerce nt 'list))

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
    (is (equal (reverse vals) (nat-tuple-list nt)))))

(deftest test-nat-tuple-complement ()
  (let* ((n 6)
         (nt (qvm::nat-tuple 0 2 4))
         (ntcomp (qvm::nat-tuple-complement n nt))
         (ntfull (qvm::nat-tuple-complement n (qvm::nat-tuple))))
    (is (null (intersection (nat-tuple-list nt)
                            (nat-tuple-list ntcomp))))
    (is (set-equal (nat-tuple-list ntfull)
                   (union (nat-tuple-list nt)
                          (nat-tuple-list ntcomp))))))

(deftest test-transposition-decomposition ()
  (labels ((random-perm (n)
             (loop :with taken := (1- (expt 2 n))
                   :until (zerop taken)
                   :collect (loop :with r := (random n)
                                  :until (logbitp (setf r (random n)) taken)
                                  :finally (progn
                                             (setf taken (dpb 0 (byte 1 r) taken))
                                             (return r)))))
           (verify-decomp (perm)
             (let ((transpositions (qvm::permutation-to-transpositions perm))
                   (identity (loop :for i :below (length perm) :collect i)))
               (loop :for (a . b) :in transpositions
                     :do (rotatef (elt identity a) (elt identity b))
                     :finally (is (equalp identity perm))))))
    (dotimes (i 1000)
      (verify-decomp (random-perm (expt 2 (1+ (random 5))))))))

(deftest test-subdivide ()
  "Test that SUBDIVIDE works."
  (let ((workers 8))
    ;; Degenerate case
    (dotimes (i workers)
      (is (equalp (qvm::subdivide i workers) `((0 . ,i)))))

    ;; Equal case
    (is (equalp (qvm::subdivide workers workers)
                (loop :for i :below workers
                      :collect (cons i (1+ i)))))

    ;; Double equal case
    (is (equalp (qvm::subdivide (* 2 workers) workers)
                (loop :for i :below (* 2 workers) :by 2
                      :collect (cons i (+ 2 i)))))

    ;; Slack case
    (is (equalp (qvm::subdivide (+ 3 workers) workers)
                (loop :for i :below (1- workers)
                      :collect (cons i (1+ i)) :into ranges
                      :finally (return (append ranges
                                               `((,(1- workers) . ,(+ 3 workers))))))))))

(deftest test-inject-bit ()
  (let ((source #b1111))
    (is (= #b11110 (qvm::inject-bit source 0)))
    (is (= #b11101 (qvm::inject-bit source 1)))
    (is (= #b11011 (qvm::inject-bit source 2)))
    (is (= #b10111 (qvm::inject-bit source 3)))
    (is (= #b01111 (qvm::inject-bit source 4)))))

(deftest test-eject-bit ()
  (let ((target #b1111))
    (is (= target (qvm::eject-bit #b11110 0)))
    (is (= target (qvm::eject-bit #b11101 1)))
    (is (= target (qvm::eject-bit #b11011 2)))
    (is (= target (qvm::eject-bit #b10111 3)))
    (is (= target (qvm::eject-bit #b01111 4)))))
