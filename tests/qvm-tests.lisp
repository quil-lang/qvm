;;;; tests/qvm-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-complex-double-float-replace ()
  "Test for bug #23 (github) in Clozure CL where REPLACE does not
  faithfully copy from arrays of complex double-floats."
  (let ((a (make-array 4 :element-type '(complex double-float)))
        (b (make-array 4 :element-type '(complex double-float))))
    (dotimes (i 4)
      (setf (aref b i) (complex (* 1.0d0 i) (* 2.0d0 i))))
    (replace a b)
    (is (equalp a b))))

(deftest test-bit-out-of-range ()
  "Test that we detect an error when a bit is out of range."
  (let ((q (make-qvm 1 :classical-memory-size 8)))
    (signals simple-error
      (classical-bit q 9))
    (signals simple-error
      (classical-bit q -1))))

(deftest test-defgate-persistence ()
  (let ((q1 (qvm:make-qvm 1))
        (q2 (qvm:make-qvm 1)))
    (qvm:load-program q1 (with-output-to-quil
                           "DEFGATE A:"
                           "    0, 1"
                           "    1, 0"))
    (qvm:load-program q2 (with-output-to-quil
                           "DEFGATE A:"
                           "    1, 0"
                           "    0, 1"))
    (is (not (eq (qvm::lookup-gate q1 "A")
                 (qvm::lookup-gate q2 "A"))))))
