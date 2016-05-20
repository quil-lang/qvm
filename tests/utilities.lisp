;;;; tests/utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defmacro define-float= (name float-type)
  (assert (subtypep float-type 'cl:float))
  (let* ((type-name (symbol-name float-type))
         (least-normalized
           (find-symbol (format nil "LEAST-POSITIVE-NORMALIZED-~A" type-name)
                        :common-lisp))
         (most-positive
           (find-symbol (format nil "MOST-POSITIVE-~A" type-name)
                        :common-lisp))
         (%name (alexandria:format-symbol :qvm-tests "%~A" name)))
    `(progn
       (declaim (inline ,name ,%name))
       (defun ,%name (x y epsilon)
         (declare (optimize speed (safety 0) (debug 0) (space 0))
                  (type ,float-type x y epsilon)
                  (dynamic-extent x y epsilon))
         (let ((delta (abs (- x y))))
           (declare (type ,float-type delta)
                    (dynamic-extent delta))
           (cond
             ((eql x y) t)
             ((or (zerop x)
                  (zerop y)
                  (< delta ,least-normalized))
              (< delta (* epsilon ,least-normalized)))
             (t
              (let* ((abs-x (abs x))
                     (abs-y (abs y))
                     (magnitude (sb-int:with-float-traps-masked (:overflow)
                                  (+ abs-x abs-y))))
                (declare (type ,float-type abs-x abs-y magnitude)
                         (dynamic-extent abs-x abs-y magnitude))
                (< (/ delta (min ,most-positive magnitude)) epsilon))))))
       (defun ,name (x y epsilon)
         ;; Doc string
         ,(format nil "Are the ~A numbers X and Y approximately equal ~
                     within the relative epsilon EPSILON?"
                  type-name)
         (declare (optimize speed (safety 0) (debug 0) (space 0)))
         (check-type x real)
         (check-type y real)
         (check-type epsilon real)
         (let ((x (coerce x ',float-type))
               (y (coerce y ',float-type))
               (epsilon (coerce epsilon ',float-type)))
           (declare (type ,float-type x y epsilon)
                    (dynamic-extent x y epsilon))
           (,%name x y epsilon)))
       (declaim (notinline ,name ,%name)))))

(define-float= single-float= single-float)
(define-float= double-float= double-float)

(declaim (inline complex=))
(defun complex= (x y epsilon)
  "Are the complex floats X and Y equal within the epsilon EPSILON?"
  (check-type epsilon real)
  (check-type x complex)
  (check-type y complex)
  (etypecase x
    ((complex single-float) (and (single-float= (realpart x)
                                                (realpart y)
                                                epsilon)
                                 (single-float= (imagpart x)
                                                (imagpart y)
                                                epsilon)))
    ((complex double-float) (and (double-float= (realpart x)
                                                (realpart y)
                                                epsilon)
                                 (double-float= (imagpart x)
                                                (imagpart y)
                                                epsilon)))))
(declaim (notinline complex=))
