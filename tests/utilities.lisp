;;;; tests/utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defmacro with-output-to-quil (&body body)
  `(let ((quil:*allow-unresolved-applications* t))
     (quil:parse-quil-string
      (with-output-to-string (*standard-output*)
        ,@(loop :for form :in body
                :if (stringp form)
                  :collect `(write-line ,form)
                :else
                  :collect form)))))

(defvar *default-epsilon* 0.00001)

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
       (defun ,name (x y &optional (epsilon *default-epsilon*))
         ;; Doc string
         ,(format nil "Are the ~A numbers X and Y approximately equal ~
                     within the relative epsilon EPSILON?"
                  type-name)
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

(defun absolute-float= (a b eps)
  (cond
    ((zerop a) (< (abs b) eps))
    ((zerop b) (< (abs a) eps))
    (t (< (abs (- a b)) eps))))

(defun kronecker-multiply (A B)
  "Compute the Kronecker product of matrices M1 and M2."
  (destructuring-bind (m n) (array-dimensions A)
    (destructuring-bind (p q) (array-dimensions B)
      (labels ((A-coord-to-R-start (i j)
                 (values (* i p) (* j q))))
        (let ((result (make-array (list (* m p) (* n q))
                                  :element-type (array-element-type A))))
          (dotimes (i m result)
            (dotimes (j n)
              (let ((Aij (aref A i j)))
                (multiple-value-bind (y x) (A-coord-to-R-start i j)
                  (loop :for u :below p :do
                    (loop :for v :below q :do
                      (setf (aref result (+ y u) (+ x v))
                            (* Aij (aref B u v))))))))))))))
