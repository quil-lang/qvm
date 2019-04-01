;;;; tests/utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defmacro run-unless-environment-has (env-var &body body)
  (alexandria:once-only (env-var)
    `(cond
       ((uiop:getenvp ,env-var)
        (skip))
       (t
        ,@body))))

(defmacro with-output-to-quil (&body body)
  `(let ((quil:*allow-unresolved-applications* t))
     (quil:parse-quil
      (with-output-to-string (*standard-output*)
        ,@(loop :for form :in body
                :if (stringp form)
                  :collect `(write-line ,form)
                :else
                  :collect form)))))

(defmacro with-execution-modes ((&rest modes) &body body)
  "Execute body in a variety of execution modes. MODES can be:

    - :INTERPRET, where any calls to QVM:RUN will execute immediately.

    - :COMPILE, where any calls to QVM:RUN will pre-compile the Quil program.

In general, this macro is used in order to *explicitly* test for identical behavior in different modes. One mode or the other can be used to forcibly test some aspect of a single mode. (For example, one might want to test that in :COMPILE mode, some instructions actually get compiled.)
"
  (when (endp modes)
    (warn "No modes specified in WITH-EXECUTION-MODES."))
  (let ((execution-body (gensym "EXECUTION-BODY-")))
    `(flet ((,execution-body ()
              ,@body))
       (declare (dynamic-extent ,execution-body))
       ,@(when (member ':interpret modes)
           (list
            `(let ((qvm:*compile-before-running* nil))
               (format t "~&    [interpreting]~%")
               (finish-output)
               (,execution-body))))
       ,@(when (member ':compile modes)
           (list
            `(let ((qvm:*compile-before-running* t))
               (format t "~&    [compiling]~%")
               (finish-output)
               (,execution-body))))

       nil)))

(defvar *default-epsilon* 0.00001)

(defmacro with-float-traps-masked (() &body body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:overflow)
     ,@body)

  #+ccl
  (let ((mode (gensym "MODE-")))
    `(let ((,mode (ccl:get-fpu-mode)))
       (ccl:set-fpu-mode :overflow nil
                         ;;:underflow nil
                         ;;:division-by-zero nil
                         ;;:invalid nil
                         ;;:inexact nil
                         )
       (prog1 (progn ,@body)
         (apply #'ccl:set-fpu-mode ,mode))))

  #-(or ccl sbcl)
  `(progn
     ,@body))

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
         (declare (type ,float-type x y epsilon))
         (let ((delta (abs (- x y))))
           (declare (type ,float-type delta))
           (cond
             ((eql x y) t)
             ((or (zerop x) (zerop y))
              (< delta epsilon))
             ((< delta ,least-normalized)
              (< delta (* epsilon ,least-normalized)))
             (t
              (let* ((abs-x (abs x))
                     (abs-y (abs y))
                     (magnitude (with-float-traps-masked ()
                                  (+ abs-x abs-y))))
                (declare (type ,float-type abs-x abs-y magnitude))
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
           (declare (type ,float-type x y epsilon))
           (,%name x y epsilon)))
       (declaim (notinline ,name ,%name)))))

(define-float= single-float= single-float)
(define-float= double-float= double-float)

(defun cflonum= (x y &optional (epsilon *default-epsilon*))
  (and (double-float= (realpart x) (realpart y) epsilon)
       (double-float= (imagpart x) (imagpart y) epsilon)))

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

(defun make-vector (length &rest elts)
  (let ((vec (qvm::make-lisp-cflonum-vector length)))
    (loop :for i :from 0
          :for elt :in elts
          :do (setf (aref vec i) (cflonum elt))
          :finally (return vec))))
