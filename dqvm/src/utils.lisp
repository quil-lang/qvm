;;;; src/utils.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas
;;;;         Robert Smith
;;;;         Lauren Capelluto

(in-package #:dqvm2)

(defun error-missing-initform (symbol)
  (error "You must specify ~a" (prin1-to-string symbol)))

(defun instruction->string (instruction)
  "Return string representation of INSTRUCTION."
  (with-output-to-string (string)
    (quil:print-instruction instruction string)))

(defun string->instruction (string)
  "Return instruction corresponding to STRING."
  (aref (quil:parsed-program-executable-code (quil:parse-quil string)) 0))

(defun get-maximum-arity (instructions)
  "Return the maximum arity of an array of INSTRUCTIONS."
  (loop :for instruction :across instructions
        :maximizing (length (quil::arguments instruction))))

(defun dqvm-error (datum &rest arguments)
  "Signal the condition formed by DATUM and ARGUMENTS."
  (apply #'format-log :err datum arguments)
  (apply #'error datum arguments))

(defmacro with-foreign-arrays (bindings &body body)
  (if bindings
      `(cffi:with-foreign-array ,(first bindings)
         (with-foreign-arrays ,(rest bindings)
           ,@body))
      `(progn
         ,@body)))
