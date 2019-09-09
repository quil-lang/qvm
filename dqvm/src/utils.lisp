;;;; src/utils.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas
;;;;         Robert Smith
;;;;         Lauren Capelluto

(in-package #:dqvm2)

(defun error-missing-initform (symbol)
  (error "You must specify ~S." symbol))

(defun instruction->string (instruction)
  "Return string representation of INSTRUCTION."
  (format nil "~/quil:instruction-fmt/" instruction))

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

(defun get-random-seed ()
  "Return a seed for the random number generator."
  ;; TODO use CFFI to call gettimeofday(2) on POSIX-compatible systems.
  #+sbcl
  (let ((random-seed (+ (nth-value 1 (sb-ext:get-time-of-day))
                        (mpi-comm-rank))))
    (format-log :info "Using random seed: ~D." random-seed)
    random-seed)
  #-sbcl
  (let ((random-seed (+ (get-universal-time)
                        (mpi-comm-rank))))
    (format-log :warning "Using random seed: ~D. This seed is of low ~
                quality, consider passing a suitable seed via the command line."
                random-seed)
    random-seed))
