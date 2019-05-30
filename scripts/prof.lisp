(require :sb-sprof)

(ql:quickload :qvm)

(in-package #:qvm)

(defun hadamard-program (n)
  (quil:parse-quil
   (with-output-to-string (s)
     (format s "DECLARE ro BIT[~d]~%" n)
     (dotimes (q n)
       (format s "H ~d~%" q))
     (dotimes (q n)
       (format s "MEASURE ~d ro[~d]~%" q q)))))

(defun testme ()
  (prepare-for-parallelization 1)
  (setf qvm::*qubits-required-for-parallelization* 49)

  (setf qvm:*transition-verbose* t)
  (let ((q (make-qvm 25))
        (p (hadamard-program 25)))
    (load-program q p :supersede-memory-subsystem t)
    (sb-sprof:with-profiling (:max-samples 10000
                              :report :flat
                              :threads :all
                              :mode :time)
      (run q)))
  nil)

;;; MAIN
