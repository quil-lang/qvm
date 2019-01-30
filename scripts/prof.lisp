(require :sb-sprof)

(ql:quickload :qvm)

(in-package #:qvm)

(defun hadamard-program (n)
  (make-instance
   'quil:parsed-program
   :executable-code (loop :with v := (make-array (* 2 n))
                          :for i :below n
                          :do (setf (aref v i)
                                    (make-instance
                                     'quil:unresolved-application
                                     :operator "H"
                                     :arguments (list (quil:qubit i))))
                              (setf (aref v (+ n i))
                                    (make-instance
                                     'quil:measure
                                     :qubit (quil:qubit i)
                                     :address (quil:address i)))
                          :finally (return v))))

(defun testme ()
  (prepare-for-parallelization 1)
  (setf qvm::*qubits-required-for-parallelization* 100)
  
  (setf qvm:*transition-verbose* t)
  (let ((q (make-qvm 25))
        (p (hadamard-program 25)))
    (load-program q p)
    (sb-sprof:with-profiling (:max-samples 10000
                              :report :flat
                              :threads :all
                              :mode :time)
      (run q)))
  nil)


;;; MAIN



