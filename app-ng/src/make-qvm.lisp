;;;; src/make-qvm.lisp
;;;;
;;;; Author: appleby

(in-package #:qvm-app-ng)

(defun run-program-on-qvm (qvm parsed-program)
  ;; Like QVM:RUN-PROGRAM, but bring your own QVM.
  (qvm:load-program qvm parsed-program :supersede-memory-subsystem t)
  (qvm:run qvm))

(defun make-requested-allocation-descriptor (allocation-method length)
  (ecase allocation-method
    (native  (make-instance 'qvm:lisp-allocation :length length))
    (foreign (make-instance 'qvm:c-allocation :length length))))

(defun make-requested-qvm (allocation-method simulation-method num-qubits)
  (check-type num-qubits (integer 0))
  (ecase simulation-method
    (pure-state
     (qvm:make-qvm num-qubits
                   :allocation (make-requested-allocation-descriptor
                                allocation-method
                                (expt 2 num-qubits))))
    (full-density-matrix
     (qvm:make-density-qvm num-qubits
                           :allocation (make-requested-allocation-descriptor
                                        allocation-method
                                        (expt 2 (* 2 num-qubits)))))))
