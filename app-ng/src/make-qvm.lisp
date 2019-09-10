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

(defgeneric make-requested-qvm (simulation-method allocation-method num-qubits)
  (:documentation "Create and return a QVM instance of the requested type."))

(defmethod make-requested-qvm ((simulation-method (eql 'pure-state))
                               allocation-method
                               num-qubits)
  (qvm:make-qvm num-qubits :allocation (make-requested-allocation-descriptor
                                        allocation-method
                                        (expt 2 num-qubits))))

(defmethod make-requested-qvm ((simulation-method (eql 'full-density-matrix))
                               allocation-method
                               num-qubits)
  (qvm:make-density-qvm num-qubits :allocation (make-requested-allocation-descriptor
                                                allocation-method
                                                (expt 2 (* 2 num-qubits)))))
