;;;; tests/linear-algebra-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defun random-wavefunction (&optional (size 8))
  (map-into (make-vector size) (lambda ()
                                 (cflonum
                                  (complex (random 1.0d0)
                                           (random 1.0d0))))))


(defun naive-sum (f v)
  (reduce #'+ v :key f))

(deftest test-psum-dotimes ()
  "Test that PSUM-DOTIMES works."
  (let* ((iterations 10000)
         (serial   (let ((*qubits-required-for-parallelization* 50))
                     (qvm::psum-dotimes (i iterations)
                       (flonum 1))))
         (parallel (let ((*qubits-required-for-parallelization* 1))
                     (qvm::psum-dotimes (i iterations)
                       (flonum 1)))))
    (is (double-float= iterations serial))
    (is (double-float= iterations parallel))))

(deftest test-psum ()
  "Test that PSUM works."
  (let* ((qubits-to-test 10)
         (vectors (loop :for i :below qubits-to-test
                        :for size := (expt 2 (1+ i))
                        :collect (random-wavefunction size)))
        serial-a
        serial-b)
    (dolist (v vectors)
      ;; Serial case.
      (let* ((*qubits-required-for-parallelization* 50)
             (a (qvm::psum #'probability v))
             (b (naive-sum #'probability v)))
        (setf serial-a a
              serial-b b)
        (is (double-float= a b)))
      
      ;; Parallel case.
      (let* ((*qubits-required-for-parallelization* 1)
             (a (qvm::psum #'probability v))
             (b (naive-sum #'probability v)))
        (is (double-float= a b))
        (is (double-float= a serial-a))
        (is (double-float= b serial-b))))))

(defun naive-norm (v)
  (loop :for x :across v
        :sum (expt (abs x) 2) :into sq-norm
        :finally (return (sqrt sq-norm))))

(deftest test-normalization ()
  "Test that wavefunction normalization produces a unit vector."
  (dotimes (i 11)
    (let ((v (random-wavefunction (expt 2 i))))
      (normalize-wavefunction v)
      (is (double-float= 1 (naive-norm v))))))

(deftest test-cdf ()
  "Test that CUMULATIVE-DISTRIBUTION-FUNCTION seems to work."
  (is (every #'double-float= #() (qvm::cumulative-distribution-function (make-vector 0))))
  (is (every #'double-float= #(1.0d0 2.0d0 3.0d0) (qvm::cumulative-distribution-function
                                                   (make-vector 3 1 1 1))))
  (is (every #'double-float= #(0.5d0 1.0d0 1.5d0) (qvm::cumulative-distribution-function
                                                   (make-vector 3
                                                                (sqrt 1/2)
                                                                (sqrt 1/2)
                                                                (sqrt 1/2))))))
