;;;; tests/linear-algebra-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defun random-wavefunction (&optional (size 8))
  (map-into (qvm::make-vector size) (lambda ()
                                      (cflonum
                                       (complex (random 1.0d0)
                                                (random 1.0d0))))))


(defun naive-sum (f v)
  (let ((s 0))
    (map nil (lambda (x) (incf s (funcall f x))) v)
    s))

(deftest test-psum-dotimes ()
  "Test that PSUM-DOTIMES works."
  (let* ((iterations 10000)
         (serial   (let ((*qubits-required-for-parallelization* 100))
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
      (let* ((*qubits-required-for-parallelization* 100)
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
