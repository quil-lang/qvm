;;;; noisy-qvm-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

 (deftest test-simple-noise ()
   "Test that the noisy gate behaves as expected."
   (let ((p (with-output-to-quil
              (write-line "X 0")))
         (tries 500)
         (results-desired
           (list
            (qvm::make-vector 2 0 -1)
            (qvm::make-vector 2 1 0)
            (qvm::make-vector 2 (complex 0 -1) 0))))
     (loop :while (and (plusp (length results-desired))
                       (plusp tries))
           :do (decf tries)
               (let* ((qvm (make-instance 'qvm::noisy-qvm :x 1/3
                                                          :y 1/3
                                                          :z 1/3
                                                          :number-of-qubits 1
                                                          :classical-memory-size 8))
                      (psi (qvm::amplitudes (progn
                                              (qvm:load-program qvm p)
                                              (qvm:run qvm)))))
                 (setf results-desired
                       (remove psi results-desired :test #'equalp))))
     (is (plusp tries))))
