;;;; tests/gate-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(defun vector-first (a)
  (aref a 0))

(defun vector-last (a)
  (aref a (1- (length a))))

(deftest test-hadamard ()
  "Test Hadamard initialization on several qubits."
  (flet ((test-size (size)
           (let* ((quil (with-output-to-quil
                          (loop :for q :below size :do
                            (format t "H ~D~%" q))))
                  (qvm (run-program size quil))
                  (expected-probability (expt 2 (- size))))
             (every (lambda (z)
                      (double-float= expected-probability (probability z) 1/10000))
                    (qvm::amplitudes qvm)))))
    (dotimes (i 7)
      (is (test-size i)))))

(deftest test-inversion ()
  "Test |000> -> |111> inversion."
  (let* ((quil (with-output-to-quil
                 (loop :for q :below 3 :do
                   (format t "X ~D~%" q))))
         (qvm (run-program 3 quil)))
    (is (double-float= 1 (probability (vector-last (qvm::amplitudes qvm))) 1/10000))))

(deftest test-bell ()
  "Test the construction of a Bell pair."
  (labels ((bell-state (n)
             "Construct an N-qubit Bell state."
             (with-output-to-quil
               (format t "H 0~%")
               (loop :for i :from 1 :below n :do
                 (format t "CNOT 0 ~D~%" i))))
           (run-bell (i)
             (qvm::amplitudes (run-program i (bell-state i)))))
    (loop :for i :from 1 :to 7 
          :for amps := (run-bell i)
          :do (is (double-float= 1/2 (probability (vector-first amps)) 1/10000))
              (is (double-float= 1/2 (probability (vector-last amps)) 1/10000)))))

(deftest test-swap ()
  "Test |01> -> |10>."
  (let* ((qvm (make-qvm 2))
         (amps (qvm::amplitudes qvm)))
    (load-program qvm (with-output-to-quil
                        (format t "X 0~%")))
    (setf qvm (run qvm))
    (is (double-float= 1 (probability (aref amps #b01)) 1/10000))
    (load-program qvm (with-output-to-quil
                        (format t "SWAP 0 1~%")))
    (setf qvm (run qvm))
    (is (double-float= 1 (probability (aref amps #b10)) 1/10000))))


(defun fourier-test-program (type)
  "Generate a test program for the QFT algorithm for two qubits. The types are as follows:

    TYPE     STATE
    ----     -----
    0        |00>
    1        |01>
    2        |10>
    3        |11>
"
  (with-output-to-quil
    (ecase type
      ((0)                                ; |00>
       nil)
      ((1)                                ; |01>
       (format t "X 0~%"))
      ((2)                                ; |10>
       (format t "X 1~%"))
      ((3)                                ; |11>
       (format t "X 0~%X 1~%")))
    ;; Write out
    (flet ((f (x) (format t "~{~A~^ ~}~%" x)))
      (mapc #'f (qvm-examples:qft-circuit '(0 1))))))

;; 1 0 0 0 => 0.5    0.5     0.5    0.5
;; 0 1 0 0 => 0.5    0.5i   -0.5   -0.5i
;; 0 0 1 0 => 0.5   -0.5     0.5   -0.5
;; 0 0 0 1 => 0.5   -0.5i   -0.5    0.5i
(deftest test-fourier ()
  "Test 2-qubit QFT."
  (let ((expected #(#(0.5 0.5 0.5 0.5)
                    #(0.5 #C(0.0 0.5) -0.5 #C (0.0 -0.5))
                    #(0.5 -0.5 0.5 -0.5)
                    #(0.5 #C(0.0 -0.5) -0.5 #C(0.0 0.5)))))
    (dotimes (type 4)
      (is (every (lambda (z w)
                   (and (absolute-float= (realpart z)
                                         (realpart w)
                                         1/10000)
                        (absolute-float= (imagpart z)
                                         (imagpart w)
                                         1/10000)))
                 (aref expected type)
                 (qvm::amplitudes (run-program 2 (fourier-test-program type))))))))
