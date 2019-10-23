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
  (with-execution-modes (:compile :interpret :interpret-no-kernel)
    (flet ((test-size (size)
             (let* ((quil (with-output-to-quil
                            (loop :for q :below size :do
                              (format t "H ~D~%" q))))
                    (qvm (run-program size quil))
                    (expected-probability (expt 2 (- size))))
               (every (lambda (z)
                        (double-float= expected-probability (probability z) 1/10000))
                      (qvm::amplitudes qvm)))))
      (dotimes (i 10)
        (is (test-size i))))))

(deftest test-qubit-ordering-in-compiled-mode ()
  "Check that the ordering of the qubits is correctly interpreted in compiled mode."
  (let* ((quil (with-output-to-quil
                 "H 0"
                 "H 1"
                 "H 2"
                 "H 3"
                 "CNOT 2 0"
                 "CSWAP 1 3 2"))
         (qvm-interpreted (run-program 4 quil))
         (qvm-compiled    (let ((qvm:*compile-before-running* t))
                            (run-program 4 quil))))
    (is (every (lambda (a b)
                 (and (double-float= (realpart a) (realpart b) 1/10000)
                      (double-float= (imagpart a) (imagpart b) 1/10000)))
               (qvm::amplitudes qvm-interpreted)
               (qvm::amplitudes qvm-compiled)))))

(deftest test-full-rotation ()
  "Test four RX(pi/2) gates."
  (with-execution-modes (:compile :interpret :interpret-no-kernel)
    (let* ((quil (with-output-to-quil
                   (loop :repeat 4 :do
                     (format t "RX(pi/2) 0~%"))))
           (qvm (run-program 1 quil)))
      (is (double-float= 1 (probability (aref (qvm::amplitudes qvm) 0)) 1/10000)))))

(deftest test-inversion ()
  "Test |000> -> |111> inversion."
  (with-execution-modes (:compile :interpret)
    (let* ((quil (with-output-to-quil
                   (loop :for q :below 3 :do
                     (format t "X ~D~%" q))))
           (qvm (run-program 3 quil)))
      (is (double-float= 1 (probability (vector-last (qvm::amplitudes qvm))) 1/10000)))))

(deftest test-cnot-from-cz ()
  "Test the construction of a CNOT gate from a CZ gate."
  (with-execution-modes (:compile :interpret :interpret-no-kernel)
    (let* ((quil (with-output-to-quil
                   (format t "X 0~%")
                   (format t "H 1~%")
                   (format t "CZ 0 1~%")
                   (format t "H 1~%")))
           (qvm (run-program 2 quil)))
      (is (double-float= 1 (probability (vector-last (qvm::amplitudes qvm))) 1/10000)))))

(deftest test-bell ()
  "Test the construction of a Bell pair."
  (with-execution-modes (:compile :interpret :interpret-no-kernel) 
    (labels ((bell-state (n)
               "Construct an N-qubit Bell state."
               (with-output-to-quil
                 (format t "H 0~%")
                 (loop :for i :from 1 :below n :do
                   (format t "CNOT 0 ~D~%" i))))
             (run-bell (i)
               (qvm::amplitudes (run-program i (bell-state i)))))
      (loop :for i :from 1 :to 10
            :for amps := (run-bell i)
            :do (is (double-float= 1/2 (probability (vector-first amps)) 1/10000))
                (is (double-float= 1/2 (probability (vector-last amps)) 1/10000))))))

(deftest test-swap ()
  "Test |01> -> |10> by direct computation."
  (with-execution-modes (:compile :interpret)
    (let* ((qvm (make-qvm 2))
           (amps (qvm::amplitudes qvm)))
      (load-program qvm (with-output-to-quil
                          (format t "X 0~%")))
      (setf qvm (run qvm))
      (is (double-float= 1 (probability (aref amps #b01)) 1/10000))
      (load-program qvm (with-output-to-quil
                          (format t "SWAP 0 1~%")))
      (setf qvm (run qvm))
      (is (double-float= 1 (probability (aref amps #b10)) 1/10000)))))

(deftest test-parametric-gate ()
  "Test a parametric gate."
  (with-execution-modes (:compile :interpret :interpret-no-kernel)
    (let* ((qvm (make-qvm 1))
           (amps (qvm::amplitudes qvm)))
      (load-program qvm (with-output-to-quil
                          (format t "DEFGATE G(%a):~%    cos(%a), sin(%a)~%    -sin(%a), cos(%a)~%G(0.0) 0")))
      (run qvm)
      (is (double-float= 1 (probability (aref amps 0)) 1/10000)))))


(defun fourier-test-program (type)
  "Generate a test program for the QFT algorithm for two qubits. The types are as follows:

    TYPE     STATE
    ----     -----
    0        |00>
    1        |01>
    2        |10>
    3        |11>
"
  (let ((X0 (make-instance 'quil:gate-application
                           :operator #.(quil:named-operator "X")
                           :name-resolution (quil:lookup-standard-gate "X")
                           :arguments (list (quil:qubit 0))))
        (X1 (make-instance 'quil:gate-application
                           :operator #.(quil:named-operator "X")
                           :name-resolution (quil:lookup-standard-gate "X")
                           :arguments (list (quil:qubit 1))))
        (qft (qvm-examples:qft-circuit '(0 1))))
    (flet ((prepend (&rest apps)
             (setf (quil:parsed-program-executable-code qft)
                   (concatenate 'vector apps (quil:parsed-program-executable-code qft)))))
      (ecase type
        ((0)                                ; |00>
         nil)
        ((1)                                ; |01>
         (prepend X0))
        ((2)                                ; |10>
         (prepend X1))
        ((3)                                ; |11>
         (prepend X0 X1)))
      ;; Return the circuit
      qft)))

;; 1 0 0 0 => 0.5    0.5     0.5    0.5
;; 0 1 0 0 => 0.5    0.5i   -0.5   -0.5i
;; 0 0 1 0 => 0.5   -0.5     0.5   -0.5
;; 0 0 0 1 => 0.5   -0.5i   -0.5    0.5i
(deftest test-fourier ()
  "Test 2-qubit QFT."
  (with-execution-modes (:compile :interpret)
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
                   (qvm::amplitudes (run-program 2 (fourier-test-program type)))))))))
