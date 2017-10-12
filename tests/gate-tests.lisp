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

(deftest test-full-rotation ()
  "Test four RX(pi/2) gates."
    (let* ((quil (with-output-to-quil
                   (loop :repeat 4 :do
                     (format t "RX(pi/2) 0~%"))))
         (qvm (run-program 1 quil)))
    (is (double-float= 1 (probability (aref (qvm::amplitudes qvm) 0)) 1/10000))))

(deftest test-inversion ()
  "Test |000> -> |111> inversion."
  (let* ((quil (with-output-to-quil
                 (loop :for q :below 3 :do
                   (format t "X ~D~%" q))))
         (qvm (run-program 3 quil)))
    (is (double-float= 1 (probability (vector-last (qvm::amplitudes qvm))) 1/10000))))

(deftest test-cnot-from-cz ()
  "Test the construction of a CNOT gate from a CZ gate."
  (let* ((quil (with-output-to-quil
                  (format t "X 0~%")
                  (format t "H 1~%")
                  (format t "CZ 0 1~%")
                  (format t "H 1~%")))
          (qvm (run-program 2 quil)))
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

(deftest test-swap-without-optimization ()
  "Test |01> -> |10> by direct computation."
  (let* ((qvm (make-qvm 2))
         (amps (qvm::amplitudes qvm)))
    (load-program qvm (with-output-to-quil
                        (format t "X 0~%")))
    (setf qvm (run qvm))
    (is (double-float= 1 (probability (aref amps #b01)) 1/10000))
    (load-program qvm (let ((quil:*recognize-swap-specially* nil))
                        (with-output-to-quil
                          (format t "SWAP 0 1~%"))))
    (setf qvm (run qvm))
    (is (double-float= 1 (probability (aref amps #b10)) 1/10000))))

(deftest test-swap-with-optimization ()
  "Test SWAP 1 2, SWAP 2 3, relying on SWAP optimization."
  (let* ((qvm (make-qvm 4))
         (amps (qvm::amplitudes qvm)))
    (load-program qvm (with-output-to-quil
                        (format t "X 1~%")))
    (setf qvm (run qvm))
    (is (double-float= 1 (probability (aref amps #b0010)) 1/10000))

    (load-program qvm (let ((quil:*recognize-swap-specially* t))
                        (with-output-to-quil
                          (format t "SWAP 1 2~%")
                          (format t "SWAP 2 3~%"))))
    (setf qvm (run qvm))
    ;; Physical location should be the same.
    (is (double-float= 1 (probability (aref amps #b0010)) 1/10000))
    ;; Permutation should be swapped.
    (is (equalp '#(0 2 3 1) (qvm::qubit-permutation qvm)))
    ;; Mapping from logical to physical should be good.
    (is (= 0 (qvm::permuted-qubit qvm 0)))
    (is (= 2 (qvm::permuted-qubit qvm 1)))
    (is (= 3 (qvm::permuted-qubit qvm 2)))
    (is (= 1 (qvm::permuted-qubit qvm 3)))))

(deftest test-swap-randomly-with-and-without-optimization ()
  "Test random SWAP sequences with and without optimization."
  (labels ((instrs (instr-list)
             (with-output-to-quil
               (mapc #'write-line instr-list)))
           (random-swaps (n)
             (loop :while (plusp n)
                   :for q1 := (random 8)
                   :for q2 := (random 8)
                   :unless (= q1 q2)
                     :collect (progn
                                (decf n)
                                (format nil "SWAP ~D ~D" q1 q2))
                       :into swap-instrs
                   :finally (return (values
                                     (let ((quil:*recognize-swap-specially* nil))
                                       (instrs swap-instrs))
                                     (let ((quil:*recognize-swap-specially* t))
                                       (instrs swap-instrs)))))))
    (loop :with amps := (let ((vec (qvm::make-vector (expt 2 8))))
                          (map-into vec (lambda () (qvm::cflonum (random 1.0)))))
          :repeat 10
          :for qvm-no-opt := (make-qvm 8)
          :for qvm-opt    := (make-qvm 8)
          :do (multiple-value-bind (no-opt opt)
                  (random-swaps 10)
                (setf (qvm::amplitudes qvm-no-opt) (copy-seq amps))
                (setf (qvm::amplitudes qvm-opt) (copy-seq amps))
                (load-program qvm-no-opt no-opt)
                (load-program qvm-opt opt)
                (qvm:run qvm-no-opt)
                (qvm:run qvm-opt)
                (qvm:map-amplitudes
                 qvm-opt
                 (let ((i 0))
                   (lambda (amp)
                     (is (double-float=
                          (realpart amp)
                          (realpart (qvm::nth-amplitude qvm-no-opt i))))
                     (incf i))))))))

(deftest test-parametric-gate ()
  "Test a parametric gate."
  (let* ((qvm (make-qvm 1))
         (amps (qvm::amplitudes qvm)))
    (load-program qvm (with-output-to-quil
                        (format t "DEFGATE G(%a):~%    cos(%a), sin(%a)~%    -sin(%a), cos(%a)~%G(0.0) 0")))
    (run qvm)
    (is (double-float= 1 (probability (aref amps 0)) 1/10000))))


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
                           :operator "X"
                           :arguments (list (quil:qubit 0))))
        (X1 (make-instance 'quil:gate-application
                           :operator "X"
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
