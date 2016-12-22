;;;; tests/qvm-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-wavefunction-ordering ()
  "Test that we can get a wavefunction out in the right order with MAP-REORDERED-AMPLITUDES when no swapping has been done."
  (let* ((perm (qvm::make-identity-permutation 2))
         (q (make-instance 'qvm:quantum-virtual-machine
                           :number-of-qubits 2
                           :classical-memory-size 64
                           :amplitudes (qvm::make-vector 4 0 1 2 3)
                           :qubit-permutation perm)))
    (qvm::map-reordered-amplitudes
     0
     (lambda (i addr)
       (is (double-float= i (realpart (qvm::nth-amplitude q addr)))))
     (qvm::permutation-to-nat-tuple perm))))

(deftest test-wavefunction-ordering-swapped ()
  "Test that we can get a wavefunction out in the right order with MAP-REORDERED-AMPLITUDES when some swapping has been done."
  (let* ((perm (qvm::make-identity-permutation 3))
         (q (make-instance 'qvm:quantum-virtual-machine
                           :number-of-qubits 2
                           :classical-memory-size 64
                           ;; permutation (12) * (01) =  vvvvvvvvvvvvvvv
                           :amplitudes (qvm::make-vector 8
                                                         0 4 1 5 2 6 3 7)
                           :qubit-permutation perm)))
    (load-program q (let ((quil:*recognize-swap-specially* t))
                      (with-output-to-quil
                        (format t "SWAP 0 1~%")
                        (format t "SWAP 1 2~%"))))
    (qvm:run q)
    (qvm:map-amplitudes
     q
     (let ((i 0))
       (lambda (amp)
         (is (double-float= i (realpart amp)))
         (incf i))))))

(deftest test-defgate-persistence ()
  (let ((q1 (qvm:make-qvm 1))
        (q2 (qvm:make-qvm 1)))
    (qvm:load-program q1 (with-output-to-quil
                           "DEFGATE A:"
                           "    0, 1"
                           "    1, 0"))
    (qvm:load-program q2 (with-output-to-quil
                           "DEFGATE A:"
                           "    1, 0"
                           "    0, 1"))
    (is (not (eq (qvm::lookup-gate q1 "A")
                 (qvm::lookup-gate q2 "A"))))))
