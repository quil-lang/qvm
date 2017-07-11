;;;; wavefunction-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-map-complement-one-qubit ()
  (flet ((invariant (n)
           (lambda (ig x)
             (declare (ignore ig))
             (is (not (logbitp n x))))))
    (loop :for i :below 8 :do
      (qvm::map-complement (invariant i) 8 (qvm::nat-tuple i)))))

(deftest test-map-complement-two-qubits ()
  (flet ((invariant (m n)
           (lambda (ig x)
             (declare (ignore ig))
             (is (and (not (logbitp m x))
                      (not (logbitp n x)))))))
    (loop :for i :below 8
          :for m := i
          :for n := (mod (* 2 i) 8) :do
            (when (= m n) (incf m))
            (qvm::map-complement (invariant m n) 8 (qvm::nat-tuple m n)))))
