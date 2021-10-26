(in-package #:qvm-tests)

(deftest test-unitary-matrix-swap-from-cnots ()
  (let ((gate-prog
          (quil:parse-quil "CNOT 0 1; CNOT 1 0; CNOT 0 1"))
        (matrix
          (magicl:from-list '(1 0 0 0
                              0 0 1 0
                              0 1 0 0
                              0 0 0 1) '(4 4) :type '(complex double-float))))
    (is (magicl:= matrix
                  (qvm:parsed-program-unitary-matrix gate-prog)))))


(deftest test-unitary-qvm-error-on-measure ()
  (let ((pp
          (quil:parse-quil "CNOT 0 1; MEASURE 0"))
        (qvm (qvm:make-unitary-qvm 2)))
    (qvm:load-program qvm pp)
    (signals error
      (qvm:run qvm))))
