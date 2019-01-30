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

(deftest test-wavefunction-qubits ()
  "Test that WAVEFUNCTION-QUBITS works."
  (is (= 1 (qvm::wavefunction-qubits (qvm::make-vector 2))))
  (is (= 2 (qvm::wavefunction-qubits (qvm::make-vector 4))))
  (is (= 3 (qvm::wavefunction-qubits (qvm::make-vector 8))))
  (is (= 4 (qvm::wavefunction-qubits (qvm::make-vector 16)))))

(deftest test-copy-wavefunction ()
  "Test that COPY-WAVEFUNCTION works."
  (let ((wf1 (qvm::make-vector 1 1))
        (wf2 (qvm::make-vector 2 1 2))
        (wf4 (qvm::make-vector 4 1 2 3 4))
        (wf8 (qvm::make-vector 8 1 2 3 4 5 6 7 8))
        (d1 (qvm::make-vector 1))
        (d2 (qvm::make-vector 2))
        (d4 (qvm::make-vector 4))
        (d8 (qvm::make-vector 8)))
    (macrolet ((test-each-combo (sources destinations)
                 `(progn
                    ;; Try each src first.
                    ,@(loop :with copy := (gensym "COPY")
                            :for src :in sources
                            :collect `(let ((,copy (qvm:copy-wavefunction ,src)))
                                        (is (every #'= ,src ,copy))))

                    ;; Now each src/dst combo.
                    ,@(loop :with i := (gensym "I")
                            :for src :in sources
                            :append (loop :for dst :in destinations
                                          :collect `(progn
                                                      (qvm:copy-wavefunction ,src ,dst)
                                                      (is (loop :for ,i :below (min (length ,src)
                                                                                    (length ,dst))
                                                                :always (= (aref ,src ,i)
                                                                           (aref ,dst ,i))))))))))
      ;; No parallelization.
      (let ((qvm:*qubits-required-for-parallelization* 10))
        (test-each-combo (wf1 wf2 wf4 wf8) (d1 d2 d4 d8)))

      ;; A lot of parallelization
      (let ((qvm:*qubits-required-for-parallelization* 0))
        (test-each-combo (wf1 wf2 wf4 wf8) (d1 d2 d4 d8))))))
