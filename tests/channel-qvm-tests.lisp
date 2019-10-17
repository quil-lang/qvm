(in-package #:qvm-tests)


(deftest test-match-strict-gate ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))
    (is (funcall (qvm::match-strict-gate "X") x))
    (is (not (funcall (qvm::match-strict-gate "X") cnot)))))


(deftest test-match-all-nq-gates ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))
    (is (funcall (qvm::match-all-nq-gates 2) cnot))
    (is (not (funcall (qvm::match-all-nq-gates 2) x)))))


(deftest test-match-all-gates ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))
    (is (funcall (qvm::match-all-gates) x))
    (is (funcall (qvm::match-all-gates) cnot))
    (is (not (funcall (qvm::match-all-gates) measure)))))


(deftest test-match-measure ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))
    (is (funcall (qvm::match-measure) measure))
    (is (not (funcall (qvm::match-measure) x)))))


(deftest test-match-measure-at-strict ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))
    (is (funcall (qvm::match-measure-at-strict 0) measure))
    (is (not (funcall (qvm::match-measure-at-strict 1) measure)))
    (is (not (funcall (qvm::match-measure-at-strict 0) x)))))


(deftest test-match-measure-at-any ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))

    (is (funcall (qvm::match-measure-at-any 0 1) measure))
    (is (not (funcall (qvm::match-measure-at-any 1 2) measure)))))


(deftest test-match-any-gates ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))
    (is (funcall (qvm::match-any-gates "X") x))
    (is (not (funcall (qvm::match-any-gates "X Y") cnot)))
    (is (funcall (qvm::match-any-gates "X" "Y" "Z") x))))


(deftest test-match-strict-qubits ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))
    (is (funcall (qvm::match-strict-qubits 0 1) cnot))
    (is (funcall (qvm::match-strict-qubits 0) x))
    (is (not (funcall (qvm::match-strict-qubits 0 1) x)))))


(deftest test-match-any-n-qubits ()
  (let* ((program "DECLARE R0 BIT; X 0; CNOT 0 1;  MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (x (quil::nth-instr 0 parsed-program))
         (cnot (quil::nth-instr 1 parsed-program))
         (measure (quil::nth-instr 2 parsed-program)))
    (is (funcall (qvm::match-any-n-qubits 2 (list 0 1 2)) cnot))
    (is (funcall (qvm::match-any-n-qubits 1 (list 0 1)) x))
    (is (not (funcall (qvm::match-any-n-qubits 1 (list 2)) x)))))


(deftest test-make-noise-predicate ()
  (let* ((pred (qvm::match-strict-qubits 0))
         (priority 1)
         (noise-position :after)
         (noise-pred (qvm::make-noise-pred pred priority noise-position)))
    (is (qvm::priority noise-pred) 1)
    (is (qvm::noise-position noise-pred) :after)))


(deftest test-predicate-and ()
  (let* ((p1 (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 :after))
         (p2 (qvm::make-noise-pred (qvm::match-strict-gate "X") 2 :before))
         (and-p (qvm::predicate-and p1 p2))
         (test-gate (quil::build-gate "X" () 0)))
    (is (qvm::priority and-p) 1)
    (is (qvm::priority and-p) ':after)
    (is (funcall (qvm::predicate and-p) test-gate))))


(deftest test-predicate-or ()
  (let* ((p1 (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 ':after))
         (p2 (qvm::make-noise-pred (qvm::match-strict-gate "X") 2 ':before))
         (or-p (qvm::predicate-or p1 p2))
         (test-gate-1 (quil::build-gate "X" () 1))
         (test-gate-2 (quil::build-gate "Y" () 0)))
    (is (qvm::priority or-p) 1)
    (is (qvm::priority or-p) ':after)
    (is (funcall (qvm::predicate or-p) test-gate-1))
    (is (funcall (qvm::predicate or-p) test-gate-2))))


(deftest test-predicate-not ()
  (let* ((p (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 ':after))
         (not-p (qvm::predicate-not p))
         (test-gate (quil::build-gate "Y" () 1)))
    (is (qvm::priority not-p) 1)
    (is (qvm::priority not-p) ':after)
    (is (funcall (qvm::predicate not-p) test-gate))))


(deftest test-make-noise-rule ()
  (let* ((np (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 ':after))
         (op-elems (qvm::generate-damping-kraus-ops 1 5))
         (nr (qvm::make-noise-rule np op-elems)))
    (is (equalp (qvm::operation-elements nr) op-elems))))


(defun pred-1 ()
  (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 ':after))

(defun pred-3 ()
  (qvm::make-noise-pred (qvm::match-all-nq-gates 1) 5 ':after))

(defun pred-2 ()
  (qvm::make-noise-pred (qvm::match-strict-gate "X") 2 ':after))


(defun op-elems ()
  (qvm::generate-damping-kraus-ops 1 5))


(defun readout-povms ()
  (let ((povm-table (make-hash-table)))
    (setf (gethash 0 povm-table) (list .9d0 .1d0 .1d0 .9d0))
    (setf (gethash 1 povm-table) (list .8d0 .2d0 .2d0 .8d0))
    povm-table)) 


(deftest test-min-rule-priority-p ()
  (let ((nr1 (qvm::make-noise-rule (pred-1) (op-elems)))
        (nr2 (qvm::make-noise-rule (pred-2) (op-elems))))
    (is (qvm::min-rule-priority-p nr1 nr2))))


(deftest test-make-noise-model ()
  (let* ((nr1 (qvm::make-noise-rule (pred-1) (op-elems)))
         (nr2 (qvm::make-noise-rule (pred-2) (op-elems)))
         (nm (qvm::make-noise-model (list nr1 nr2))))
    (setf (qvm::qubit-povm nm 0) (list .9d0 .1d0 .1d0 .9d0))
    (setf (qvm::qubit-povm nm 1) (list .8d0 .2d0 .2d0 .8d0))
                                        ; check that rules are sorted in priority order
    (is (< (qvm::priority (qvm::noise-predicate (nth 0 (qvm::noise-rules nm)))) (qvm::priority (qvm::noise-predicate  (nth 1 (qvm::noise-rules nm))))))))


(deftest test-add-noise-models ()
  (let* ((nr1 (qvm::make-noise-rule (pred-1) (op-elems)))
         (nr2 (qvm::make-noise-rule (pred-3) (op-elems)))
         (nr3 (qvm::make-noise-rule (pred-2) (op-elems)))
         (nm1 (qvm::make-noise-model (list nr1 nr2)))
         (nm2 (qvm::make-noise-model (list nr3)))
         (tot-nm (qvm::add-noise-models nm1 nm2)))
    (is (= (length (qvm::noise-rules tot-nm)) 3))
    (is (< (qvm::priority (qvm::noise-predicate (nth 0 (qvm::noise-rules tot-nm)))) (qvm::priority (qvm::noise-predicate  (nth 1 (qvm::noise-rules tot-nm))))))
    (is (< (qvm::priority (qvm::noise-predicate (nth 1 (qvm::noise-rules tot-nm)))) (qvm::priority (qvm::noise-predicate  (nth 2 (qvm::noise-rules tot-nm))))))))


(deftest test-multiply-noise-models ()
  (let* ((nr1 (qvm::make-noise-rule (pred-1) (op-elems)))
         (nr2 (qvm::make-noise-rule (pred-3) (op-elems)))
         (nr3 (qvm::make-noise-rule (pred-2) (op-elems)))
         (nm1 (qvm::make-noise-model (list nr1 nr2)))
         (nm2 (qvm::make-noise-model (list nr3)))
         (tot-nm (qvm::multiply-noise-models nm1 nm2)))
    (is (= (length (qvm::noise-rules tot-nm)) 6))
    (is (<= (qvm::priority (qvm::noise-predicate (nth 0 (qvm::noise-rules tot-nm)))) (qvm::priority (qvm::noise-predicate  (nth 1 (qvm::noise-rules tot-nm))))))
    (is (<= (qvm::priority (qvm::noise-predicate (nth 1 (qvm::noise-rules tot-nm)))) (qvm::priority (qvm::noise-predicate  (nth 2 (qvm::noise-rules tot-nm))))))
    (is (<= (qvm::priority (qvm::noise-predicate (nth 4 (qvm::noise-rules tot-nm)))) (qvm::priority (qvm::noise-predicate  (nth 5 (qvm::noise-rules tot-nm))))))))
