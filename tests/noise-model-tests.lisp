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
    (is (funcall (qvm::match-any-n-qubits 2 '(0 1 2)) cnot))
    (is (funcall (qvm::match-any-n-qubits 1 '(0 1)) x))
    (is (not (funcall (qvm::match-any-n-qubits 1 '(2)) x)))))


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
         (test-gate-x0 (quil::build-gate "X" () 0))
         (test-gate-x1 (quil::build-gate "X" () 1)))
    (is (qvm::priority and-p) 1)
    (is (qvm::priority and-p) ':after)
    (is (funcall (qvm::predicate and-p) test-gate-x0))
    (is (not (funcall (qvm::predicate and-p) test-gate-x1)))))


(deftest test-predicate-or ()
  (let* ((p1 (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 ':after))
         (p2 (qvm::make-noise-pred (qvm::match-strict-gate "X") 2 ':before))
         (or-p (qvm::predicate-or p1 p2))
         (test-gate-1 (quil::build-gate "X" () 1))
         (test-gate-2 (quil::build-gate "Y" () 0))
         (test-gate-3 (quil::build-gate "CNOT" () 0 1)))
    (is (qvm::priority or-p) 1)
    (is (qvm::priority or-p) ':after)
    (is (funcall (qvm::predicate or-p) test-gate-1))
    (is (funcall (qvm::predicate or-p) test-gate-2))
    (is (not (funcall (qvm::predicate or-p) test-gate-3)))))


(deftest test-predicate-not ()
  (let* ((p (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 ':after))
         (not-p (qvm::predicate-not p))
         (test-gate-1 (quil::build-gate "Y" () 1))
         (test-gate-2 (quil::build-gate "Z" () 0)))
    (is (qvm::priority not-p) 19)
    (is (qvm::noise-position not-p) ':after)
    (is (funcall (qvm::predicate not-p) test-gate-1))
    (is (not (funcall (qvm::predicate not-p) test-gate-2)))))


(deftest test-make-noise-rule ()
  (let* ((np (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 ':after))
         (kraus-map-1 (qvm::generate-damping-kraus-map 5 1))
         (kraus-map-2 (qvm::generate-damping-dephasing-kraus-map 6 2))
         (nr (qvm::make-noise-rule np kraus-map-1 kraus-map-2)))
    (loop :for mapi :in (qvm::operation-elements nr)
          :for mapj :in (list kraus-map-1 kraus-map-2)
          :do (is (every #'cl-quil::matrix-equality mapi mapj)))))


(defun pred-1 ()
  (qvm::make-noise-pred (qvm::match-strict-qubits 0) 1 ':after))

(defun pred-3 ()
  (qvm::make-noise-pred (qvm::match-all-nq-gates 1) 5 ':after))

(defun pred-2 ()
  (qvm::make-noise-pred (qvm::match-strict-gate "X") 2 ':after))

(defun op-elems ()
  (qvm::generate-damping-kraus-map 5 1))

(defun readout-povms ()
  (let ((povm-table (make-hash-table)))
    (setf (gethash 0 povm-table) '(.9d0 .1d0 .1d0 .9d0))
    (setf (gethash 1 povm-table) '(.8d0 .2d0 .2d0 .8d0))
    povm-table)) 


(deftest test-rule-priority>= ()
  (let ((nr1 (qvm::make-noise-rule (pred-1) (op-elems)))
        (nr2 (qvm::make-noise-rule (pred-2) (op-elems))))
    (is (qvm::rule-priority>= nr2 nr1))))


(deftest test-make-noise-model ()
  (let* ((nr1 (qvm::make-noise-rule (pred-1) (op-elems)))
         (nr2 (qvm::make-noise-rule (pred-2) (op-elems)))
         (nm (qvm::make-noise-model (list nr1 nr2))))
    (setf (qvm::qubit-povm nm 0) '(.9d0 .1d0 .1d0 .9d0))
    (setf (qvm::qubit-povm nm 1) '(.8d0 .2d0 .2d0 .8d0))
    ;; check that rules are sorted in priority order
    (is (>= (qvm::priority (qvm::noise-predicate (nth 0 (qvm::noise-rules nm)))) 
            (qvm::priority (qvm::noise-predicate  (nth 1 (qvm::noise-rules nm))))))))


(deftest test-add-noise-models ()
  ;; Check that adding noise models produces a correct 'sum' noise
  ;; model. The priority should be the highest priority from the two
  ;; original models, and the noise rules should be the sume of the
  ;; noise rules of the two original models.
  (let* ((nr1 (qvm::make-noise-rule (pred-1) (op-elems)))
         (nr2 (qvm::make-noise-rule (pred-3) (op-elems)))
         (nr3 (qvm::make-noise-rule (pred-2) (op-elems)))
         (nm1 (qvm::make-noise-model (list nr1 nr2)))
         (nm2 (qvm::make-noise-model (list nr3)))
         (tot-nm (qvm::add-noise-models nm1 nm2)))
    (is (= (length (qvm::noise-rules tot-nm)) 3))
    (is (>= (qvm::priority (qvm::noise-predicate (nth 0 (qvm::noise-rules tot-nm)))) 
            (qvm::priority (qvm::noise-predicate  (nth 1 (qvm::noise-rules tot-nm))))))
    (is (>= (qvm::priority (qvm::noise-predicate (nth 1 (qvm::noise-rules tot-nm)))) 
            (qvm::priority (qvm::noise-predicate  (nth 2 (qvm::noise-rules tot-nm))))))))


(deftest test-multiply-noise-models-simple ()
  ;; Test that multiplying two noise simple noise models produces the
  ;; correct result noise model.
  (let* ((priority 20)
         (p1 (qvm::make-noise-pred (qvm::match-strict-qubits 0) priority ':after))
         (p2 (qvm::make-noise-pred (qvm::match-strict-qubits 1) priority ':after))
         (nr1 (qvm::make-noise-rule p1 (op-elems)))
         (nr2 (qvm::make-noise-rule p2 (op-elems)))
         (nm1 (qvm::make-noise-model (list nr1)))
         (nm2 (qvm::make-noise-model (list nr2)))
         (mult-nm (qvm::multiply-noise-models nm1 nm2)))
    (is (= (length (qvm::noise-rules mult-nm)) 3))
    ;; New noise rules should be (not p1 and p2 with p2's op elems),
    ;; (not p2 and p1 with p1's elems), and (p2 and p1 with both p2
    ;; and p1's elems).
    (let ((first-rule (nth 0 (qvm::noise-rules mult-nm)))
          (second-rule (nth 1 (qvm::noise-rules mult-nm)))
          (third-rule (nth 2 (qvm::noise-rules mult-nm))))
      (is  (= priority (qvm::priority (qvm::noise-predicate first-rule))))
      (is  (= priority (qvm::priority (qvm::noise-predicate second-rule))))
      (is  (= priority (qvm::priority (qvm::noise-predicate third-rule))))
      (is  (= 1 (length (qvm::operation-elements first-rule))))
      (is  (= 1 (length (qvm::operation-elements second-rule))))
      (is  (= 2 (length (qvm::operation-elements third-rule)))))))


(deftest test-multiply-noise-models ()
  ;; Test that multiplying two more complex noise models produces the
  ;; correct result noise model.
  (let* ((priority 20)
         (p1 (qvm::make-noise-pred (qvm::match-strict-qubits 0) priority ':after))
         (p2 (qvm::make-noise-pred (qvm::match-strict-qubits 1) priority ':after))
         (nr1 (qvm::make-noise-rule p1 (op-elems) (op-elems)))
         (nr2 (qvm::make-noise-rule p2 (op-elems) (op-elems)))
         (nm1 (qvm::make-noise-model (list nr1)))
         (nm2 (qvm::make-noise-model (list nr2)))
         (mult-nm (qvm::multiply-noise-models nm1 nm2)))
    (is (= (length (qvm::noise-rules mult-nm)) 3))
    ;; New noise rules should be (not p1 and p2 with p2's op elems),
    ;; (not p2 and p1 with p1's elems), and (p2 and p1 with both p2
    ;; and p1's elems).
    (let ((first-rule (nth 0 (qvm::noise-rules mult-nm)))
          (second-rule (nth 1 (qvm::noise-rules mult-nm)))
          (third-rule (nth 2 (qvm::noise-rules mult-nm))))
      (is  (= priority  (qvm::priority (qvm::noise-predicate first-rule))))
      (is  (= priority  (qvm::priority (qvm::noise-predicate second-rule))))
      (is  (= priority  (qvm::priority (qvm::noise-predicate third-rule))))
      (is  (= 2 (length (qvm::operation-elements first-rule))))
      (is  (= 2 (length (qvm::operation-elements second-rule))))
      (is  (= 4 (length (qvm::operation-elements third-rule)))))))
