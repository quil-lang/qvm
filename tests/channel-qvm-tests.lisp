(in-package #:qvm-tests)


(deftest test-channel-qvm-make-instance ()
  (let* ((p1 (qvm::make-noise-pred (qvm::match-all-nq-gates 1) 12 :after))
         (p2 (qvm::make-noise-pred (qvm::match-strict-qubits 0) 18 :after))
         (p3 (qvm::make-noise-pred (qvm::match-strict-gate "X") 19 :after))
         (k1 (qvm::generate-damping-kraus-map 5 1))
         (k2 (qvm::generate-dephasing-kraus-map 5 2))
         (k3 (qvm::generate-damping-kraus-map 6 1))
         (nr1 (qvm::make-noise-rule p1 k1))
         (nr2 (qvm::make-noise-rule p2 k2 k3 k1))
         (nr3 (qvm::make-noise-rule p3 k1 k3))
         (nrs (list nr1 nr2 nr3))
         (nm (qvm::make-noise-model nrs))
         (q (make-instance 'qvm::channel-qvm :number-of-qubits 2  :noise-model nm)))
    (is (equal  (length (qvm::noise-rules (qvm::noise-model q))) 3))
    ;; Check noise rules are ordered by priority 
    (is (>= (qvm::priority (qvm::noise-predicate (nth 0 (qvm::noise-rules (qvm::noise-model q)))))
            (qvm::priority (qvm::noise-predicate (nth 1 (qvm::noise-rules (qvm::noise-model q)))))
            (qvm::priority (qvm::noise-predicate (nth 2 (qvm::noise-rules (qvm::noise-model q)))))))
    ;; Check that each noise rule has the correct number of operation elements.
    (is (equal (length (qvm::operation-elements (nth 0 (qvm::noise-rules (qvm::noise-model q))))) 2))
    (is (equal (length (qvm::operation-elements (nth 1 (qvm::noise-rules (qvm::noise-model q))))) 3))
    (is (equal (length (qvm::operation-elements (nth 2 (qvm::noise-rules (qvm::noise-model q))))) 1))))


(deftest test-rule-matches-instr-p ()
  ;; Test that RULE-MATCHES-INSTR-P correctly finds matches between
  ;; rules and instructions.
  (let* ((posn :after)
         (p1 (qvm::make-noise-pred (qvm::match-all-nq-gates 1) 12 :after))
         (p2 (qvm::make-noise-pred (qvm::match-any-n-qubits 2 '(0 2)) 18 :after))
         (kraus (qvm::generate-damping-kraus-map 5 1))
         (rule1 (qvm::make-noise-rule p1 kraus))
         (rule2 (qvm::make-noise-rule p2 kraus))
         (test-gate-1 (quil::build-gate "X" () 0))
         (test-gate-2 (quil::build-gate "CNOT" () 0 1)))
    (is (qvm::rule-matches-instr-p rule1 test-gate-1 posn))
    (is (not (qvm::rule-matches-instr-p rule1 test-gate-2 posn)))
    (is (qvm::rule-matches-instr-p rule2 test-gate-2 posn))
    (is (not (qvm::rule-matches-instr-p rule2 test-gate-1 posn)))))


(deftest test-match-rules ()
  ;; Test that MATCH-RULES correctly returns the kraus operators for
  ;; the matching rule of the instruction.
  (let* ((posn :after)
         (p1 (qvm::make-noise-pred (qvm::match-all-nq-gates 1) 12 :after))
         (p2 (qvm::make-noise-pred (qvm::match-strict-qubits 1 2) 18 :after))
         (kraus1 (qvm::generate-damping-kraus-map 5 1))
         (kraus2 (qvm::generate-damping-kraus-map 6 2))
         (rule1 (qvm::make-noise-rule p1 kraus1))
         (rule2 (qvm::make-noise-rule p2 kraus2))
         (rules (list rule1 rule2))
         (test-gate-1 (quil::build-gate "X" () 0))
         (test-gate-2 (quil::build-gate "CNOT" () 1 2))
         (test-gate-3 (quil::build-gate "CNOT" () 3 4))
         (gate-1-match (first (qvm::match-rules rules test-gate-1 posn))) 
         (gate-2-match (first (qvm::match-rules rules test-gate-2 posn))))
    ;; Check that the kraus operators returned (gate-x-match) are correct. 
    (is (every #'cl-quil::matrix-equality gate-1-match kraus1))
    (is (every #'cl-quil::matrix-equality gate-2-match kraus2))
    (is (qvm::match-rules rules test-gate-2 posn))
    (is (not (qvm::match-rules rules test-gate-3 posn)))))


(deftest test-check-povm ()
  ;; Test that check-povm rejects invalid povms, and accepts valid
  ;; ones.
  (let ((valid-povm (list .9d0 .1d0 .1d0 .9d0))
        (invalid-povm1 (list .9d0 .1d0 .2d0 .9d0))
        (invalid-povm2 (list .9 .1 .1 .2)))
    (qvm::check-povm valid-povm)
    (signals error (qvm::check-povm invalid-povm1))
    (signals error (qvm::check-povm invalid-povm2))))


(deftest test-check-kraus-ops ()
  ;; Test that CHECK-KRAUS-OPS rejects invalid kraus ops and accepts
  ;; valid ones.
  (let ((valid-k1 (qvm::generate-damping-kraus-map 5 1))
        (valid-k2 (qvm::generate-dephasing-kraus-map 5 2))
        (invalid-k1 (qvm::generate-damping-kraus-map 2 5)) ; t1 < gate-time 
        (invalid-k2 '(4 3 2 1))) ; not a magicl matrix
    (qvm::check-kraus-ops valid-k1)
    (qvm::check-kraus-ops valid-k2)
    (signals error (qvm::check-kraus-ops invalid-k1))
    (signals error (qvm::check-kraus-ops invalid-k2))
    ))


(deftest test-channel-qvm-noisy-readout ()
  ;; Test that noisy readout works on the channel qvm. Applying a
  ;; program 100 times and evaluate that the resulting excited state
  ;; population is affected by the reaodut noise.
  (let* ((nm (make-instance 'qvm::noise-model))
         (qubit 0)
         (ones 0)
         (numshots 100)
         (program "DECLARE R0 BIT; X 0; MEASURE 0 R0")
         (parsed-program (quil:parse-quil program))
         (q (make-instance 'qvm::channel-qvm :number-of-qubits 1 :noise-model nm))
         (povm (list .9d0 .1d0 .1d0 .9d0)))
    (setf (gethash 0 (qvm::readout-povms (qvm::noise-model q))) povm)
    (load-program q parsed-program :supersede-memory-subsystem t)
    (loop :repeat numshots
          :do (qvm::bring-to-zero-state (qvm::amplitudes q))
          :do (run q)
          :do (incf ones (qvm::dereference-mref q (quil:mref "R0" qubit))))
    (is (< 50 ones numshots))))
