;;;; channel-qvm-tests.lisp
;;;;
;;;; Author: Sophia Ponte


(in-package #:qvm-tests)

(deftest test-channel-qvm-make-instance ()
  (let* ((p1 (qvm::make-noise-predicate (qvm::match-all-nq-gates 1) 12 :after))
         (p2 (qvm::make-noise-predicate (qvm::match-strict-qubits 0) 18 :after))
         (p3 (qvm::make-noise-predicate (qvm::match-strict-gate "X") 19 :after))
         (k1 (qvm::damping-kraus-map 5 1))
         (k2 (qvm::dephasing-kraus-map 5 2))
         (k3 (qvm::damping-kraus-map 6 1))
         (nr1 (qvm::make-noise-rule p1 k1))
         (nr2 (qvm::make-noise-rule p2 k2 k3 k1))
         (nr3 (qvm::make-noise-rule p3 k1 k3))
         (nrs (list nr1 nr2 nr3))
         (nm (qvm::make-noise-model nrs))
         (q (make-instance 'qvm::channel-qvm :number-of-qubits 2  :noise-model nm)))
    (is (= 3 (length (qvm::noise-rules (qvm::noise-model q)))))
    ;; Check noise rules are ordered by priority
    (is (>= (qvm::priority (qvm::noise-predicate 
                            (nth 0 (qvm::noise-rules (qvm::noise-model q)))))
            (qvm::priority (qvm::noise-predicate 
                            (nth 1 (qvm::noise-rules (qvm::noise-model q)))))
            (qvm::priority (qvm::noise-predicate 
                            (nth 2 (qvm::noise-rules (qvm::noise-model q)))))))
    ;; Check that each noise rule has the correct number of operation elements.
    (is (= 2 (length (qvm::operation-elements 
                      (nth 0 (qvm::noise-rules (qvm::noise-model q)))))))
    (is (= 3 (length (qvm::operation-elements 
                      (nth 1 (qvm::noise-rules (qvm::noise-model q)))))))
    (is (= 1 (length (qvm::operation-elements 
                      (nth 2 (qvm::noise-rules (qvm::noise-model q))))))))
  ;; Test channel-qvm with no noise-model
  (let ((channel-qvm (make-instance 'channel-qvm :number-of-qubits 2))
        (program (quil:parse-quil "DECLARE R0 BIT; H 0; CNOT 0 1")))
    (load-program channel-qvm program :supersede-memory-subsystem t)
    (run channel-qvm))
  (signals error (make-instance 'channel-qvm :number-of-qubits 2 :noise-model nil)))

(deftest test-rule-matches-instr-p ()
  ;; Test that RULE-MATCHES-INSTR-P correctly finds matches between
  ;; rules and instructions.
  (let* ((posn :after)
         (p1 (qvm::make-noise-predicate (qvm::match-all-nq-gates 1) 12 :after))
         (p2 (qvm::make-noise-predicate (qvm::match-any-n-qubits 2 '(0 2)) 18 :after))
         (kraus (qvm::damping-kraus-map 5 1))
         (rule1 (qvm::make-noise-rule p1 kraus))
         (rule2 (qvm::make-noise-rule p2 kraus))
         (test-gate-1 (quil::build-gate "X" () 0))
         (test-gate-2 (quil::build-gate "CNOT" () 0 1)))
    (is (qvm::rule-matches-instr-p rule1 test-gate-1 posn))
    (is (not (qvm::rule-matches-instr-p rule1 test-gate-2 posn)))
    (is (qvm::rule-matches-instr-p rule2 test-gate-2 posn))
    (is (not (qvm::rule-matches-instr-p rule2 test-gate-1 posn)))))

(deftest test-find-matching-rule ()
  ;; Test that FIND-MATCHING-RULE correctly returns the matching rule
  ;; of the instruction.
  (let* ((posn :after)
         (p1 (qvm::make-noise-predicate (qvm::match-all-nq-gates 1) 12 posn))
         (p2 (qvm::make-noise-predicate (qvm::match-strict-qubits 1 2) 18 posn))
         (kraus1 (qvm::damping-kraus-map 5 1))
         (kraus2 (qvm::damping-kraus-map 6 2))
         (rule1 (qvm::make-noise-rule p1 kraus1))
         (rule2 (qvm::make-noise-rule p2 kraus2))
         (rules (list rule1 rule2))
         (test-gate-1 (quil::build-gate "X" () 0))
         (test-gate-2 (quil::build-gate "CNOT" () 1 2))
         (test-gate-3 (quil::build-gate "CNOT" () 3 4))
         (rule-1-match (qvm::find-matching-rule rules test-gate-1 posn))
         (rule-2-match (qvm::find-matching-rule rules test-gate-2 posn)))
    ;; Check that the kraus operators returned (gate-x-match) are correct.
    (is (every #'quil::matrix-equality (first (qvm::operation-elements rule-1-match)) kraus1))
    (is (every #'quil::matrix-equality (first (qvm::operation-elements rule-2-match)) kraus2))
    (is (qvm::find-matching-rule rules test-gate-2 posn))
    (is (not (qvm::find-matching-rule rules test-gate-3 posn)))))

(deftest test-check-povm ()
  ;; Test that check-povm rejects invalid povms, and accepts valid
  ;; ones.
  (let ((valid-povm '(.9d0 .1d0 .1d0 .9d0))
        (invalid-povm1 '(.9d0 .1d0 .2d0 .9d0))
        (invalid-povm2 '(.9 .1 .1 .2)))
    (qvm::check-povm valid-povm)
    (signals error (qvm::check-povm invalid-povm1))
    (signals error (qvm::check-povm invalid-povm2))))

(deftest test-check-kraus-ops ()
  ;; Test that CHECK-KRAUS-OPS rejects invalid kraus ops and accepts
  ;; valid ones.
  (let ((valid-k1 (qvm::damping-kraus-map 5 1))
        (invalid-k2 '(4 3 2 1))) ; not a magicl matrix
    (qvm::check-kraus-ops valid-k1)
    (signals error 
      (qvm::check-kraus-ops invalid-k2))))

(defun run-n-shot-program (nunshots qvm program)
  "Runs an NUMSHOT-shot PROGRAM on the QVM, and returns the number of times that q0 was measured to be 1"
  (let ((ones 0)
        (parsed-program (quil::parse-quil program))
        (qubit 0))
    (load-program qvm parsed-program :supersede-memory-subsystem t)
    (loop :repeat nunshots
          :do (qvm::bring-to-zero-state (qvm::amplitudes qvm))
          :do (run qvm)
          :do (incf ones (qvm::dereference-mref qvm (quil:mref "R0" qubit))))
    ones))

(deftest test-channel-qvm-noisy-readout ()
  ;; Test that noisy readout works on the channel qvm. Applying a
  ;; program 100 times and evaluate that the resulting excited state
  ;; population is affected by the reaodut noise.
  (with-execution-modes (:interpret)
    (let* ((nm (make-instance 'qvm::noise-model))
           (qubit 0)
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0")
           (q (make-instance 'qvm::channel-qvm :number-of-qubits 1 :noise-model nm))
           (povm (list .9d0 .1d0 .1d0 .9d0)))
      (setf (gethash qubit (qvm::readout-povms (qvm::noise-model q))) povm)
      (let ((ones-measured (run-n-shot-program numshots q program)))
        (is (< 50 ones-measured numshots))))))

(deftest test-channel-noise-model-priority ()
  ;; This function tests that the channel-qvm correctly applies rules
  ;; according to their priorities. The noise model in this example
  ;; consists of two rules that both match the gate in the
  ;; program. The lower priority rule has stronger noise (kraus
  ;; operators) than the higher priority rule. We want to ensure that
  ;; the higher priority rule is applied.
  (with-execution-modes (:interpret)
    (let* ((low-pred (qvm::make-noise-predicate (qvm::match-all-nq-gates 1) 18 :after))
           (high-pred (qvm::make-noise-predicate (qvm::match-strict-gate "X") 1 :after))
           (low-kraus (qvm::damping-kraus-map 5 4))
           (high-kraus (qvm::damping-kraus-map 5 1))
           (nrs (list (qvm::make-noise-rule low-pred low-kraus) ; low priority
                      (qvm::make-noise-rule high-pred high-kraus))) ; high priority 
           (nm (make-instance 'qvm::noise-model :noise-rules nrs))
           (q (make-instance 'qvm::channel-qvm :number-of-qubits 1 :noise-model nm))
           (numshots 100)
           (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
      ;; if the lower priority rule were applied, ones would be <
      ;; 50. The higher priority rule, which is the one that we want to
      ;; apply, should make ones > 50
      (let ((ones-measured (run-n-shot-program numshots q program)))
        (is (> 70 ones-measured))))))

(deftest test-noise-model-multiplication-names ()
  ;; This test checks that the noise model produced by a
  ;; multiplication has the correct name. The two noise models are
  ;; given names from their predicates "Q0-NOISE" and
  ;; "X-GATE-NOISE". We test that the noise model constructed from the
  ;; multiplication is given the name "Q0-NOISE & X-GATE-NOISE".
  (let* ((p1 (qvm::make-noise-predicate (qvm::match-strict-qubits 0) 18 :after "Q0-NOISE"))
         (p2 (qvm::make-noise-predicate (qvm::match-strict-gate "X") 1 :after "X-GATE-NOISE"))
         (k1 (qvm::damping-kraus-map 5 4))
         (k2 (qvm::damping-kraus-map 5 1))
         (nr1 (qvm::make-noise-rule p1 k1))
         (nr2 (qvm::make-noise-rule p2 k2))
         (nm1 (make-instance 'qvm::noise-model :noise-rules (list nr1)))
         (nm2 (make-instance 'qvm::noise-model :noise-rules (list nr2)))
         (total-model (qvm::multiply-noise-models nm1 nm2))
         (q (make-instance 'qvm::channel-qvm :number-of-qubits 2 :noise-model total-model))
         (parsed-program (quil:parse-quil "DECLARE R0 BIT; X 0; Y 1; MEASURE 0 R0"))
         (noisy-prog-strings (noisy-program-strings parsed-program q)))
    (is (string= (nth 1 noisy-prog-strings) "(Q0-NOISE & X-GATE-NOISE)"))))

(defun make-test-noise-model  ()
  (let* ((priority 10)
         (position :after)
         (np (qvm::make-noise-predicate (qvm::match-all-gates) priority position))
         (depol-prob .2)
         (kraus (qvm::depolarizing-kraus-map depol-prob))
         (noise-rules (list (qvm::make-noise-rule np kraus))))
    (qvm::make-noise-model noise-rules)))

(deftest test-channel-qvm-with-density-matrix ()
 ;; Test that a depolarizing NOISE-MODEL applied to the CHANNEL-QVM
 ;; with a DENSITY-MATRIX-STATE correctly depolarizes the state.
  (let* ((noise-model (qvm-tests::make-test-noise-model))
         (num-qubits 2)
         (numshots 100)
         (dms (qvm::make-density-matrix-state num-qubits))
         (dms-channel-qvm (make-instance 'channel-qvm :number-of-qubits num-qubits
                                                      :state dms
                                                      :noise-model noise-model))
         (program "DECLARE R0 BIT; X 0; MEASURE 0 R0"))
    (let ((ones-measured (qvm-tests::run-n-shot-program numshots dms-channel-qvm program)))
      (is (< ones-measured numshots)))))

(defun noisy-program-strings (parsed-program qvm)
  "Return a list of strings representing a noisy program. The returned list consists of the original program instructions as strings interjected with the NOISE-PRED NAMEs of the NOISE-RULES in the QVM's NOISE-MODEL."
  (let ((parsed-instructions (quil::parsed-program-executable-code parsed-program))
        (rules (qvm::noise-rules (qvm::noise-model qvm))))
    (loop :for instr :across parsed-instructions
          :for rule-before-instr := (qvm::find-matching-rule rules instr :before)
          :for rule-after-instr := (qvm:: find-matching-rule rules instr :after)
          :when rule-before-instr
            :collect (qvm::name (qvm::noise-predicate rule-before-instr))
          :collect (quil::print-instruction-to-string instr)
          :when rule-after-instr
            :collect (qvm::name (qvm::noise-predicate rule-after-instr)))))

(deftest test-print-noisy-program ()
  ;; This function tests that QVM::NOISY-PROG-STRINGS correctly
  ;; prints the instructions of a qvm's program interjected with the
  ;; "noise instructions" from the QVM's NOISE-MODEL.
  (let* ((pred (qvm::make-noise-predicate (qvm::match-all-nq-gates 1) 1 :after))
         (kraus (qvm::damping-kraus-map 5 1))
         (nrs (list (qvm::make-noise-rule pred kraus)))
         (nm (make-instance 'qvm::noise-model :noise-rules nrs))
         (q (make-instance 'qvm::channel-qvm :number-of-qubits 2 :noise-model nm))
         (parsed-prog (quil:parse-quil "DECLARE R0 BIT; X 0; Z 1; MEASURE 0 R0"))
         (noisy-prog-strings (noisy-program-strings parsed-prog q)))
    (is (string= (nth 0 noisy-prog-strings) "X 0"))
    (is (string= (nth 1 noisy-prog-strings) "NOISE-NAME"))
    (is (string= (nth 2 noisy-prog-strings) "Z 1"))
    (is (string= (nth 3 noisy-prog-strings) "NOISE-NAME"))
    (is (string= (nth 4 noisy-prog-strings) "MEASURE 0 R0[0]"))))
