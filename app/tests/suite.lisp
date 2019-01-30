(in-package :qvm-app-tests)

(defmacro mute (&body body)
  `(let ((*standard-output* (make-broadcast-stream))
         (*trace-output* (make-broadcast-stream)))
     ,@body))

(defun run-qvm-app-tests (&key (verbose nil) (headless nil))
  "Run all qvm-app tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (cond
    ((null headless)
     (run-package-tests :package ':qvm-app-tests
                        :verbose verbose
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':qvm-app-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))


(deftest test-remapping ()
  (let* ((quil (cl-quil:parse-quil-string "X 1
PRAGMA ADD-KRAUS X 1 \"(1.0 0.0 0.0 1.0)\"
PRAGMA READOUT-POVM 1 \"(0.9 0.2 0.1 0.8)\"
DECLARE ro BIT[1]
MEASURE 1 ro[1]"))
         (processed-quil (qvm-app::process-quil quil))
         (code (cl-quil:parsed-program-executable-code processed-quil))
         (gate-app (aref code 0))
         (add-kraus (aref code 1))
         (readout-povm (aref code 2))
         (measure (aref code 3)))
    ;; test gate application remapped
    (is (= 0
           (cl-quil:qubit-index
            (first (cl-quil:application-arguments gate-app)))))
    ;; test kraus pragma remapped
    (is (= 0
           (first (cl-quil:pragma-qubit-arguments add-kraus))))
    ;; test readout noise pragma remapped
    (is (= 0
           (cl-quil:pragma-qubit-index readout-povm)))
    ;; test types of povm probabilities
    (is (typep (first (cl-quil:pragma-matrix-entries readout-povm))
               'double-float))
    ;; test measurement qubit remapped
    (is (= 0
           (cl-quil:qubit-index
            (cl-quil:measurement-qubit measure))))))

(deftest test-multishot-measure-remapping ()
  (dolist (simulation-method '(qvm-app::pure-state qvm-app::full-density-matrix))
    (let* ((qubits '(1 2 3))
           (quil-code "X 1
X 3
I 2")
           (quil (cl-quil:parse-quil-string quil-code)))
      ;; process-quil modifies the program so we need to re-parse the source.
      (multiple-value-bind (processed-quil relabeling)
          (mute (qvm-app::process-quil (cl-quil:parse-quil-string quil-code)))
        (let ((results-1
                (mute
                  (qvm-app::perform-multishot-measure simulation-method quil 4 qubits 1 nil)))
              (results-2
                (mute
                  (qvm-app::perform-multishot-measure simulation-method processed-quil 3 qubits 1 relabeling))))
          (is (equalp results-1 results-2)))))))

;;; The behavior of a MEASURE operation on a qubit outside of the
;;; range of the QAM is undefined. This test checks the current
;;; behavior of the PURE-STATE-QVM for such measurements (the measured
;;; value is 0, and there is no change to the wavefunction), but in
;;; general it seems like a bad idea. Nonetheless, since there's the
;;; word "bug" in the test name, I'm leaving it.
(deftest test-multishot-measure-more-qubits-bug ()
  ;; Version #1
  (multiple-value-bind (p relabeling) (qvm-app::process-quil
                                       (qvm-app::safely-parse-quil-string "X 0"))
    (let ((answer (mute
                   (qvm-app::perform-multishot-measure 'qvm-app::pure-state p 1 '(0 1) 10 relabeling))))
      (is (every (lambda (result) (and (= 2 (length result))
                                       (= 1 (first result))
                                       (= 0 (second result))))
                 answer))))
  ;; Version #2
  (let* ((p (qvm-app::safely-parse-quil-string "X 0"))
         (q (qvm:make-qvm (quil::qubits-needed p))))
    (qvm:load-program q p)
    (qvm:run q)
    (let ((answer (qvm-app::parallel-measure q '(0 1))))
      (is (and (= 2 (length answer))
               (= 1 (first answer))
               (= 0 (second answer)))))))

(deftest test-multishot-measure-disjoint-measurement-qubits ()
  (dolist (simulation-method '(qvm-app::pure-state  qvm-app::full-density-matrix))
    (let ((quil (qvm-app::safely-parse-quil-string "X 0")))
      (let ((answer (mute
                      ;; measure qubits 1 and 2 which have not been
                      ;; affected by program
                      (qvm-app::perform-multishot-measure simulation-method quil 3 '(1 2) 10 nil))))
        (print answer)
        (is (every (lambda (result) (and (= 2 (length result))
                                         (= 0 (first result))
                                         (= 0 (second result))))
                   answer))))))


(deftest test-multishot-simulation-methods ()
  (dolist (simulation-method '(qvm-app::pure-state qvm-app::full-density-matrix))
    (let ((quil (qvm-app::safely-parse-quil-string
                 "DECLARE ro BIT[1]
X 0
MEASURE 0 ro[0]"))
          (addresses
            (alexandria:plist-hash-table
             (list "ro" (list 0))
             :test 'equal)))
      (let ((answer (mute
                      ;; run 5 times, with no noise
                      (qvm-app::perform-multishot simulation-method quil 2 addresses 5)))
            (expected `((1) (1) (1) (1) (1))))
        (is (equalp (gethash "ro" answer) expected))))))


(deftest test-expectation-hadamard-plus ()
  (dolist (simulation-method '(qvm-app::pure-state qvm-app::full-density-matrix))
    (let ((state-prep (qvm-app::safely-parse-quil-string "H 0")) ; we are in the |+> state
          (ops (mapcar #'qvm-app::safely-parse-quil-string
                       (list "X 0"
                             "Y 0"
                             "Z 0"))))
      (let ((answer (mute
                      (qvm-app::perform-expectation simulation-method state-prep ops 1))))
        (is (every #'quil::double=
                   answer
                   '(1.0 0.0 0.0)))))))

(deftest test-expectation-hadamard-minus ()
  (dolist (simulation-method '(qvm-app::pure-state qvm-app::full-density-matrix))
    (let ((state-prep (qvm-app::safely-parse-quil-string "X 0
H 0")) ; we are in the |-> state
          (ops (mapcar #'qvm-app::safely-parse-quil-string
                       (list "X 0"
                             "Y 0"
                             "Z 0"))))
      (let ((answer (mute
                      (qvm-app::perform-expectation simulation-method state-prep ops 1))))
        (is (every #'quil::double=
                   answer
                   '(-1.0 0.0 0.0)))))))


(deftest test-expectation-mixed-op ()
  (dolist (simulation-method '(qvm-app::pure-state qvm-app::full-density-matrix))
    (let ((state-prep (qvm-app::safely-parse-quil-string "H 0")) 
          (ops (list (qvm-app::safely-parse-quil-string "X 0
Y 1"))))
      (let ((answer (mute
                      ;; we run on 5 qubits, just to make sure matrices are forced
                      ;; to a compatible size
                      (qvm-app::perform-expectation simulation-method state-prep ops 5))))
        (is (quil::double= 0.0 (first answer)))))))
