(in-package :qvm-app-ng-tests)

(defun run-qvm-app-ng-tests (&key (verbose nil) (headless nil))
  "Run all qvm-app-ng tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  (setf fiasco::*test-run-standard-output* (make-broadcast-stream *standard-output*))
  (let ((successp (run-package-tests :package ':qvm-app-ng-tests
                                     :verbose (or headless verbose)
                                     :describe-failures t
                                     :interactive (not headless))))
    (if headless
        (quit-nicely (qvm::boolean-bit successp))
        successp)))

(deftest test-remapping ()
  (let* ((quil (cl-quil:parse-quil "X 1
PRAGMA ADD-KRAUS X 1 \"(1.0 0.0 0.0 1.0)\"
PRAGMA READOUT-POVM 1 \"(0.9 0.2 0.1 0.8)\"
DECLARE ro BIT[1]
MEASURE 1 ro[1]"))
         (processed-quil (qvm-app-ng::process-quil quil))
         (code (cl-quil:parsed-program-executable-code processed-quil))
         (gate-app (aref code 0))
         (add-kraus (aref code 1))
         (readout-povm (aref code 2))
         (measure (aref code 3)))
    ;; test gate application remapped
    (is (zerop
         (cl-quil:qubit-index
          (first (cl-quil:application-arguments gate-app)))))
    ;; test kraus pragma remapped
    (is (zerop
         (first (cl-quil:pragma-qubit-arguments add-kraus))))
    ;; test readout noise pragma remapped
    (is (zerop
         (cl-quil:pragma-qubit-index readout-povm)))
    ;; test types of povm probabilities
    (is (typep (first (cl-quil:pragma-matrix-entries readout-povm))
               'double-float))
    ;; test measurement qubit remapped
    (is (zerop
         (cl-quil:qubit-index
          (cl-quil:measurement-qubit measure))))))

(deftest test-update-available ()
  (multiple-value-bind (update-available-p update)
      (qvm-app-ng::sdk-update-available-p "0.0.0")
    (if update-available-p
        ;; If the network is down, then update-available-p is NIL, but
        ;; we don't want to error in that case. Skip instead.
        (is update)
        (skip))))
