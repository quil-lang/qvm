(in-package #:qvm-tests)

;;; TODO: export symbols from QVM
(deftest test-tracing-simple-quilt-program ()
  (let ((pp (quil:parse-quil "
DEFFRAME 0 \"xy\":
    SAMPLE-RATE: 1.0

DEFFRAME 1 \"xy\":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e9

DEFFRAME 0 1 \"foo\":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e9

DEFFRAME 1 \"rx\":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e9

SET-FREQUENCY 0 \"xy\" 1e9
SET-PHASE 0 \"xy\" 0.5
PULSE 0 \"xy\" gaussian(duration: 1.0, fwhm: 2, t0: 3)    # 0s-1s
FENCE 0 1
PULSE 1 \"xy\" gaussian(duration: 1.0, fwhm: 2, t0: 3)    # 1s-2s
DECLARE ro REAL[100]
RAW-CAPTURE 1 \"rx\" 2.0 ro                               # 2s-4s
PULSE 0 1 \"foo\" flat(duration: 1.5, iq: 1.0)            # 4s-5.5s
"))
        ;; The frames here are pure nonsense.
        (qvm (qvm::make-pulse-tracing-qvm))
        (log nil))
    (qvm::initialize-frame-states qvm (quil:parsed-program-frame-definitions pp))
    (qvm::install-event-handler qvm
                                (lambda (event)
                                  (alexandria:appendf log (list event))))
    (load-program qvm pp)
    (run qvm)
    ;; check frame state
    (let* ((frame (quil:frame (list (quil:qubit 0)) "xy"))
           (state (qvm::frame-state qvm frame)))
      (is (= 1e9 (qvm::frame-state-frequency state)))
      (is (- 0.5 (qvm::frame-state-phase state))))

    ;; check # of events
    (is (= 4 (length log)))
    ;; check that the last event has the right timing
    (let ((event (elt log (1- (length log)))))
      (is (= 4.0 (qvm::pulse-event-start-time event)))
      (is (= 5.5 (qvm::pulse-event-end-time event)))
      (is (= 1.5 (qvm::pulse-event-duration event))))))

(deftest test-tracing-quilt-nonblocking ()
  (let* ((pp (quil:parse-quil "
DEFFRAME 0 \"xy\":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e9

DEFFRAME 0 \"foo\":
    SAMPLE-RATE: 1.0
    INITIAL-FREQUENCY: 1e9

NONBLOCKING PULSE 0 \"xy\" gaussian(duration: 1.0, fwhm: 2, t0: 3)
PULSE 0 \"foo\" gaussian(duration: 1.5, fwhm: 2, t0: 3)
PULSE 0 \"xy\" gaussian(duration: 1.0, fwhm: 2, t0: 3)"))
         (log (qvm::trace-quilt-program pp)))
    (is (equalp '(0.0 0.0 1.5)
                (map 'list #'qvm::pulse-event-start-time log)))))
