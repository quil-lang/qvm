;;;; tests/error-qvm-tests.lisp
;;;;
;;;; Tests the behavior of depolarizing noise in the Error QVM.

(in-package #:qvm-tests)

(defun build-measure (qubit-index memory-offset)
  (make-instance 'cl-quil::measure
                 :qubit (cl-quil::qubit qubit-index)
                 :address (cl-quil::mref "ro" memory-offset)))

(defmacro define-depolarizing-test
    (name (&key num-qubits distribution trials
                (alpha 0.5d0))
     &body clauses)
  "Sets up scaffolding for a test which sanity-checks the depolarizing noise QVM.

NAME is the name of the test.
NUM-QUBITS is the number of qubits (and memory registers) involved in the test.
DISTRIBUTION is a p-list mapping bitstrings to expected probabilities.
TRIALS is the number of trials to simulate.
ALPHA is the width of the confidence window: 1.0 tests nothing, 0.0 requires exact agreement, 0.5 means we require that we're 50% confident that our sampled estimate is within Hoeffding's bound of the true distribution.
CLAUSES is a sequence of clauses, each of the form ( SETF-FORM . INSTRUCTIONS ), where SETF-FORM is a single form to be executed (typically to set the QVM's noise slots) and INSTRUCTIONS is a sequence of CL-QUIL instructions along which the QVM will transition."
  (alexandria:with-gensyms (histogram)
    (multiple-value-bind (clauses decls docstring)
        (alexandria:parse-body clauses :documentation t)
      (assert (null decls))
      (let ((confidence-window (sqrt (/ (log (/ 2 (- 1 alpha))) (* 2 trials)))))
        `(deftest ,name ()
           ,@(when docstring `(,docstring))
           (let ((,histogram (make-hash-table)))
             (dotimes (j ,trials)
               (let* ((quil-string (format nil "DECLARE ro BIT[~A]" ,num-qubits))
                      (memory-model (qvm:memory-descriptors-to-qvm-memory-model
                                     (quil:parsed-program-memory-definitions
                                      (quil::parse-quil quil-string))))
                      (qvm (qvm.error::make-error-qvm ,num-qubits
                                                      :classical-memory-model memory-model)))
                 ;; enact clauses
                 ,@(loop :for (setf-form . transitions) :in clauses
                         :collect `(progn
                                     ,setf-form
                                     ,@(loop :for transition :in transitions
                                             :collect `(qvm::transition qvm ,transition))))
                 ;; record results
                 (qvm::dereference-mref qvm (cl-quil::mref "ro" 0))
                 (loop :with acc := 0
                       :for i :from (1- ,num-qubits) :downto 0
                       :do (setf acc (* acc 2)
                                 acc (+ acc (qvm::dereference-mref qvm (cl-quil::mref "ro" i))))
                       :finally (incf (gethash acc ,histogram 0)))))
             ;; check our estimates
             (maphash (lambda (key true-value)
                        (let* ((recorded-hits (gethash key ,histogram 0d0))
                               (estimated-value (/ recorded-hits ,trials)))
                          (format *error-output* "~&Checking ~8b: ~5f < ~5f < ~5f, estimated mean: ~d / ~d = ~5f~%"
                                  key
                                  (- estimated-value ,confidence-window)
                                  true-value
                                  (+ estimated-value ,confidence-window)
                                  (gethash key ,histogram 0d0)
                                  ,trials
                                  estimated-value)))
                      (alexandria:plist-hash-table ,distribution))
             (maphash (lambda (key true-value)
                        (let* ((recorded-hits (gethash key ,histogram 0d0))
                               (estimated-value (/ recorded-hits ,trials)))
                          (is (< (- estimated-value ,confidence-window)
                                 true-value
                                 (+ estimated-value ,confidence-window)))))
                      (alexandria:plist-hash-table ,distribution))))))))

(define-depolarizing-test test-identity-noise
    (:num-qubits 1
     :distribution '(#b0 #.(- 1 (* 1d-1 2/3)) #b1 #.(* 1d-1 2/3))
     :trials 1000)
  "Checks that depolarizing noise acts as expected on the identity gate."
  ;; run a noisy I gate
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 1d-1
         (qvm.error::fowler-qvm-noise-class qvm) #b11111)
   (cl-quil::build-gate "I" () 0))
  ;; perform a perfect measurement
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 0d0)
   (build-measure 0 0)))

(define-depolarizing-test test-identity-noise-2q
    (:num-qubits 2
     :distribution '(#b00 #.(- 1 (+ (* 1d-1 2/3) (* 1d-1 2/3) (* 1d-1 2/3 1d-1 2/3)))
                     #b01 #.(* 1d-1 2/3)
                     #b10 #.(* 1d-1 2/3)
                     #b11 #.(* 1d-1 1d-1 2/3 2/3))
     :alpha 0.9d0
     :trials 1000)
  "Checks that depolarizing noise acts as expected on the identity gate."
  ;; run two noisy I gates
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 1d-1
         (qvm.error::fowler-qvm-noise-class qvm) #b11111)
   (cl-quil::build-gate "I" () 0)
   (cl-quil::build-gate "I" () 1))
  ;; perform two perfect measurements
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 0d0)
   (build-measure 0 0)
   (build-measure 1 1)))

(define-depolarizing-test test-H-noise
    (:num-qubits 1
     :distribution '(#b0 #.(- 1 (* 1d-1 2/3)) #b1 #.(* 1d-1 2/3))
     :alpha 0.9d0
     :trials 1000)
  "Checks that noise in momentum space matches noise in position space."
  ;; run a noisy H gate
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 1d-1
         (qvm.error::fowler-qvm-noise-class qvm) #b11111)
   (cl-quil::build-gate "H" () 0))
  ;; perform a perfect un-compute and measure
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 0d0)
   (cl-quil::build-gate "H" () 0)
   (build-measure 0 0)))

(define-depolarizing-test test-CNOT-noise
    (:num-qubits 2
     :distribution '(;; 1-p and 3 of p/15 options flip neither bit
                     #b00 #.(+ (- 1 1d-1) (* 1d-1 3/15))
                     ;; 4 of p/15 options flip only the first bit
                     #b01 #.(* 1d-1 4/15)
                     ;; 4 of p/15 options flip only the second bit (and then CNOT flips the other)
                     #b11 #.(* 1d-1 4/15)
                     ;; 4 of p/15 options flip both bits (and then CNOT un-flips)
                     #b10 #.(* 1d-1 4/15))
     :alpha 0.9d0
     :trials 1000)
  "Checks that CNOT propagates noise appropriately."
  ;; run a noisy CNOT gate
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 1d-1
         (qvm.error::fowler-qvm-noise-class qvm) #b11111)
   (cl-quil::build-gate "CNOT" () 0 1))
  ;; perform a perfect un-compute and measure
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 0d0)
   (cl-quil::build-gate "CNOT" () 0 1)
   (build-measure 0 0)
   (build-measure 1 1)))

(define-depolarizing-test test-CNOT-noise-cat-state
    (:num-qubits 2
     :distribution '(;; 1-p and 3 of p/15 options flip neither bit
                     #b00 #.(+ (- 1 1d-1) (* 1d-1 3/15))
                     ;; 4 of p/15 options flip only the first bit
                     #b01 #.(* 1d-1 4/15)
                     ;; 4 of p/15 options flip only the second bit (and then CNOT flips the other)
                     #b11 #.(* 1d-1 4/15)
                     ;; 4 of p/15 options flip both bits (and then CNOT un-flips)
                     #b10 #.(* 1d-1 4/15))
     :alpha 0.9d0
     :trials 1000)
  "Checks that CNOT propagates noise appropriately."
  ;; prepare a perfect |0+> state
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 0d0
         (qvm.error::fowler-qvm-noise-class qvm) #b11111)
   (cl-quil::build-gate "H" () 0))
  ;; run a noisy CNOT gate
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 1d-1)
   (cl-quil::build-gate "CNOT" () 0 1))
  ;; perform a perfect un-compute and measure
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 0d0)
   (cl-quil::build-gate "CNOT" () 0 1)
   (cl-quil::build-gate "H" () 0)
   (build-measure 0 0)
   (build-measure 1 1)))

(define-depolarizing-test test-measurement-noise
    (:num-qubits 2
     :distribution '(;; no flip
                     #b00 #.(* (- 1 1d-1) (- 1 1d-1))
                     ;; flip second, not first
                     #b10 #.(* (- 1 1d-1) 1d-1)
                     ;; flip first, not second
                     #b11 #.(* (- 1 1d-1) 1d-1)
                     ;; flip both times
                     #b01 #.(* 1d-1 1d-1))
     :alpha 0.9d0
     :trials 1000)
  "Checks that MEASURE injects some noise."
  ;; run two noisy measurements in succession
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 1d-1
         (qvm.error::fowler-qvm-noise-class qvm) #b11111)
   (build-measure 0 0)
   (build-measure 0 1)))

(define-depolarizing-test test-reset-noise
    (:num-qubits 1
     :distribution '(#b0 #.(- 1 1d-1)
                     #b1 1d-1)
     :alpha 0.9d0
     :trials 1000)
  "Checks that MEASURE injects some noise."
  ;; reset a noisy qubit
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 1d-1
         (qvm.error::fowler-qvm-noise-class qvm) #b11111)
   (make-instance 'cl-quil::reset-qubit :target (cl-quil::qubit 0)))
  ;; run an error-free measurement
  ((setf (qvm.error::fowler-qvm-noise-probability qvm) 0d0)
   (build-measure 0 0)))
