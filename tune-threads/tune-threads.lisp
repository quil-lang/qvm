;;;; tune-threads/tune-threads.lisp
;;;;
;;;; Author: John Lapeyre

(in-package #:qvm-tune-threads)

(defstruct pre-timing
  "Data characterizing coarse timing of quil program."
  (init-num-trials 1 :read-only t)
  (num-trials 1 :read-only t)
  (trial-time 1.0 :read-only t))

(defparameter *minimum-target-time* 0.1
  "The minimum required execution time for a block of repeated trials of a quil program.
The number of trials will be increased until this target time is exceeded.")

(defun time-quil-prog-raw (q &key (num-trials 1) (num-threads 4))
  "Return the execution time in seconds of the quil program in qvm Q."
  (qvm::prepare-for-parallelization num-threads)
  (simple-time num-trials (run-and-reset-qvm q)))

(defun run-pre-timing (q num-threads init-num-trials)
  "Perform coarse timing of program in qvm Q."
  (loop :for num-pre-trials := init-num-trials :then (* num-pre-trials 2)
        :for trial-time := (time-quil-prog-raw q :num-threads num-threads :num-trials num-pre-trials)
        :while (< trial-time *minimum-target-time*) ; avoid divide by zero and times very close to zero
        :finally (return (make-pre-timing :init-num-trials init-num-trials :num-trials num-pre-trials :trial-time trial-time))))

(defun num-trials-in-target-time (pt target-time)
  "Use coarse timing data in PT to determine the number of trials required to consume TARGET-TIME seconds.
The latter time is used for more precise timing."
  (1+ (floor target-time (/ (pre-timing-trial-time pt) (pre-timing-num-trials pt)))))

(defstruct timing
  "Data characterizing precise (relative to coarse timing) timing of a quil program.
All slots are meant to be read only, and have no default values."
  pre-timing
  num-trials
  num-threads
  target-time
  run-time)

(defun print-timing-info (tm)
  "Print 1) optimal number of threads; 2) rescaled exectution times for each number of threads
using the timing data in TM, an instance of TIMING."
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'float (lambda (s f) (format s "~,2f" f)))
    (format t "~d: ~a~%" (optimal-num-threads-from-scan tm) (scaled-times-per-trial tm))))

(defun time-per-trial (tm)
  "Compute number of seconds per thread, per trial from timing data in instance of TIMING struct TM."
  (/ (timing-run-time tm) (* (timing-num-threads tm) (timing-num-trials tm))))

(defun times-per-trial (tms)
  "Compute timing via TIME-PER-TRIAL for each entry in hash table TMS."
  (map 'vector (lambda (num-threads) (time-per-trial (gethash num-threads tms)))
       (sort-hash-table-keys tms)))

(defun scaled-times-per-trial (tms)
  "Compute and collect the timing from each entry in hash table TMS and rescale by the minimum time.
A vector is returned reprenting the relative efficiecy for a fixed number of qubits and varying number of threads."
  (rescale-by-minimum (times-per-trial tms)))

(defun optimal-num-threads-from-scan (tms)
  "Compute the optimal number of threads from the hash table of TIMING structs TMS
whose keys are different numbers of threads."
  (let* ((run-times (times-per-trial tms))
         (min-index (findmin run-times)))
    (elt (sort-hash-table-keys tms) min-index)))

(defparameter *default-target-time* 1.0
  "The approximate time consumed by a block of repeated trials during precise timing.")

(defun optimal-num-threads (num-qubits
                            &key (target-time *default-target-time*) (num-threads-to-scan nil)
                                 (prepare-qvm-program #'prepare-hadamard-test) (print-info nil))
  "Compute the optimal number of threads for a NUM-QUBITS qubit program. Keys are:
TARGET-TIME -- approximate seconds to be consumed in precise timing. Default 1s.
NUM-THREADS-TO-SCAN -- a list whose elements are different numbers of threads to time.
                       Defaults to nil which means one through the number of cores.
PREPARE-QVM-PROGRAM -- a function that takes one argument, the number of qubits, and returns a compiled quil program.
PRINT-INFO -- print very brief summary of timing if true."
  (optimal-num-threads-from-scan (scan-num-threads num-qubits
                                         :prepare-qvm-program prepare-qvm-program :print-info print-info
                                         :num-threads-to-scan num-threads-to-scan
                                         :target-time target-time)))

(defun time-quil-prog (q num-threads target-time &key (init-num-pre-trials 1))
  "Time the quil program in qvm Q and return a TIMING struct. NUM-THREADS threads are used during timing.
Approximately TARGET-TIME seconds will be consumed during timing.
The keyword parameter :INIT-NUM-PRE-TRIALS determines the intial number of trials to use during pre-timing.
Pre-timing is used to choose the number of trials to consume TARGET-TIME seconds."
  (let* ((pt (run-pre-timing q num-threads init-num-pre-trials))
         (num-trials (num-trials-in-target-time pt target-time))
         (run-time (time-quil-prog-raw q :num-threads num-threads :num-trials num-trials)))
    (make-timing :pre-timing pt :num-trials num-trials
                 :num-threads num-threads :target-time target-time :run-time run-time)))

(defun scan-num-threads (num-qubits
                         &key (target-time *default-target-time*) (num-threads-to-scan nil)
                              (prepare-qvm-program #'prepare-hadamard-test) (print-info nil))
  "Run TIME-QUIL-PROGRAM repeatedly, varying the number of threads, but keeping NUM-QUBITS fixed.
Return a hash table of TIMING structs, one for each number of threads. Keys are:
TARGET-TIME -- approximate seconds to be consumed in precise timing. Default 1s.
NUM-THREADS-TO-SCAN -- a list whose elements are different numbers of threads to time.
                       Defaults to nil which means one through the number of cores.
PREPARE-QVM-PROGRAM -- a function that takes one argument, the number of qubits, and returns a compiled quil program.
PRINT-INFO -- If true, print very brief summary of timing.
"  
  (let ((tms (make-hash-table))
        (q (funcall prepare-qvm-program num-qubits))
        (num-threads-to-scan (if (not (null num-threads-to-scan)) num-threads-to-scan
                                 (alexandria:iota (count-logical-cores) :start 1))))
    (loop :for num-threads :in num-threads-to-scan
          :do (setf (gethash num-threads tms) (time-quil-prog q num-threads target-time))
          :finally
             (when print-info (print-timing-info tms))
             (return tms))))

(defun scan-num-qubits (num-qubits-to-scan
                        &key (target-time *default-target-time*) (num-threads-to-scan nil)
                             (prepare-qvm-program #'prepare-hadamard-test) (print-info nil))
  "Time a quil program, varying both the number of qubits and the number of threads. NUM-QUBITS-TO-SCAN
is a list of the different numbers of qubits to time. Keyword parameters are :TARGET-TIME,
:NUM-THREADS-TO-SCAN, :PREPARE-QVM-PROGRAM, and :PRINT-INFO. The meanings of these parameters
is the same as for SCAN-NUM-THREADS, which is called repeatedly by SCAN-NUM-QUBITS."
  (let ((qtms (make-hash-table)))
    (loop :for num-qubits :in num-qubits-to-scan
          :do (setf (gethash num-qubits qtms)
                    (scan-num-threads num-qubits
                                      :prepare-qvm-program prepare-qvm-program :print-info print-info
                                      :num-threads-to-scan num-threads-to-scan
                                      :target-time target-time))
          :finally (return qtms))))

;; TODO: Make PR to move reset-qvm somewhere under qvm src.
(defun reset-qvm (q)
  "Reset the quantum and classical states and program counter of Q."
  (qvm::reset-quantum-state q)
  (qvm::reset-classical-memory q)
  (setf (qvm::pc q) 0))

(defun run-and-reset-qvm (q)
  "Run the qvm Q, then reset its state."
  (let ((qvm:*transition-verbose* nil)
        (qvm::*qubits-required-for-parallelization* 1)); for testing, always use parallelization
    (qvm:run q)
    (reset-qvm q)))

(defun hadamard-program-source (n)
  "Return a quil source program that performs a Hadamard gate on each of N qubits, then measures each qubit."
  (with-output-to-string (s)
    (format s "DECLARE ro BIT[~d]~%" n)
    (dotimes (q n)
      (format s "H ~d~%" q))
    (dotimes (q n)
      (format s "MEASURE ~d ro[~d]~%" q q))))

(defun hadamard-program (n)
  "Return a parsed quil program that performs a Hadamard gate on each of N qubits and measures each qubit."
  (let ((cl-quil::*allow-unresolved-applications* t))
    (quil:parse-quil (hadamard-program-source n))))

(defun prepare-hadamard-test (num-qubits)
  "Create a qvm and load the program created by HADAMARD-PROGRAM with NUM-QUBITS qubits."
  (let* ((q (qvm:make-qvm num-qubits))
         (p (hadamard-program num-qubits)))
    (qvm:load-program q p :supersede-memory-subsystem t)
    q))
