;;;; tune-threads/tune-threads.lisp
;;;;
;;;; Author: John Lapeyre

(in-package #:qvm-tune-threads)

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

(defun time-quil-prog-raw (q &key (num-trials 1) (num-threads 4))
  "Time the quil program in qvm Q and return the number of seconds."
  (qvm::prepare-for-parallelization num-threads)
  (simple-time num-trials (run-and-reset-qvm q)))

(defun time-quil-prog (q &key (num-trials 1) (num-threads 4))
  "Time the quil program in qvm Q and return the number of seconds per thread per trial."
  (/ (time-quil-prog-raw q :num-trials num-trials :num-threads num-threads) (* num-threads num-trials)))

(defun find-num-trials (q num-threads time-limit &optional (hint 0))
  "Choose the number of trials for the qvm/program Q that take around TIME-LIMIT seconds to complete."
  (if (= hint 1)
      1
      (loop :for num-trials := 1 :then (* num-trials 2)
            :for one-time := 0.0 :then (time-quil-prog-raw q :num-threads num-threads :num-trials num-trials)
            :while (< one-time 0.1) ; avoid divide by zero and times very close to zero
            :finally (return (1+ (floor (/ time-limit (/ one-time num-trials))))))))

(defun hadamard-program-source (n)
  "Return a quil source program that performs a Hadamard gate on each of N qubits and measures each qubit."
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
  "Return a qvm loaded with a program created by HADAMARD-PROGRAM with NUM-QUBITS qubits."
  (let* ((q (qvm:make-qvm num-qubits))
         (p (hadamard-program num-qubits)))
    (qvm:load-program q p :supersede-memory-subsystem t)
    q))

(defun time-hadamard-raw (&key (num-trials 1) (num-threads 4) (num-qubits 10))
  (time-quil-prog-raw (prepare-hadamard-test num-qubits) :num-trials num-trials :num-threads num-threads))

(defun time-hadamard (&key (num-trials 1) (num-threads 4) (num-qubits 10))
  "Time the Hadamard program and return the number of seconds per thread per trial."
  (time-quil-prog (prepare-hadamard-test num-qubits) :num-trials num-trials :num-threads num-threads))

(defvar *default-testing-time-limit* 2.0)

(defun scan-num-threads (&key (time-limit *default-testing-time-limit*) (num-qubits 12)
                              (max-num-threads (qvm:count-logical-cores))
                              (min-num-threads 1)
                              (q (prepare-hadamard-test num-qubits)))
  "Return a list of execution time per sample per thread of the qvm/program Q for different numbers of threads.
The number of threads varies from MIN-NUM-THREADS to MAX-NUM-THREADS."
  (norm-min
   (loop :for num-trials := (find-num-trials q num-threads time-limit 0)
           :then (find-num-trials q num-threads time-limit num-trials)
         :for num-threads :from min-num-threads :to max-num-threads
         :collect (time-quil-prog q :num-threads num-threads :num-trials num-trials))))

(defun scan-num-threads-qubits (&key (time-limit *default-testing-time-limit*)
                                     (min-num-threads 1) (max-num-threads (qvm:count-logical-cores))
                                     (min-num-qubits 2) (max-num-qubits 23)
                                     (prepare-qvm-program #'prepare-hadamard-test))
  "Scan both the number of qubits and the number of threads to find the optimal number
 of threads for each number of qubits."
  (format t "qubits threads rel-times~%" )
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'float (lambda (s f) (format s "~,2f" f)))
    (loop :for num-qubits :from min-num-qubits :to max-num-qubits
          :for q := (funcall prepare-qvm-program num-qubits)
          :for scan-times := (scan-num-threads :time-limit time-limit :num-qubits num-qubits
                                               :max-num-threads max-num-threads :min-num-threads min-num-threads :q q)
          :for optimal-num-qubits = (1+ (cadr (findmin scan-times)))
          :do (format t "~3d    ~3d     ~a~%" num-qubits optimal-num-qubits scan-times))))
