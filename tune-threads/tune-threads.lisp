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
  (qvm:run q)
  (reset-qvm q))

(defun prepare-quil-prog-for-timing (num-threads)
  "Time the quil program in qvm Q and return the number of seconds."
  (setf qvm:*transition-verbose* nil)
  (setf cl-quil::*allow-unresolved-applications* t)
  (setf qvm::*qubits-required-for-parallelization* 2) ; for testing, always use parallelization
  (qvm::prepare-for-parallelization num-threads)
  nil)

(defun time-quil-prog-raw (q &key (num-trials 1) (num-threads 4))
  "Time the quil program in qvm Q and return the number of seconds."
  (prepare-quil-prog-for-timing num-threads)
  (simple-time num-trials (run-and-reset-qvm q)))

(defun time-quil-prog (q &key (num-trials 1) (num-threads 4))
  "Time the quil program in qvm Q and return the number of seconds per thread per trial."
  (/ (time-quil-prog-raw q :num-trials num-trials :num-threads num-threads) (* num-threads num-trials)))

(defun find-num-trials (q num-threads time-limit &optional (hint 0))
  "Choose the number of trials for the qvm/program Q that take around TIME-LIMIT seconds to complete."
  (if (= hint 1)
      1
      (loop :with one-time := 0.0
            :with num-trials := 1
            :do
               (setf one-time (time-quil-prog-raw q :num-threads num-threads :num-trials num-trials))
               (setf num-trials (* num-trials 2))
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
  (quil:parse-quil (hadamard-program-source n)))

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

(defun scan-num-threads (&key (time-limit 5.0) (num-qubits 12)
                              (max-num-threads (qvm:count-logical-cores))
                              (min-num-threads 1)
                              (q (prepare-hadamard-test num-qubits)))
  "Return a list of execution time per sample per thread of the qvm/program Q for different numbers of threads.
The number of threads varies from MIN-NUM-THREADS to MAX-NUM-THREADS."
  (norm-min
   (let ((num-trials 0))
     (loop :for num-threads :from min-num-threads :to max-num-threads
           :collect
           (progn (setf num-trials (find-num-trials q num-threads time-limit num-trials))
                  (time-quil-prog q :num-threads num-threads :num-trials num-trials))))))

(defun scan-num-threads-qubits (&key (time-limit 5.0)
                                     (max-num-threads (qvm:count-logical-cores))
                                     (min-num-threads 1)
                                     (min-num-qubits 2)
                                     (max-num-qubits 23)
                                     (prepare-qvm-program #'prepare-hadamard-test))
  "Scan both the number of qubits and the number of threads to find the optimal number
 of threads for each number of qubits."
  (loop :for num-qubits :from min-num-qubits :to max-num-qubits
        :do (let* ((q (funcall prepare-qvm-program num-qubits))
                   (scan-times
                     (scan-num-threads :time-limit time-limit :max-num-threads max-num-threads :min-num-threads min-num-threads :q q))
                   (optimal-num-qubits (1+ (cadr (findmin scan-times)))))
              (format t "nq: ~2d: nthreads: ~2d  ~a~%" num-qubits optimal-num-qubits scan-times))))
