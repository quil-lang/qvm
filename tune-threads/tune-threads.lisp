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

;; (defun prepare-quil-prog-for-timing (q &key (num-trials 1) (num-threads 4))
;;   )

(defun time-quil-prog-raw (q &key (num-trials 1) (num-threads 4))
  "Time the quil program in qvm Q and return the number of seconds."
  (setf qvm:*transition-verbose* nil)
  (setf cl-quil::*allow-unresolved-applications* t)
  (setf qvm::*qubits-required-for-parallelization* 2) ; for testing, always use parallelization
  (qvm::prepare-for-parallelization num-threads)
  (simple-time num-trials (run-and-reset-qvm q)))

(defun time-quil-prog (&key (num-trials 1) (num-threads 4) (q nil))
  "Time the quil program in qvm Q and return the number of seconds per thread per trial."
  (/ (time-quil-prog-raw q :num-trials num-trials :num-threads num-threads))
  (* num-threads num-trials))

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
  (time-quil-prog :num-trials num-trials :num-threads num-threads :q (prepare-hadamard-test num-qubits)))

(defun scan-num-threads (&key (num-trials 1) (num-qubits 12)
                              (max-num-threads (qvm:count-logical-cores))
                              (min-num-threads 1)
                              (q (prepare-hadamard-test num-qubits)))
  "Return a list of execution time per sample per thread of the qvm/program Q for different numbers of threads.
The number of threads varies from MIN-NUM-THREADS to MAX-NUM-THREADS."
  (norm-min (loop :for i :from min-num-threads :to max-num-threads
                  :collect (time-quil-prog :num-threads i :num-trials num-trials :q q))))
