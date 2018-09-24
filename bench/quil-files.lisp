;;;; bench/quil-files.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-benchmarks)

(defparameter *bench-files-directory*
  (asdf:system-relative-pathname ':qvm-benchmarks "bench/")
  "Directory containing Quil benchmark files.")

(defun timed-run (file)
  "Load the Quil file designated by FILE and time its execution."
  (let ((quil::*allow-unresolved-applications* t))
    (let* ((program (cl-quil:read-quil-file
                     (merge-pathnames file *bench-files-directory*)))
           (qvm (qvm:make-qvm (cl-quil:qubits-needed program))))
      (benchmark:with-benchmark-sampling
        (qvm:load-program qvm program :supersede-memory-subsystem t)
        (qvm:run qvm)))))

(benchmark:define-benchmark bench-25-hadamards ()
  "Benchmark a sequence of 25 Hadamard gates."
  (timed-run "25H.quil"))

(benchmark:define-benchmark bench-H4 ()
  "Benchmark for sample VQE run from H4 computation."
  (timed-run "H4.quil"))

(benchmark:define-benchmark bench-H6 ()
  "Benchmark for sample VQE run from H6 computation."
  (timed-run "H6.quil"))

(benchmark:define-benchmark bench-big-defgate ()
  "Benchmark a very large DEFGATE and associated invocations."
  (timed-run "qaoa_8q.quil"))

(benchmark:define-benchmark bench-supremacy ()
  "Benchmark sample code from supremacy studies."
  (timed-run "5x4x25.quil"))
