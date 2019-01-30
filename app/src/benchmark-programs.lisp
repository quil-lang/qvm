;;;; benchmark-programs.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun bell-program (n)
  (safely-parse-quil-string
   (with-output-to-string (*standard-output*)
     (format t "DECLARE ro BIT[~D]~%" n)
     (format t "H 0~%")
     (loop :for i :below (1- n)
           :do (format t "CNOT ~D ~D~%" i (1+ i)))
     (loop :for i :below n
           :do (format t "MEASURE ~D ro[~D]~%" i i)))))

(defun qft-program (n)
  (qvm-examples:qft-circuit (loop :for i :below n :collect i)))

(defun hadamard-program (n)
  (safely-parse-quil-string
   (with-output-to-string (*standard-output*)
     (format t "DECLARE ro BIT[~D]~%" n)
     (dotimes (i n)
       (format t "H ~D~%" i))
     (dotimes (i n)
       (format t "MEASURE ~D ro[~D]~%" i i)))))

(defun norm-baseline-timing (wf)
  ;; touch all entries
  (qvm::bring-to-zero-state wf)
  (let (timing)
    (with-timing (timing)
      (qvm::%serial-norm wf))
    timing))

(defun perform-benchmark (type num-qubits)
  (check-type num-qubits (integer 1))
  (if (string-equal type "suite")
      (qvm-benchmarks:run-benchmarks :verbose t)
      (let ((p (alexandria:eswitch (type :test #'string-equal)
                 ("bell" (bell-program num-qubits))
                 ("qft"  (qft-program num-qubits))
                 ("hadamard" (hadamard-program num-qubits))))
            (q (qvm:make-qvm num-qubits))
            timing)
        (qvm:load-program q p :supersede-memory-subsystem t)

        (format-log "Computing baseline serial norm timing...")
        (finish-output)

        (tg:gc :full t)
        (format-log "Baseline serial norm timing: ~D ms" (norm-baseline-timing (qvm::amplitudes q)))

        (tg:gc :full t)

        (format-log "Starting ~S benchmark with ~D qubits...~%" type num-qubits)

        (with-timing (timing)
          (time (qvm:run q)))

        (room)
        (terpri)
        (format-log "Total time for program run: ~D ms" timing))))
