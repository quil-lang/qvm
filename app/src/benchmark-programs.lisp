;;;; benchmark-programs.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun bell-program (n)
  (quil:safely-parse-quil
   (with-output-to-string (*standard-output*)
     (format t "DECLARE ro BIT[~D]~%" n)
     (format t "H 0~%")
     (loop :for i :below (1- n)
           :do (format t "CNOT ~D ~D~%" i (1+ i)))
     (loop :for i :below n
           :do (format t "MEASURE ~D ro[~D]~%" i i)))))

;;; TODO: remove this when QVM-EXAMPLES won't cause a circular dependency
(defun qft-program (n)
  (labels ((bit-reversal-circuit (qubits)
             "Create a circuit which does a bit reversal on the amplitude indexes."
             (let ((n (length qubits)))
               (if (< n 2)
                   nil
                   (loop :for i :below (floor n 2)
                         :for qs :in qubits
                         :for qe :in (reverse qubits)
                         :collect (make-instance 'quil:gate-application
                                                 :operator #.(quil:named-operator "SWAP")
                                                 :name-resolution (quil:lookup-standard-gate "SWAP")
                                                 :arguments (list (quil:qubit qs)
                                                                  (quil:qubit qe)))))))

           (qft-circuit (qubits)
             "Generate the QFT circuit on the given qubits."
             (labels ((qft (qubits)
                        (destructuring-bind (q . qs) qubits
                          (if (null qs)
                              (list (make-instance 'quil:gate-application
                                                   :operator #. (quil:named-operator "H")
                                                   :name-resolution (quil:lookup-standard-gate "H")
                                                   :arguments (list (quil:qubit q))))
                              (let ((cR nil))
                                (loop :with n := (1+ (length qs))
                                      :for i :from (1- n) :downto 1
                                      :for qi :in qs
                                      :for angle := (qvm:flonum (/ pi (expt 2 (- n i))))
                                      :do (push (make-instance
                                                 'quil:gate-application
                                                 :operator #.(quil:named-operator "CPHASE")
                                                 :name-resolution (quil:lookup-standard-gate "CPHASE")
                                                 :parameters (list (quil:constant angle))
                                                 :arguments (list (quil:qubit q)
                                                                  (quil:qubit qi)))
                                                cR))
                                (append
                                 (qft qs)
                                 cR
                                 (list (make-instance 'quil:gate-application
                                                      :operator #. (quil:named-operator "H")
                                                      :name-resolution (quil:lookup-standard-gate "H")
                                                      :arguments (list (quil:qubit q))))))))))
               (make-instance 'quil:parsed-program
                              :gate-definitions nil
                              :circuit-definitions nil
                              :executable-code
                              (concatenate
                               'vector
                               ;; Core QFT with normalization.
                               (qft qubits)

                               ;; Re-ordering the output.
                               (bit-reversal-circuit qubits))))))
    (qft-circuit (loop :for i :below n :collect i))))

(defun hadamard-program (n)
  (quil:safely-parse-quil
   (with-output-to-string (*standard-output*)
     (dotimes (i n)
       (format t "H ~D~%" i)))))

(defun nop-loop (num-loops num-nops)
  "Create a loop that has NUM-LOOPS iterations and NUM-NOPS NOP instructions per iteration."
  (check-type num-loops unsigned-byte)
  (check-type num-nops unsigned-byte)
  (quil:safely-parse-quil
   (with-output-to-string (*standard-output*)
     (write-line "DECLARE counter INTEGER")
     (write-line "DECLARE done BIT")
     (format t   "MOVE counter ~D~%" num-loops)
     (write-line "LABEL @start")
     (write-line "EQ done counter 0")
     (write-line "JUMP-WHEN @end done")
     (loop :repeat num-nops
           :do (write-line "NOP"))
     (write-line "SUB counter 1")
     (write-line "JUMP @start")
     (write-line "LABEL @end")
     (write-line "HALT"))))

;;; Benchmark from https://github.com/qulacs/qulacs
;;;
;;; Note that the original benchmark as stated requires 100 shots. It
;;; is not clear if the wavefunction can be sampled to do this, or
;;; whether the program must be run 100 times.
(defun qulacs-program (n &key (rx-layers 10))
  "The qulacs benchmark, specified to be 10 layers of random RX rotations interleaved with 9 layers of neighboring CNOTs, followed by measurement of all qubits.

We are assuming the CNOTs are dense on an even number of qubits."
  (assert (plusp rx-layers))
  (quil:safely-parse-quil
   (with-output-to-string (*standard-output*)
     ;; Initial RX layer.
     (loop :for q :below n :do
       (format t "RX(~F) ~D~%" (random (* 2 pi)) q))
     ;; CNOT-RX layers.
     (loop :repeat (1- rx-layers) :do
       ;; CNOT's
       (loop :with qubits := (alexandria:shuffle (alexandria:iota n))
             :repeat (floor n 2)
             :do (format t "CNOT ~D ~D~%" (pop qubits) (pop qubits)))
       ;; RX's
       (loop :for q :below n :do
         (format t "RX(~F) ~D~%" (random (* 2 pi)) q)))
     ;; Measurements
     (loop :for q :below n :do
       (format t "MEASURE ~D~%" q)))))

(defun interleaved-measurements-program (n)
  (assert (>= n 2))
  (quil:safely-parse-quil
   (with-output-to-string (*standard-output*)
     (dotimes (q n)
       (format t "RX(~F) ~D~%" (random (* 2 pi)) q))
     (dotimes (q n)
       (let ((qubits (subseq (alexandria:shuffle (alexandria:iota n)) 0 2))
             (mqubit (random n)))
         (format t "PSWAP(~F) ~D ~D~%" (random (* 2 pi)) (pop qubits) (pop qubits))
         (format t "MEASURE ~D~%" mqubit))))))

(defun norm-baseline-timing (wf)
  ;; touch all entries
  (qvm::bring-to-zero-state wf)
  (let (timing)
    (with-timing (timing)
      (qvm::%serial-norm wf))
    timing))

(defun perform-benchmark (type num-qubits)
  (check-type num-qubits (integer 1))
  (cond
    ((string-equal type "suite")
     (qvm-benchmarks:run-benchmarks :verbose t))
    ((string-equal type "baseline")
     (let ((q (qvm:make-qvm num-qubits :allocation (funcall **default-allocation** (expt 2 num-qubits)))))
       (format-log "Computing baseline serial norm timing...")
       (finish-output)
       (tg:gc :full t)
       (format-log "Baseline serial norm timing: ~D ms" (norm-baseline-timing (qvm::amplitudes q)))))
    (t
     (let ((p (alexandria:eswitch (type :test #'string-equal)
                ("bell" (bell-program num-qubits))
                ("qft"  (qft-program num-qubits))
                ("hadamard" (hadamard-program num-qubits))
                ("qulacs" (qulacs-program num-qubits))
                ("interleaved-measurements" (interleaved-measurements-program num-qubits))
                ("nop-loop" (nop-loop 100000 250))))
           (q (qvm:make-qvm num-qubits
                            :allocation (funcall **default-allocation** (expt 2 num-qubits))))
           timing)
       (qvm:load-program q p :supersede-memory-subsystem t)

       (format-log "Starting ~S benchmark with ~D qubits...~%" type num-qubits)

       (tg:gc :full t)

       (with-timing (timing)
         (time (qvm:run q)))

       (room)
       (terpri)
       (format-log "Total time for program run: ~D ms" timing)))))
