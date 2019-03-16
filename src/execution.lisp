;;;; execution.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Execution of quantum programs on QVM instances.

(defmethod run ((qvm pure-state-qvm))
  ;; Compile the program before running it.
  (when *compile-before-running*
    (when *transition-verbose*
      (format *trace-output* "~&; Fusing gates...~%"))
    (let ((old-length (loaded-program-length qvm)))
      (setf (program qvm) (quil::fuse-gates-in-executable-code (program qvm)))
      (format *trace-output* "~&; Fused length from ~D to ~D instruction~:P~%"
              old-length
              (loaded-program-length qvm)))
    (when *transition-verbose*
      (format *trace-output* "~&; Compiling program loaded into QVM...~%"))
    (let ((start (get-internal-real-time)))
      (compile-loaded-program qvm)
      (when *transition-verbose*
        (format *trace-output* "~&; Compiled in ~D ms.~%"
                (round (* (/ 1000 internal-time-units-per-second)
                          (- (get-internal-real-time) start))))
        (finish-output *trace-output*))))

  ;; Actually start the execution.
  (loop :with pc := 0
        :until (or (null pc) (>= pc (loaded-program-length qvm))) :do
          (setf (pc qvm) pc)
          (multiple-value-setq (qvm pc)
            (transition qvm (current-instruction qvm)))
        :finally (return qvm)))

(defun run-program (num-qubits program)
  "Run the program PROGRAM on a QVM of NUM-QUBITS qubits."
  (check-type num-qubits unsigned-byte)
  (check-type program quil:parsed-program)
  (assert (>= num-qubits (quil:qubits-needed program))
          (num-qubits)
          "The program being run requires more qubits than the ~D specified."
          num-qubits)
  (let ((qvm (make-qvm num-qubits
                       :classical-memory-model
                       (memory-descriptors-to-qvm-memory-model
                        (quil:parsed-program-memory-definitions program)))))
    (load-program qvm program)
    (run qvm)))
