;;;; execution.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Execution of quantum programs on QVM instances.

(defmethod run ((qvm pure-state-qvm))
  ;; Compile the program before running it.
  (when *compile-before-running*
    (when *transition-verbose*
      (format *trace-output* "~&; Compiling program loaded into QVM...~%"))
    (let ((start (get-internal-real-time)))
      (compile-loaded-program qvm)
      (when *transition-verbose*
        (format *trace-output* "~&; Compiled in ~D ms.~%"
                (* (/ 1000 internal-time-units-per-second)
                   (- (get-internal-real-time) start)))
        (finish-output *trace-output*))))

  ;; Actually start the execution.
  (loop :with pc := 0
        :until (or (null pc) (>= pc (loaded-program-length qvm))) :do
          (setf (pc qvm) pc)
          (multiple-value-setq (qvm pc)
            (transition qvm (current-instruction qvm)))
        :finally (return qvm)))

(defun run-program (num-qubits program &key (classical-memory-size 8))
  "Run the program PROGRAM on a QVM of NUM-QUBITS qubits, with a classical memory size of CLASSICAL-MEMORY-SIZE."
  (let ((qvm (make-qvm num-qubits :classical-memory-size classical-memory-size)))
    (load-program qvm program)
    (run qvm)))
