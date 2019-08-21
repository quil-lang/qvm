;;;; transition.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas
;;;;         Robert Smith

(in-package #:dqvm2)

(defmethod transition :around (qvm instr)
  ;; Ideally, we should make *trace-output* be a gateway to cl-syslog instead of adapting QVM's code here.
  (cond
    ((not qvm:*transition-verbose*) (call-next-method))
    (t
     (let ((start (get-internal-real-time))
           gc-time bytes-alloc)
       (multiple-value-prog1 (qvm::measuring-gc (gc-time bytes-alloc) (call-next-method))
         ;; (format-log :debug "~A" qvm)

         (format-log :debug "Transition ~A took ~D ms (gc: ~D ms; alloc: ~D bytes)"
                     (with-output-to-string (s) (cl-quil:print-instruction instr s))
                     (round (* (/ 1000 internal-time-units-per-second)
                               (- (get-internal-real-time) start)))
                     gc-time
                     bytes-alloc))))))

(defmethod transition ((qvm distributed-qvm) (instr quil:gate-application))
  (format-log :debug "Evaluating instruction ~A" (instruction->string instr))

  (apply-distributed-gate qvm instr)

  (incf (qvm::pc qvm))
  qvm)

(defmethod transition ((qvm distributed-qvm) (instr quil:halt))
  (declare (ignore instr))
  (setf (qvm::pc qvm) nil)
  qvm)

(defmethod transition ((qvm distributed-qvm) (instr quil:measure-discard))
  (incf (qvm::pc qvm))
  (multiple-value-bind (ret-qvm cbit)
      (measure qvm
               (quil:qubit-index (quil:measurement-qubit instr)))
    (when (zerop (mpi-comm-rank))
      (format-log :info "~A -> ~D" instr cbit))
    ret-qvm))

(defmethod transition ((qvm distributed-qvm) (instr quil:reset))
  (declare (ignore instr))
  (fill (amplitudes qvm) (qvm::cflonum 0))
  (alexandria:when-let ((offset (offset (addresses qvm) 0)))
    (setf (aref (amplitudes qvm) offset) (qvm::cflonum 1)))
  (incf (qvm::pc qvm))
  qvm)

(defmethod transition ((qvm distributed-qvm) (instr quil:reset-qubit))
  ;; See also: qvm/src/transition.lisp
  (let ((q (quil:qubit-index (quil:reset-qubit-target instr))))
    (multiple-value-bind (measured-qvm measured-bit)
        (measure qvm q)
      (when (= 1 measured-bit)
        (let ((x-instr (aref (quil:parsed-program-executable-code
                              (quil:parse-quil (format nil "X ~D" q)))
                             0)
                       ;; XXX Do something more elegant here.
                       ))
          (apply-distributed-gate qvm x-instr)))
      (setf (qvm::pc qvm) (1+ (qvm::pc measured-qvm)))
      qvm)))
