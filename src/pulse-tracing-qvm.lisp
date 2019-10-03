(in-package :qvm)

(defstruct pulse-event
  start-time
  end-time
  frame
  frame-state
  instruction)

(defun pulse-event-duration (pulse-event)
  "The duration of a pulse event."
  (- (pulse-event-end-time pulse-event)
     (pulse-event-start-time pulse-event)))

;;; A structure tracking the state of a specific frame.
(defstruct frame-state
  (phase 0.0d0
   :type real)
  (scale 1.0d0
   :type real)
  (frequency nil
   :type (or real null))
  ;; The sample rate in Hz for waveform generation.
  (sample-rate (error "Sample rate must be defined.")
   :type real))

(defparameter *initial-pulse-event-log-length* 100
  "The initial length of the pulse tracing QVM's event log.")

(defclass pulse-tracing-qvm (classical-memory-mixin)
  ((local-clocks :initarg :local-clocks
                 :accessor local-clocks
                 :initform (make-hash-table :test #'quil::frame= :hash-function #'quil::frame-hash)
                 :documentation "A table mapping qubit indices to the time of their last activity.")
   (frame-states :initarg :frame-states
                 :accessor frame-states
                 :initform (make-hash-table :test #'quil::frame= :hash-function #'quil::frame-hash)
                 :documentation "A table mapping frames to their active states.")
   (pulse-event-handlers :initarg :event-handler
                   :accessor pulse-event-handlers
                   :initform nil
                   :documentation "A list of event handlers, which are called with active PULSE events."))
  (:documentation "A quantum virtual machine capable of tracing pulse sequences over time."))

;;; TODO this fakeness is mainly to make LOAD-PROGRAM happy
(defmethod number-of-qubits ((qvm pulse-tracing-qvm))
  most-positive-fixnum)

(defun make-pulse-tracing-qvm ()
  "Create a new pulse tracing QVM."
  (make-instance 'pulse-tracing-qvm
                 :classical-memory-subsystem
                 (make-instance 'classical-memory-subsystem
                                :classical-memory-model
                                quil:**empty-memory-model**)))

(defun initialize-frame-states (qvm frame-definitions)
  "Set up initial frame states on the pulse tracing QVM."
  (check-type qvm pulse-tracing-qvm)
  (dolist (defn frame-definitions)
    (let ((frame (quil:frame-definition-frame defn))
          (sample-rate (quil:frame-definition-sample-rate defn))
          (initial-frequency (quil:frame-definition-initial-frequency defn)))
      (unless sample-rate
        (error "Frame ~A has unspecified sample-rate" frame))
      (setf (gethash frame (frame-states qvm))
            (make-frame-state :frequency (and initial-frequency
                                              (quil:constant-value initial-frequency))
                              :sample-rate (quil:constant-value sample-rate))))))

(defun install-event-handler (qvm handler)
  (check-type handler function)
  (push handler (pulse-event-handlers qvm)))

(defun trace-quilt-program (program)
  "Trace a quilt PROGRAM, returning a list of pulse events."
  (check-type program quil:parsed-program)
  (let ((qvm (make-pulse-tracing-qvm))
        (log nil))
    (initialize-frame-states qvm (quil:parsed-program-frame-definitions program))
    (install-event-handler qvm
                           (lambda (event)
                             (push event log)))
    (load-program qvm program)
    (run qvm)
    (nreverse log)))

(defun local-time (qvm frame &optional (default 0.0d0))
  "Get the local time of FRAME on the pulse tracing QVM."
  (check-type qvm pulse-tracing-qvm)
  (gethash frame (local-clocks qvm) default))

(defun (setf local-time) (new-value qvm frame)
  "Set the local time of FRAME on the pulse tracing QVM."
  (setf (gethash frame (local-clocks qvm))
        new-value))

(defun frame-state (qvm frame)
  "Returns a copy of the state associated with the given frame."
  (check-type qvm pulse-tracing-qvm)
  (alexandria:if-let ((state (gethash frame (frame-states qvm))))
    (copy-structure state)
    (error "Attempted to reference non-existent frame ~A" frame)))

(defun (setf frame-state) (new-value qvm frame)
  "Set the state associated with the given frame."
  (check-type qvm pulse-tracing-qvm)
  (if (gethash frame (frame-states qvm))
      (setf (gethash frame (frame-states qvm))
            new-value)
      (error "Attempted to modify non-existent frame ~A" frame)))

(defun latest-time (qvm &rest frames)
  "Get the latest time of the specified FRAMES on the pulse tracing QVM."
  (loop :for f :in frames :maximize (local-time qvm f)))

;;; TRANSITIONs

(defun intersecting-frames (qvm qubits)
  "Return all frames tracked by the pulse tracing QVM which involve any of the specified QUBITS."
  (check-type qvm pulse-tracing-qvm)
  (remove-if-not (lambda (f)
                   (quil::frame-intersects-p f qubits))
                 (alexandria:hash-table-keys (frame-states qvm))))

(defun frames-on-qubits (qvm qubits)
  "Return all frames tracked by the pulse tracing QVM which involve exactly the specified QUBITS in the specified order."
  (check-type qvm pulse-tracing-qvm)
  (remove-if-not (lambda (f)
                   (quil::frame-on-p f qubits))
                 (alexandria:hash-table-keys (frame-states qvm))))

(defmethod transition ((qvm pulse-tracing-qvm) (instr quil:delay-on-frames))
  (dolist (frame (quil:delay-frames instr))
    (incf (local-time qvm frame) (quil:delay-duration instr)))

  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pulse-tracing-qvm) (instr quil:delay-on-qubits))
  (let ((frames (frames-on-qubits qvm (quil:delay-qubits instr))))
    (transition qvm (make-instance 'quil:delay-on-frames
                                   :frames frames
                                   :duration (quil:delay-duration instr)))))

(defun synchronize-frame-clocks (qvm frames)
  (let ((latest (apply #'latest-time qvm frames)))
    (dolist (frame frames)
      (setf (local-time qvm frame) latest))))

(defmethod transition ((qvm pulse-tracing-qvm) (instr quil:fence))
  (let ((frames (intersecting-frames qvm (quil:fence-qubits instr))))
    (synchronize-frame-clocks qvm frames))

  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pulse-tracing-qvm) (instr quil:simple-frame-mutation))
  (let* ((frame (quil:frame-mutation-target-frame instr))
         (val (quil:constant-value
               (quil:frame-mutation-value instr)))
         (fs (frame-state qvm frame)))

    ;; update state
    (etypecase instr
      (quil:set-frequency
       (setf (frame-state-frequency fs) val))
      (quil:set-phase
       (setf (frame-state-phase fs) val))
      (quil:shift-phase
       (incf (frame-state-phase fs) val))
      (quil:set-scale
       (setf (frame-state-scale fs) val)))

    ;; update entry
    (setf (frame-state qvm frame) fs))

  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pulse-tracing-qvm) (instr quil:swap-phase))
  (with-slots (left-frame right-frame) instr
    (when (equalp left-frame right-frame)
      (error "SWAP-PHASE requires distinct frames."))
    ;; there is an implicit synchronization in SWAP-PHASE
    (apply #'synchronize-frame-clocks left-frame right-frame)
    (let ((left-state (frame-state qvm left-frame))
          (right-state (frame-state qvm right-frame)))
      (rotatef (frame-state-phase left-state) (frame-state-phase right-state))
      (setf (frame-state qvm left-frame) left-state
            (frame-state qvm right-frame) right-state)))

  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pulse-tracing-qvm) instr)
  (if (not (typep instr '(or quil:pulse quil:capture quil:raw-capture)))
      (warn "Cannot resolve timing information for instruction ~A" instr)
      (let* ((frame (quil::pulse-operation-frame instr))
             (frame-state (frame-state qvm frame))
             (start-time (latest-time qvm frame))
             (end-time (+ start-time
                          (quil::quilt-instruction-duration instr)))
             (event (make-pulse-event :instruction instr
                                      :start-time start-time
                                      :end-time end-time
                                      :frame frame
                                      :frame-state frame-state)))
        (dolist (handler (pulse-event-handlers qvm))
          (funcall handler event))
      (setf (local-time qvm frame) end-time)

      (unless (quil:nonblocking-p instr)
        ;; this pulse/capture/raw-capture excludes other frames until END-TIME
        (dolist (other (intersecting-frames qvm (quil:frame-qubits frame)))
          (quil:print-instruction other)
          (setf (local-time qvm other)
                (max end-time (local-time qvm other)))))))

  (incf (pc qvm))
  qvm)
