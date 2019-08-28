(in-package :qvm)

;;; TODO is there a better name?
;;; the event can either be tx or rx
(defstruct pulse-event
  instruction
  start-time
  end-time
  frame-state)

;;; TODO move this to cl-quil ast code?
(defun pulse-op-frame (instr)
  (etypecase instr
    (quil:pulse (quil:pulse-frame instr))
    (quil:capture (quil:capture-frame instr))
    (quil:raw-capture (quil:raw-capture-frame instr))))

(defun pulse-event-frame (event)
  "The frame associated with a pulse event."
  (let ((instr (pulse-event-instruction event)))
    (pulse-op-frame instr)))

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
  (frequency (error "Frequency must be defined.")
   :type real))

(defparameter *default-frame-frequency* 1.0 ; TODO get a better default
  "The default frame frequency.")

(defparameter *initial-pulse-event-log-length* 100
  "The initial length of the pulse tracing QVM's event log.")

;;; This is pretty barebones, but does make some claims about what is important
;;; to track. Namely, qubit local time and frame states. We also update a log,
;;; although TODO in principle this could be more generic (e.g. provide some
;;; sort of "event consumer" callbacks).
(defclass pulse-tracing-qvm (classical-memory-mixin)
  ((sample-rate :initarg :sample-rate
                :accessor sample-rate
                :initform (error "A sample rate must be specified.")
                :documentation "The sample rate in Hz for waveform generation.")
   (local-clocks :initarg :local-clocks
                 :accessor local-clocks
                 :initform (make-hash-table)
                 :documentation "A table mapping qubit indices to the time of their last activity.")
   (frame-states :initarg :frame-states
                 :accessor frame-states
                 :initform (make-hash-table :test 'equalp) ; TODO move to alist so we can use a custom equality check?
                 :documentation "A table mapping frames to their active states.")
   (pulse-event-log :initarg :log
                    :accessor pulse-event-log
                    :initform  (make-array *initial-pulse-event-log-length*
                                           :fill-pointer 0
                                           :adjustable t)
                    :documentation "A log, in reverse chronological order, of observed pulse events."))
  (:documentation "A quantum virtual machine capable of tracing pulse sequences over time."))

;;; TODO this fakeness is mainly to make LOAD-PROGRAM happy
(defmethod number-of-qubits ((qvm pulse-tracing-qvm))
  most-positive-fixnum)

(defun make-pulse-tracing-qvm (sample-rate)
  "Create a new pulse tracing QVM with the specified SAMPLE-RATE."
  (make-instance 'pulse-tracing-qvm
                 :sample-rate sample-rate
                 :classical-memory-subsystem
                 (make-instance 'classical-memory-subsystem
                                :classical-memory-model
                                quil:**empty-memory-model**)))

(defun trace-quilt-program (program &key (sample-rate 100)) ; TODO better magic number
  "Trace a quilt PROGRAM, returning a list of pulse events."
  (check-type program quil:parsed-program)
  (let ((qvm (make-pulse-tracing-qvm sample-rate)))
    (load-program qvm program)
    (run qvm)
    (pulse-event-log qvm)))

(defun local-time (qvm qubit &optional (default 0.0))
  "Get the local time of QUBIT on the pulse tracing qvm QVM."
  (check-type qvm pulse-tracing-qvm)
  (gethash (quil:qubit-index qubit) (local-clocks qvm) default))

(defun (setf local-time) (new-value qvm qubit)
  "Set the local time of qubit Q on the pulse tracing qvm QVM."
  (setf (gethash (quil:qubit-index qubit) (local-clocks qvm))
        new-value))

(defun frame-state (qvm frame)
  (gethash frame (frame-states qvm)))

(defun (setf frame-state) (new-value qvm frame)
  (setf (gethash frame (frame-states qvm))
        new-value))

(defun latest-time (qvm &rest qubits)
  "Get the latest time of the specified QUBITS on the pulse tracing QVM.
If "
  (loop :for q :in qubits :maximize (local-time qvm q)))

(defun new-or-copied-state (qvm frame)
  "Get a copy of the frame state associated with FRAME on the pulse tracing qvm QVM.
If there is no associated state, return a new one."
  (check-type qvm pulse-tracing-qvm)
  (alexandria:if-let ((fs (frame-state qvm frame)))
    (copy-structure fs)
    (make-frame-state :frequency *default-frame-frequency*)))

;;; TRANSITIONs

;;; TODO: right now we support basic quilt instructions: delay, fence, frame
;;; mutations, pulse, capture, raw-capture
;;; Do we want more?

(defmethod transition ((qvm pulse-tracing-qvm) (instr quil:delay))
  (incf (local-time qvm (quil:delay-qubit instr))
        (quil:delay-duration instr))

  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pulse-tracing-qvm) (instr quil:fence))
  (let ((latest (apply #'latest-time qvm (quil::fence-qubits instr))))
    (loop :for q :in (quil::fence-qubits instr)
          :do (setf (local-time qvm q) latest)))

  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pulse-tracing-qvm) (instr quil:simple-frame-mutation))
  (let* ((frame (quil:frame-mutation-target-frame instr))
         (val (quil:constant-value
               (quil:frame-mutation-value instr)))
         (fs (new-or-copied-state qvm frame)))

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
    (let ((left-state (new-or-copied-state qvm left-frame))
          (right-state (new-or-copied-state qvm left-frame)))
      (rotatef (frame-state-phase left-state) (frame-state-phase right-state))
      (setf (frame-state qvm left-frame) left-state
            (frame-state qvm right-frame) right-state)))

  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm pulse-tracing-qvm) instr)
  (unless (typep instr '(or quil:pulse quil:capture quil:raw-capture))
    (error "Cannot resolve timing information for non-quilt instruction ~A" instr))
  (let* ((qubits (quil::quilt-instruction-qubits instr))
         (start-time (apply #'latest-time qvm qubits))
         (end-time (+ start-time
                      (quil::quilt-instruction-duration instr (sample-rate qvm))))
         (frame-state (frame-state qvm (pulse-op-frame instr))))
    (unless frame-state                 ; TODO is this a reasonable default?
      (warn "Instruction ~A references an uninitialized frame." instr)
      (setf frame-state (make-frame-state :frequency *default-frame-frequency*)))
    (vector-push-extend (make-pulse-event
                         :instruction instr
                         :start-time start-time
                         :end-time end-time
                         :frame-state frame-state)
                        (pulse-event-log qvm))
    ;; update local times
    (loop :for q :in qubits
          :do (setf (local-time qvm q) end-time)))

  (incf (pc qvm))
  qvm)
