;;;; app-src/profiled-qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

;;; Instruction Counting

(defclass profiled-qvm-mixin ()
  ((instructions-executed :initform 0
                          :accessor instructions-executed)))

(defclass profiled-pure-state-qvm (qvm:pure-state-qvm profiled-qvm-mixin)
  ())

(defclass profiled-depolarizing-qvm (qvm::depolarizing-qvm profiled-qvm-mixin)
  ())

(defvar *instruction-counter*)

(defmethod qvm:run :around ((qvm profiled-qvm-mixin))
  (let ((*instruction-counter* 0))
    (prog1 (call-next-method)
      (incf (instructions-executed qvm) *instruction-counter*))))

(defmethod qvm:transition :after ((qvm profiled-qvm-mixin) instr)
  (incf *instruction-counter*))
