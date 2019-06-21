;;;; stabilizer-qvm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file contains an implementation of the QVM that can execute
;;; Clifford gates (and only Clifford gates!) efficiently.

(global-vars:define-global-var **cliffords**
    (make-hash-table :test 'equal
                     #+sbcl :synchronized
                     #+sbcl t)
  "Map string gate name to a CLIFFORD object.")

(global-vars:define-global-var **clifford-operations**
    (quil.clifford:make-clifford-hash-table)
  "Map a CLIFFORD to a compiled tableau function.")

(defclass stabilizer-qvm (classical-memory-mixin)
  ((tableau :initarg :tableau
            :reader stabilizer-qvm-tableau))
  (:documentation "A QVM that can efficiently simulate Clifford circuits and measurement."))

(defun make-stabilizer-qvm (num-qubits &key (classical-memory-model quil:**empty-memory-model**))
  (check-type num-qubits unsigned-byte)
  (check-type classical-memory-model quil:memory-model)
  (make-instance 'stabilizer-qvm
                 :tableau (cl-quil.clifford::make-tableau-zero-state num-qubits)
                 :classical-memory-subsystem (make-instance 'classical-memory-subsystem
                                                             :classical-memory-model
                                                             classical-memory-model)))

(defmethod number-of-qubits ((qvm stabilizer-qvm))
  (cl-quil.clifford::tableau-qubits (stabilizer-qvm-tableau qvm)))

(defun gate-application-to-clifford (gate-app)
  (check-type gate-app quil:gate-application)
  (cond
    ((quil::plain-operator-p (quil::application-operator gate-app))
     (let ((name (quil::operator-description-name (quil::application-operator gate-app))))
       (multiple-value-bind (clifford exists?)
           (gethash name **cliffords**)
         (unless exists?
           (setf (gethash name **cliffords**)
                 (quil.clifford:matrix-to-clifford
                  (quil:gate-matrix
                   (pull-teeth-to-get-a-gate gate-app))))
           (setf clifford (gethash name **cliffords**)))
         clifford)))
    (t (quil.clifford:matrix-to-clifford
        (quil:gate-matrix
         (pull-teeth-to-get-a-gate gate-app))))))

(defun compile-clifford (c &key (cache t))
  (check-type c quil.clifford:clifford)
  (multiple-value-bind (compiled exists?)
      (gethash c **clifford-operations**)
    (cond
      (exists? compiled)
      (t
       (when *transition-verbose*
         (format *trace-output* "~&;      compiling Clifford gate...~%"))
       (let ((compiled (compile-lambda (cl-quil.clifford::compile-tableau-operation c))))
         (when cache
           (setf (gethash c **clifford-operations**) compiled))
         compiled)))))


;;;;;;;;;;;;;;;;;;;;;;;;; TRANSITION Methods ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod measure ((qvm stabilizer-qvm) q)
  (values qvm (cl-quil.clifford::tableau-measure (stabilizer-qvm-tableau qvm) q)))

(defmethod transition ((qvm stabilizer-qvm) (instr quil:reset))
  (cl-quil.clifford::zero-out-tableau (stabilizer-qvm-tableau qvm))
  (incf (pc qvm))
  qvm)

(defmethod transition ((qvm stabilizer-qvm) (instr quil:reset-qubit))
  (let ((q (quil:qubit-index (quil:reset-qubit-target instr))))
    ;; Do the measurement: MEASURE q
    (multiple-value-bind (measured-qvm measured-bit)
        (measure qvm q)
      ;; Conditionally do an X.
      (cond
        ((= 1 measured-bit) (transition qvm (quil::build-gate "X" () q)))
        (t (incf (pc measured-qvm))))
      measured-qvm)))

(defmethod transition ((qvm stabilizer-qvm) (instr quil:measure))
  (incf (pc qvm))
  (measure-and-store qvm
                     (quil:qubit-index (quil:measurement-qubit instr))
                     (quil:measure-address instr)))

(defmethod transition ((qvm stabilizer-qvm) (instr quil:measure-discard))
  (incf (pc qvm))
  (measure qvm
           (quil:qubit-index (quil:measurement-qubit instr))))

(defmethod transition ((qvm stabilizer-qvm) (instr quil:gate-application))
  (let* ((clifford (gate-application-to-clifford instr))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))
    ;; Do some error checking.
    (let ((given-qubits (length qubits))
          (expected-qubits (quil.clifford:num-qubits clifford)))
      (unless (= given-qubits expected-qubits)
        (error 'invalid-instruction-encountered
               :instruction instr
               :because (format nil "I attempted to apply the ~D-qubit gate to ~D qubit~:P"
                                expected-qubits
                                given-qubits))))
    ;; Will aggressively cache any compiled output.
    (apply (compile-clifford clifford)
           (stabilizer-qvm-tableau qvm)
           qubits)
    (incf (pc qvm))
    qvm))


;;; A little test fixture.

(defclass clifford-application (quil:application)
  ((clifford :initarg :clifford
             :reader clifford-application-clifford))
  (:documentation "A gate application that's actually a Clifford."))

(defmethod quil::print-instruction-generic ((instr clifford-application) stream)
  (format stream "~{ ~/cl-quil:instruction-fmt/~}" 
          (mapcar (lambda (thing) (quil:print-instruction thing nil))
                  (quil:application-arguments instr))))

(defmethod print-object ((o clifford-application) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~Dq" (quil.clifford:num-qubits
                          (clifford-application-clifford o)))))

(defun make-clifford-application (clifford &rest qubits)
  (check-type clifford quil.clifford:clifford)
  (assert (= (quil.clifford:num-qubits clifford) (length qubits)))
  (make-instance 'clifford-application :clifford clifford
                                       :arguments (mapcar #'quil:qubit qubits)))

(defun random-qubits (num-qubits max-qubit)
  (loop :with qubits := nil
        :while (plusp num-qubits)
        :do (let ((qubit (random (1+ max-qubit))))
              (unless (member qubit qubits)
                (push qubit qubits)
                (decf num-qubits)))
        :finally (return qubits)))

(defun random-range (a b)
  (+ a (random (- b a))))

(defun random-clifford-program (length max-clifford-arity num-qubits &key (measure nil))
  (assert (>= num-qubits max-clifford-arity))
  (let* ((code (make-array (+ length (if measure num-qubits 0))))
         (program (make-instance 'quil:parsed-program :executable-code code)))
    (dotimes (i length)
      (let ((arity (random-range 1 (1+ max-clifford-arity))))
        (setf (aref code i)
              (apply #'make-clifford-application
                     (cl-quil.clifford:random-clifford arity)
                     (loop :for q :from (1- arity) :downto 0 :collect q)))))
    (when measure
      (dotimes (i num-qubits)
        (let ((j (+ length i)))
          (setf (aref code j)
                (make-instance 'quil:measure-discard :qubit (quil:qubit i))))))
    ;; hack: cheat so we don't run this transform
    (quil::record-transform 'quil::patch-labels program)
    program))

(defmethod transition ((qvm stabilizer-qvm) (instr clifford-application))
  (let* ((clifford (clifford-application-clifford instr))
         (qubits (mapcar #'quil:qubit-index (quil:application-arguments instr))))
    ;; Do some error checking.
    (let ((given-qubits (length qubits))
          (expected-qubits (quil.clifford:num-qubits clifford)))
      (unless (= given-qubits expected-qubits)
        (error 'invalid-instruction-encountered
               :instruction instr
               :because (format nil "I attempted to apply the ~D-qubit gate to ~D qubit~:P"
                                expected-qubits
                                given-qubits))))
    ;; Don't cache these guys. They're probably made up Clifford gates.
    (apply (compile-clifford clifford :cache nil)
           (stabilizer-qvm-tableau qvm)
           qubits)
    (incf (pc qvm))
    qvm))

(defmethod transition ((qvm pure-state-qvm) (instr clifford-application))
  (transition qvm (make-instance 'quil:gate-application
                                 :operator (quil:named-operator "dummy")
                                 :gate (make-instance 'quil:simple-gate :matrix (cl-quil.clifford::clifford-to-matrix (clifford-application-clifford instr)))
                                 :parameters nil
                                 :arguments (quil:application-arguments instr))))
