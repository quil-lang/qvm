;;;; path.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; In this file, we implement simulation of a straight-line Quil
;;; program without measurement. The technique used is (maybe
;;; informally) called the Multi-Amplitude Discrete Path Simulation
;;; (MADPI).
;;;
;;; Classical states are represented as objects of the Lisp type
;;; UNSIGNED-BYTE.

(defun list-classical-states (classical-state qubits)
  "List all of the bitstring states in the order that QUBITS are specified, using CLASSICAL-STATE as the base state."
  (check-type classical-state unsigned-byte)
  (let ((states nil))
    (map-reordered-amplitudes
     classical-state
     (lambda (combo index)
       (declare (ignore combo))
       (push index states))
     qubits)
    (nreverse states)))

(defun map-classical-state (instruction classical-state)
  "Map the classical state CLASSICAL-STATE to its component amplitudes under the transformation dictated by the Quil instruction INSTRUCTION, which should be a gate application.

Return two values:

    1. A list of the classical states that this state maps to.

    2. A list of complex amplitude factors associated with those states."
  (check-type instruction quil:gate-application)
  (let* ((m (apply #'quil:gate-matrix
                   (pull-teeth-to-get-a-gate instruction)
                   (mapcar #'quil:constant-value (quil:application-parameters instruction))))
         (qubits (apply #'nat-tuple
                        (mapcar #'quil:qubit-index
                                (quil:application-arguments instruction))))
         (column 0))
    ;; Calculate the column.
    (do-nat-tuple (q (reverse qubits))  ; LSB -> MSB order
      (setf column (logior (ash column 1)
                           (ldb (byte 1 q) classical-state))))

    ;; Read off the entries.
    (loop :for row :below (expt 2 (nat-tuple-cardinality qubits))
          :collect (magicl:tref m row column) :into amplitudes
          :finally (return (values (list-classical-states classical-state qubits)
                                   amplitudes)))))


(defun path-simulate (parsed-prog initial-classical-state final-classical-states)
  "Simulate the parsed program PARSED-PROG starting with the initial classical state INITIAL-CLASSICAL-STATE and ending with the final classical states FINAL-CLASSICAL-STATES. FINAL-CLASSICAL-STATES should be a list of classical states.

Return a list of amplitudes associated with the classical states.

PARSED-PROG must be a program that only contains gate applications."
  (check-type parsed-prog quil:parsed-program)
  (check-type initial-classical-state unsigned-byte)
  (check-type final-classical-states sequence)
  (assert (every (lambda (isn) (typep isn 'quil:gate-application))
                 (quil:parsed-program-executable-code parsed-prog))
          (parsed-prog)
          "Only gate applications are allowed for PATH-SIMULATE.")
  (let ((sums (make-array (length final-classical-states)
                          :element-type 'cflonum
                          :initial-element (cflonum 0))))
    (declare (type (simple-array cflonum (*)) sums))
    (labels ((accumulate-amplitude (classical-state amplitude)
               (declare (type cflonum amplitude))
               (alexandria:when-let (p (position classical-state final-classical-states))
                 (incf (aref sums p) amplitude)))

             (descend (program amplitude classical-state)
               (declare (type cflonum amplitude))
               (cond
                 ;; Don't descend if we've reached a (multiplicative)
                 ;; zero factor.
                 ((zerop amplitude)
                  nil)

                 ;; At the end of the Quil program, accumulate the
                 ;; amplitude.
                 ((endp program)
                  (accumulate-amplitude classical-state amplitude))

                 ;; "We need to go deeper."
                 ;;
                 ;;             —Dom Cobb
                 (t
                  (destructuring-bind (isn . rest-program) program
                    (flet ((visit-state (amplitude-factor mapped-classical-state)
                             (descend rest-program (* amplitude amplitude-factor) mapped-classical-state)))
                      (declare (dynamic-extent #'visit-state))
                      (multiple-value-bind (amplitude-factors mapped-classical-states)
                          (map-classical-state isn classical-state)
                        (mapc #'visit-state mapped-classical-states amplitude-factors))))))
               nil))
      ;; Simulate the program.
      (descend (coerce (quil:parsed-program-executable-code parsed-prog) 'list)
               (cflonum 1)
               initial-classical-state)
      ;; Return the sums.
      sums)))

(defun wavefunction-from-path-simulation (parsed-prog)
  "Compute the wavefunction of the program PARSED-PROG using path simulation."
  (path-simulate parsed-prog 0 (alexandria:iota (expt 2 (quil:qubits-needed parsed-prog)))))


