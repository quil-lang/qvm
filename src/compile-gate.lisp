;;;; src/compile-gate.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(declaim #.*optimize-safely*)

(defun compile-lambda (form)
  "Compile the lambda form FORM."
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (compile nil form)))

;; For reference only. The following code is from the qHipster
;; paper. We don't follow it because the two loops are difficult to
;; parallelize evenly.
#+#:ignore
(defun apply-1q-gate (matrix wf qubit)
  (declare #.*optimize-dangerously-fast*
           (type quantum-operator matrix)
           (type quantum-state wf)
           (type nat-tuple-element qubit))
  (let ((stride (expt 2 qubit)))
    (lparallel:pdotimes (g (floor (length wf) (expt 2 (1+ qubit))))
      (declare (type amplitude-address g))
      (let ((g (* g (expt 2 (1+ qubit)))))
        (declare (type amplitude-address g))
        (dotimes (i stride)
          (declare (type amplitude-address i))
          (let ((i (+ g i)))
            (declare (type amplitude-address i))
            (let ((a (aref wf i))
                  (b (aref wf (+ i stride))))
              (setf (aref wf i)            (+ (* a (aref matrix 0 0)) (* b (aref matrix 0 1)))
                    (aref wf (+ i stride)) (+ (* a (aref matrix 1 0)) (* b (aref matrix 1 1)))))))))))


;;;;;;;;;;;;;;;;;;;;;;; GATE APPLICATION CODE ;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-complement-iteration (qubits wavefunction body-gen &key (dotimes-iterator 'cl:dotimes))
  "Generate the complement iteration loop for an operator acting on the qubits QUBITS operating on the wavefunction WAVEFUNCTION.

BODY-GEN should be a unary function which takes as an argument the symbol referring to the generated address, and produces a form that uses that address.

DOTIMES-ITERATOR specifies the DOTIMES-like macro that is used for iteration."
  (check-type qubits nat-tuple)
  (check-type wavefunction symbol)
  (check-type body-gen function)
  (check-type dotimes-iterator symbol)
  (let ((addr (alexandria:format-symbol nil "ADDR")))
    `(,dotimes-iterator (i (the non-negative-fixnum
                                (expt 2 (the nat-tuple-cardinality
                                             (- (wavefunction-qubits ,wavefunction)
                                                (nat-tuple-cardinality ,qubits))))))
       (declare (type non-negative-fixnum i))
       (let ((,addr i))
         (declare (type amplitude-address ,addr))
         ;; Prepare the complement address.
         ,@(let ((code nil))
            (do-nat-tuple (q qubits)
              (push `(setf ,addr (inject-bit ,addr ,q)) code))
             code)
         ;; Execute the body with ADDR bound.
         ,(funcall body-gen addr)))))

(defun generate-amplitude-address-code (address flags qubits)
  (check-type address symbol)
  (check-type flags non-negative-fixnum)
  (loop :with code := address
        :for i :from 0
        :for q :across qubits
        :for bit := (ldb (byte 1 i) flags)
        :unless (zerop bit)
          :do (setf code `(dpb 1 (byte 1 ,q) ,code))
        :finally (return code)))

(defun generate-extraction-code (complement-address wavefunction qubits body-gen
                                 &key (generate-extractions t))
  "BODY-GEN is a function which takes a list of Z's and a list of AREF's."
  (check-type complement-address symbol)
  (check-type wavefunction symbol)

  (let* ((operator-size (expt 2 (nat-tuple-cardinality qubits)))
         (amps (if (not generate-extractions)
                   nil
                   (loop :for i :below operator-size
                         :collect (alexandria:format-symbol nil "Z~D" i))))
         (indexes (loop :for i :below operator-size
                        :collect (alexandria:format-symbol nil "I~D" i)))
         (arefs (loop :for index :in indexes
                      :collect `(aref ,wavefunction ,index))))
    `(let ,(loop :for i :below operator-size
                 :for index :in indexes
                 :collect `(,index (the non-negative-fixnum
                                        ,(generate-amplitude-address-code complement-address
                                                                          i
                                                                          qubits))))
       (declare (type amplitude-address ,@indexes)
                (dynamic-extent ,@indexes)
                (ignorable ,@indexes))
       (let ,(if (not generate-extractions)
                 '()
                 (loop :for amp :in amps
                       :for aref :in arefs
                       :collect `(,amp ,aref)))
         ,@(if (not generate-extractions)
               nil
               `((declare (type cflonum ,@amps)
                          (dynamic-extent ,@amps)
                          (ignorable ,@amps))))
         ,(funcall body-gen amps arefs)
         nil))))

(defun generate-inner-matrix-multiply-code (n matrix column result)
  (check-type n non-negative-fixnum)
  (check-type matrix symbol)
  (check-type column alexandria:proper-list)
  (check-type result alexandria:proper-list)
  `(progn
     ,@(loop :for i :below n
             :for r :in result
             :collect `(setf ,r
                             (+ ,@(loop :for j :below n
                                        :for c := (nth j column)
                                        :collect `(* ,c (aref ,matrix ,i ,j))))))))

(defun generate-gate-application-code (qubits)
  (let* ((num-gate-qubits (nat-tuple-cardinality qubits))
         (operator-size (expt 2 num-gate-qubits)))
    (alexandria:with-gensyms (operator wavefunction)
      `(lambda (,operator ,wavefunction)
         (declare ,*optimize-dangerously-fast*
                  (type quantum-operator ,operator)
                  (type quantum-state ,wavefunction))
         ,(generate-complement-iteration
           qubits
           wavefunction
           (lambda (addr)
             (generate-extraction-code
              addr
              wavefunction
              qubits
              (lambda (amps arefs)
                (generate-inner-matrix-multiply-code
                 operator-size
                 operator
                 amps
                 arefs))))
           :dotimes-iterator 'lparallel:pdotimes)))))


;;;;;;;;;;;;;;;;;;;;;;;;; PERMUTATION GATES ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun permutation-to-transpositions (permutation)
  "Decompose a permutation PERMUTATION represented as a sequence of non-negative integers into a list of transpositions represented as conses."
  (let ((indices (copy-seq permutation))
        (swaps nil))
    (dotimes (dest (length indices) (nreverse swaps))
      (let ((src (elt indices dest)))
        (loop :while (< src dest) :do
          (setf src (elt indices src)))
        (when (/= src dest)
          (push (cons src dest) swaps))))))

(defun generate-permutation-gate-code (permutation accessors)
  "Generate an efficient unary function which modifies a quantum state according to the permutation PERMUTATION."
  `(progn
     ,@(loop :for (left . right) :in (permutation-to-transpositions permutation)
             :collect `(rotatef ,(nth left accessors)
                                ,(nth right accessors)))))

;;; TODO: Don't generate all of the extraction code, only that which
;;; is needed by the permutation.
(defun generate-permutation-gate-application-code (qubits permutation)
  (alexandria:with-gensyms (wavefunction)
    `(lambda (,wavefunction)
       (declare ,*optimize-dangerously-fast*
                (type quantum-state ,wavefunction))
       ,(generate-complement-iteration
         qubits
         wavefunction
         (lambda (addr)
           (generate-extraction-code
            addr
            wavefunction
            qubits
            (lambda (amps arefs)
              (declare (ignore amps))
              (generate-permutation-gate-code
               permutation
               arefs))
            :generate-extractions nil))
         :dotimes-iterator 'lparallel:pdotimes))))


;;;;;;;;;;;;;;;;;;;;;;; APPLY OPERATOR CACHING ;;;;;;;;;;;;;;;;;;;;;;;

(global-vars:define-global-var **apply-matrix-operator-functions**
  (make-hash-table :test 'equal))

(defun find-or-make-apply-matrix-operator-function (qubits)
  (check-type qubits nat-tuple)
  (let ((key (cons ':gate (coerce qubits 'list))))
    (or (gethash key **apply-matrix-operator-functions**)
        (setf (gethash key **apply-matrix-operator-functions**)
              (compile-lambda
               (generate-gate-application-code qubits))))))

;; This function gets called at compile-time in apply-gate.lisp.
(defun warm-apply-matrix-operator-cache (&key (max-qubits 36))
  ;; Warm the 1q cache.
  (loop :for q :from 1 :to max-qubits :do
    (find-or-make-apply-matrix-operator-function (nat-tuple q)))
  ;; Warm the 2q cache.
  (loop :for p :from 1 :to max-qubits :do
    (loop :for q :from 1 :to max-qubits :do
      (unless (= p q)
        (find-or-make-apply-matrix-operator-function (nat-tuple p q)))))
  nil)

(defclass compiled-gate ()
  ((source-instruction :initarg :source-instruction
                       :accessor source-instruction)
   (source-gate :initarg :source-gate
                :reader source-gate)
   (apply-operator :initarg :apply-operator
                   :reader compiled-gate-apply-operator))
  (:metaclass abstract-class))

(defclass compiled-matrix-gate (compiled-gate)
  ((gate-matrix :initarg :gate-matrix
                :reader compiled-matrix)))

(defclass compiled-permutation-gate (compiled-gate)
  ())

(defmethod quil::print-instruction ((instr compiled-gate) stream)
  (format stream "compiled{ ")
  (quil::print-instruction (source-instruction instr) stream)
  (format stream " }")
  nil)

(defgeneric compile-operator (op qubits parameters)
  (:documentation "Compile the operator OP into an efficient representation, or NIL if not possible.")
  (:method ((op t) qubits parameters)
    (declare (ignore op parameters))
    nil))

(defmethod compile-operator ((op quil:simple-gate) qubits parameters)
  (assert (null parameters) (parameters) "Parameters don't make sense for a SIMPLE-GATE.")
  (make-instance 'compiled-matrix-gate
                 :source-gate op
                 :gate-matrix (magicl-matrix-to-quantum-operator
                               (quil:gate-matrix op))
                 :apply-operator (find-or-make-apply-matrix-operator-function qubits)))

(defmethod compile-operator ((op quil:parameterized-gate) qubits parameters)
  (make-instance 'compiled-matrix-gate
                 :source-gate op
                 :gate-matrix (magicl-matrix-to-quantum-operator
                               (apply #'quil:gate-matrix op parameters))
                 :apply-operator (find-or-make-apply-matrix-operator-function qubits)))

(defmethod compile-operator ((op quil:permutation-gate) qubits parameters)
  (assert (null parameters) (parameters) "Parameters don't make sense for a SIMPLE-GATE.")
  (make-instance 'compiled-permutation-gate
                 :source-gate op
                 :apply-operator (compile-lambda
                                  (generate-permutation-gate-application-code
                                   qubits
                                   (quil:permutation-gate-permutation op)))))

(defgeneric compile-instruction (qvm isn)
  (:documentation "Compile the instruction ISN to some more efficient representation, if possible. May return ISN back."))

;;; Don't do anything by default.
(defmethod compile-instruction (qvm isn)
  (declare (ignore qvm))
  isn)

(defmethod compile-instruction (qvm (isn quil:gate-application))
  (cond
    ;; Don't do anything for SWAPs.
    (quil:*recognize-swap-specially* isn)

    ;; Otherwise, we're free to compile.
    (t
     (alexandria:if-let (it (compile-operator
                             (lookup-gate qvm (quil:application-operator isn))
                             (apply #'nat-tuple
                                    (mapcar #'quil:qubit-index
                                            (quil:application-arguments isn)))
                             (mapcar #'quil:constant-value (quil:application-parameters isn))))
       (progn
         (setf (source-instruction it) isn)
         it)
       isn))))

