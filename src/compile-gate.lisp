;;;; src/compile-gate.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file handles compilation of Quil instructions to native code.

;;; Since compilation is tricky, we might as well compile this file as
;;; safely as we can. Note, that this doesn't reflect the optimization
;;; qualities of the compiled code.
(declaim #.*optimize-safely*)

(defun compile-lambda (form)
  "Compile the lambda form FORM into a FUNCTION object."
  (handler-bind (#+sbcl (sb-ext:compiler-note #'muffle-warning))
    (compile nil form)))

;;;;;;;;;;;;;;;;;;;;;;; GATE APPLICATION CODE ;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-complement-iteration (qubits wavefunction body-gen &key (dotimes-iterator 'cl:dotimes))
  "Generate the complement iteration loop for an operator acting on the qubits QUBITS operating on the wavefunction WAVEFUNCTION.

BODY-GEN should be a unary function which takes as an argument the symbol referring to the generated address, and produces a form that uses that address. This form should act as the operator.

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
         ;; Prepare the complement address. Make sure to inject bits
         ;; from LSB to MSB! So we iterate from highest to lowest,
         ;; pushing so that the lowest will be at the top of the list.
         ,@(let ((code nil))
             (do-nat-tuple (q (sort (copy-seq qubits) '>))
              (push `(setf ,addr (inject-bit ,addr ,q)) code))
             code)
         ;; Execute the body with ADDR bound.
         ,(funcall body-gen addr)))))

(defun generate-amplitude-address-code (address flags qubits)
  "Generate code (a single form) to modify a base address ADDRESS (as produced by a complement iteration loop; represented by a symbol bound to that address) with the flags FLAGS (a non-negative FIXNUM) and qubits QUBITS (a NAT-TUPLE).

This function is similar to the function SET-QUBIT-COMPONENTS-OF-AMPLITUDE-ADDRESS. See its documentation for details."
  (check-type address symbol)
  (check-type flags non-negative-fixnum)
  (check-type qubits nat-tuple)
  (loop :with code := address
        :for i :from 0
        :for q :across qubits
        :for bit := (ldb (byte 1 i) flags)
        :unless (zerop bit)
          :do (setf code `(dpb 1 (byte 1 ,q) ,code))
        :finally (return code)))

(defun generate-extraction-code (complement-address wavefunction qubits body-gen
                                 &key (generate-extractions t))
  "Generate LET-like code that extracts amplitudes designated by the qubits QUBITS.

COMPLEMENT-ADDRESS should be a symbol which should be (eventually) bound to the complement address, like that produced by GENERATE-COMPLEMENT-ITERATION.

WAVEFUNCTION should be a symbol which should be (eventually) bound to a wavefunction.

BODY-GEN should be a two-argument function. The first argument will be a list of symbols that will be bound to the wavefunction-addressed values in order. The second argument will be a list of SETF-able accessor forms for those values.

GENERATE-EXTRACTIONS will enable or disable the generation of the values. Setting this to NIL will cause the first argument of BODY-GEN to be NIL."
  (check-type complement-address symbol)
  (check-type wavefunction symbol)
  (check-type qubits nat-tuple)
  (check-type body-gen function)

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
  "Generate N x N matrix multiplication code.

MATRIX should be a symbol which should (eventually) be bound to a QUANTUM-OPERATOR object.

COLUMN should be a list of symbols all of which should (eventually) be bound to the vector being multiplied.

RESULT should be a list of SETF-able forms to which the result will be assigned."
  (check-type n non-negative-fixnum)
  (check-type matrix symbol)
  (check-type column alexandria:proper-list)
  (check-type result alexandria:proper-list)
  (assert (= n (length column) (length result)))
  `(progn
     ,@(loop :for i :below n
             :for r :in result
             :collect `(setf ,r
                             (+ ,@(loop :for j :below n
                                        :for c := (nth j column)
                                        :collect `(* ,c (aref ,matrix ,i ,j))))))))

(defun generate-gate-application-code (qubits)
  "Generate a lambda form which takes two arguments, a QUANTUM-OPERATOR and a QUANTUM-STATE, which efficiently applies that operator to that state, lifted from the Hilbert space designated by QUBITS.

QUBITS should be a NAT-TUPLE of qubits representing the Hilbert space."
  (check-type qubits nat-tuple)
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
  "Given a list of accessor forms ACCESSORS, generate a form which permutes those according to the permutation PERMUTATION.

This function is used to permute wavefunction amplitudes."
  (check-type permutation sequence)
  (check-type accessors alexandria:proper-list)
  `(progn
     ,@(loop :for (left . right) :in (permutation-to-transpositions permutation)
             :collect `(rotatef ,(nth left accessors)
                                ,(nth right accessors)))))

;;; TODO: Don't generate all of the extraction code, only that which
;;; is needed by the permutation.
(defun generate-permutation-gate-application-code (qubits permutation)
  "Generate a lambda form which takes one argument, a QUANTUM-STATE, which efficiently applies a permuting operator to that state, lifted from the Hilbert space designated by QUBITS.

PERMUTATION should be the permutation representation of that operator.

QUBITS should be a NAT-TUPLE of qubits representing the Hilbert space."
  (check-type qubits nat-tuple)
  (check-type permutation sequence)
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
    (make-hash-table :test 'equal)
  "A table mapping lists of qubit indexes representing Hilbert spaces to their compiled gate application functions.")

(defun find-or-make-apply-matrix-operator-function (qubits)
  "Find a matrix application function for the Hilbert subspace designated by QUBITS.

This function will compile new ones on-demand."
  (check-type qubits nat-tuple)
  (let ((key (cons ':gate (coerce qubits 'list))))
    (or (gethash key **apply-matrix-operator-functions**)
        (setf (gethash key **apply-matrix-operator-functions**)
              (compile-lambda
               (generate-gate-application-code qubits))))))

;; This function gets called at compile-time in apply-gate.lisp.
(defun warm-apply-matrix-operator-cache (&key (max-qubits 36))
  "Warm up the **APPLY-MATRIX-OPERATOR-FUNCTIONS** cache for Hilbert spaces B_i and B_i (x) B_j for 0 <= i, j <= MAX-QUBITS."
  (check-type max-qubits nat-tuple-cardinality)
  ;; Warm the 1q cache.
  (loop :for q :to max-qubits :do
    (find-or-make-apply-matrix-operator-function (nat-tuple q)))
  ;; Warm the 2q cache.
  (loop :for p :to max-qubits :do
    (loop :for q :to max-qubits :do
      (unless (= p q)
        (find-or-make-apply-matrix-operator-function (nat-tuple p q)))))
  nil)

(defclass compiled-gate-application (quil:gate-application)
  ((source-instruction :initarg :source-instruction
                       :accessor source-instruction
                       :documentation "The instruction that was compiled to produce this one.")
   (source-gate :initarg :source-gate
                :reader source-gate
                :documentation "The gate object being represented by this application.")
   (apply-operator :initarg :apply-operator
                   :reader compiled-gate-apply-operator
                   :documentation "The operator (a FUNCTION) which, at least as one of its arguments, modifies the wavefunction."))
  (:metaclass abstract-class)
  (:documentation "A representation of a compiled gate application."))

(defclass compiled-matrix-gate-application (compiled-gate-application)
  ((gate-matrix :initarg :gate-matrix
                :reader compiled-matrix
                :documentation "The (static) matrix represented by this application."))
  (:documentation "A compiled GATE-APPLICATION. Note that this is a subclass of GATE-APPLICATION."))

(defclass compiled-permutation-gate-application (compiled-gate-application)
  ()
  (:documentation "A compiled GATE-APPLICATION where the gate happens to be a permutation gate."))

(defmethod quil::print-instruction-generic ((instr compiled-gate-application) stream)
  (format stream "compiled{ ")
  (quil:print-instruction (source-instruction instr) stream)
  (format stream " }")
  nil)

(defgeneric compile-operator (op qubits parameters)
  (:documentation "Compile the operator OP into an efficient representation. Return two values:

    1. The class name to instantiate.

    2. The COMPILED-GATE-APPLICATION initargs.

If the gate can't be compiled, return (VALUES NIL NIL).")
  (:method ((op t) qubits parameters)
    (declare (ignore op parameters))
    (values nil nil)))

(defmethod compile-operator ((op quil:simple-gate) qubits parameters)
  (assert (null parameters) (parameters) "Parameters don't make sense for a SIMPLE-GATE.")
  (values 'compiled-matrix-gate-application
          `(:source-gate ,op
            :gate-matrix ,(magicl-matrix-to-quantum-operator
                           (quil:gate-matrix op))
            :apply-operator ,(find-or-make-apply-matrix-operator-function qubits))))

(defmethod compile-operator ((op quil:parameterized-gate) qubits parameters)
  (values 'compiled-matrix-gate-application
          `(:source-gate ,op
            :gate-matrix ,(magicl-matrix-to-quantum-operator
                           (apply #'quil:gate-matrix op parameters))
            :apply-operator ,(find-or-make-apply-matrix-operator-function qubits))))

(defmethod compile-operator ((op quil:permutation-gate) qubits parameters)
  (assert (null parameters) (parameters) "Parameters don't make sense for a SIMPLE-GATE.")
  (values 'compiled-permutation-gate-application
          `(:source-gate ,op
            :apply-operator ,(compile-lambda
                              (generate-permutation-gate-application-code
                               qubits
                               (quil:permutation-gate-permutation op))))))

(defgeneric compile-instruction (qvm isn)
  (:documentation "Compile the instruction ISN to some more efficient representation, if possible. May return the same ISN back."))

;;; Don't do anything by default.
(defmethod compile-instruction (qvm isn)
  (declare (ignore qvm))
  isn)

(defmethod compile-instruction (qvm (isn compiled-gate-application))
  (declare (ignore qvm))
  isn)

;;; XXX: This is a function that will probably be removed after
;;; there's more compilation sophistication. Here, we are just doing
;;; *anything* to get a gate out of a GATE-APPLICATION. We are
;;; forgoing opportunities to generate advanced or optimized
;;; representations from the definition itself.
(defun pull-teeth-to-get-a-gate (gate-app)
  "Produce a valid, applicable gate from the application GATE-APPLICATION."
  (check-type gate-app quil:gate-application)
  (if (quil::anonymous-gate-application-p gate-app)
      (quil::gate-application-gate gate-app)
      (if (slot-boundp gate-app 'quil::gate)
          (quil::gate-application-gate gate-app)
          (funcall
           (quil::operator-description-gate-lifter
            (quil:application-operator gate-app))
           (quil:gate-definition-to-gate
            (quil::gate-application-resolution gate-app))))))

(defmethod compile-instruction (qvm (isn quil:gate-application))
  ;; Reject compiling an operator with unknown parameters.
  ;;
  ;; TODO: Add this feature.
  (if (not (every #'quil::is-constant (quil:application-parameters isn)))
      isn
      (multiple-value-bind (class-name initargs)
          (compile-operator
           (pull-teeth-to-get-a-gate isn)
           (apply #'nat-tuple
                  (mapcar #'quil:qubit-index
                          (quil:application-arguments isn)))
           (mapcar #'quil:constant-value (quil:application-parameters isn)))
        (cond
          ((null class-name)
           isn)
          (t
           (let ((all-initargs
                   `(
                     ;; COMPILED-GATE initargs
                     :source-instruction ,isn
                                         ;; QUIL:GATE-APPLICATION initargs
                     :operator ,(quil:application-operator isn)
                     :parameters ,(quil:application-parameters isn)
                     :arguments ,(quil:application-arguments isn)
                     ,@(if (quil::anonymous-gate-application-p isn)
                           nil
                           `(:name-resolution ,(quil::gate-application-resolution isn)))
                     ,@(if (not (slot-boundp isn 'quil::gate))
                           nil
                           `(:gate ,(quil::gate-application-gate isn)))
                     ;; The rest of the inirargs
                     ,@initargs)))
             (apply #'make-instance class-name all-initargs)))))))
