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

(defmacro dotimes-parallel ((var num) &body body)
  `(dotimes (,var ,num)
     (declare (type non-negative-fixnum ,var))
     ,@body)
  #+ign
  (alexandria:once-only (num)
    `(if (< ,num (expt 2 *qubits-required-for-parallelization*))
         (dotimes (,var ,num) ,@body)
         (lparallel:pdotimes (,var ,num) ,@body))))

(defconstant +dotimes-iterator+ 'dotimes-parallel)

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
                                                ,(nat-tuple-cardinality qubits))))))
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

(defun gray-encode (n)
  "Compute the Nth Gray code."
  (check-type n (integer 0))
  (logxor n (ash n -1)))

(defun gray-bit (n)
  "Let G(n) be the Nth number in Gray code order with G(0) = 0, i.e., GRAY-ENCODE. Then

    (G (1+ n)) == (LOGXOR (G n) (ASH 1 (GRAY-BIT n)))."
  (check-type n (integer 0))
  (labels ((%gray-bit (n)
             (if (oddp n)
                 1
                 (1+ (%gray-bit (floor n 2))))))
    (1- (%gray-bit (1+ n)))))

;; The Gray code order is solely used for supposed efficiency. One
;; could generate code in linear order with DPB and the like.
(defun generate-amplitude-address-in-gray-order-bindings (indexes qubits base-address)
  "Generate the bindings for the variables in the list INDEXES, to the amplitude addresses specified BASE-ADDRESS and QUBITS in Gray code order.

The list INDEXES must have length 2^|QUBITS|."
  (check-type qubits nat-tuple)
  (assert (and (alexandria:proper-list-p indexes)
               (every #'symbolp indexes)))

  (let* ((n (expt 2 (nat-tuple-cardinality qubits))))
    (assert (= n (length indexes)))
    (list* `(,(first indexes) ,base-address)
           (loop :for i :from 1 :below n
                 :for last-g := (nth (gray-encode (1- i)) indexes)
                 :for g := (nth (gray-encode i) indexes)
                 :collect `(,g (logxor ,last-g ,(ash 1 (aref qubits (gray-bit (1- i))))))))))

(defun generate-amplitude-address-in-parallel-bindings (indexes qubits base-address)
  "Generate the bindings for the variables in the list INDEXES, to the amplitude addresses specified BASE-ADDRESS and nat-tuple QUBITS. The bindings can be bound in parallel with LET.

The list INDEXES must have length 2^|QUBITS|."
  (check-type qubits nat-tuple)
  (assert (and (alexandria:proper-list-p indexes)
               (every #'symbolp indexes)))

  (let* ((n (expt 2 (nat-tuple-cardinality qubits))))
    (assert (= n (length indexes)))
    (list* `(,(first indexes) ,base-address)
           (loop :for i :from 1 :below n
                 :for g :in (rest indexes)
                 :collect `(,g (logior ,base-address
                                       ,(loop :for j :from 0
                                              :for q :across qubits
                                              :when (logbitp j i)
                                                :sum (ash 1 q))))))))

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
    `(let ,(generate-amplitude-address-in-parallel-bindings indexes qubits complement-address)
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

    ... or it can be a QUANTUM-OPERATOR here.

COLUMN should be a list of symbols all of which should (eventually) be bound to the vector being multiplied.

RESULT should be a list of SETF-able forms to which the result will be assigned."
  (check-type n non-negative-fixnum)
  (check-type matrix (or symbol quantum-operator))
  (check-type column alexandria:proper-list)
  (check-type result alexandria:proper-list)
  (assert (= n (length column) (length result)))
  (etypecase matrix
    (symbol
     `(progn
        ,@(loop :for i :below n
                :for r :in result
                :collect `(setf ,r
                                (+ ,@(loop :for j :below n
                                           :for c := (nth j column)
                                           :collect `(* ,c (aref ,matrix ,i ,j))))))))
    (quantum-operator
     `(progn
        ,@(loop :for i :below n
                :for r :in result
                :for r-code := (loop :for j :below n
                                     :for c := (nth j column)
                                     :for mij := (aref matrix i j)
                                     :unless (zerop mij)
                                       :collect (if (= 1 mij)
                                                    c
                                                    `(* ,c ,(aref matrix i j))))
                ;; Avoid setting R1 = C1.
                :unless (and (= 1 (length r-code))
                             (symbolp (first r-code))
                             (eql (position r result)
                                  (position (first r-code) column)))
                  :collect `(setf ,r (+ ,@r-code)))))))

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
           :dotimes-iterator +dotimes-iterator+)))))

(defun generate-gate-application-code-with-matrix (qubits matrix)
  "Generate a lambda form which takes two arguments, a QUANTUM-OPERATOR and a QUANTUM-STATE, which efficiently applies that operator to that state, lifted from the Hilbert space designated by QUBITS.

QUBITS should be a NAT-TUPLE of qubits representing the Hilbert space."
  (check-type qubits nat-tuple)
  (let* ((num-gate-qubits (nat-tuple-cardinality qubits))
         (operator-size (expt 2 num-gate-qubits)))
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
                (generate-inner-matrix-multiply-code
                 operator-size
                 matrix
                 amps
                 arefs))))
           :dotimes-iterator +dotimes-iterator+)))))

(defvar *unrolling-size* 0 "Maximum number of loop body duplications in loop unrolling. Zero disables unrolling.")

;;; TODO: Parallelize
(defun generate-single-qubit-measurement-code (qubit)
  (declare (type nat-tuple-element qubit))
  (alexandria:with-gensyms (wavefunction size zero-probability address keep-zero inv-norm delete-address mask keep-address)
    `(lambda (,wavefunction)
       (declare ,*optimize-dangerously-fast*
                (type quantum-state ,wavefunction)
                (notinline random))
       (let ((,size (length ,wavefunction))
             (,zero-probability (flonum 0)))
         (declare (type non-negative-fixnum ,size)
                  (type flonum ,zero-probability))
         ;; Calculate the probability
         ,(cond
            ((zerop qubit)
             `(loop :for ,address :of-type non-negative-fixnum :below ,size :by 2 :do
               (incf ,zero-probability (probability (aref ,wavefunction ,address)))))
            (t
             (let ((jump (ash 1 qubit)))
               `(loop :with ,address :of-type non-negative-fixnum := 0
                      :while (< ,address ,size)
                      :do (progn
                            (loop :repeat ,jump :do
                              (incf ,zero-probability (probability (aref ,wavefunction ,address)))
                              (incf ,address))
                            ;; At this point, we will have a 1 in the
                            ;; QUBIT'th position.
                            (incf ,address ,jump))))))
         ;; Who do we spare?
         (let* ((,keep-zero (< (the flonum (random (flonum 1))) ,zero-probability))
                (,inv-norm (if ,keep-zero
                               (/ (sqrt (the (flonum 0) ,zero-probability)))
                               (/ (sqrt (the (flonum 0) (- (flonum 1) ,zero-probability)))))))
           (declare (type boolean ,keep-zero)
                    (type flonum ,inv-norm))
           ;; ADDRESS = to be kept
           ;; DELETE-ADDRESS = to be projected out
           ,(cond
              ((zerop qubit)
               `(loop :for ,keep-address :of-type non-negative-fixnum :from (if ,keep-zero 0 1) :below ,size :by 2
                      :for ,delete-address :of-type non-negative-fixnum := (logxor ,keep-address 1)
                      :do (setf (aref ,wavefunction ,keep-address) (* ,inv-norm (aref ,wavefunction ,keep-address))
                                (aref ,wavefunction ,delete-address) (cflonum 0))))
              (t
               (let ((jump (ash 1 qubit)))
                 `(loop :with ,address :of-type non-negative-fixnum := 0
                        :with ,mask := (if ,keep-zero 0 ,jump)
                        :while (< ,address ,size)
                        :do (progn
                              ;; decide on unrolling
                              ,(if (<= jump *unrolling-size*)
                                   ;; yes unroll
                                   `(let* ((,keep-address   (logior ,address ,mask))
                                           (,delete-address (logxor ,keep-address ,jump)))
                                      (declare (type non-negative-fixnum ,keep-address ,delete-address))
                                      ,@(loop
                                          :repeat jump
                                          :append `((setf (aref ,wavefunction ,keep-address) (* ,inv-norm (aref ,wavefunction ,keep-address))
                                                          (aref ,wavefunction ,delete-address) (cflonum 0))
                                                    ;; increment lower half of address
                                                    (incf ,keep-address)
                                                    (incf ,delete-address))))
                                   ;; no don't unroll
                                   `(loop :with ,keep-address :of-type non-negative-fixnum := (logior ,address ,mask)
                                          :with ,delete-address :of-type non-negative-fixnum := (logxor ,keep-address ,jump)
                                          :repeat ,jump
                                          :do
                                             (setf (aref ,wavefunction ,keep-address) (* ,inv-norm (aref ,wavefunction ,keep-address))
                                                   (aref ,wavefunction ,delete-address) (cflonum 0))
                                             ;; increment lower half of address
                                             (incf ,keep-address)
                                             (incf ,delete-address)))
                              ;; increment upper half of address
                              (incf ,address ,(* 2 jump)))))))
           ;; Return what we kept (aka measured).
           (if ,keep-zero 0 1))))))

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
         :dotimes-iterator +dotimes-iterator+))))


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

(defclass compiled-instruction ()       ; Explicitly do *NOT* subclass
                                        ; from QUIL:INSTRUCTION.
  ((source-instruction :initarg :source-instruction
                       :accessor source-instruction
                       :documentation "The instruction that was compiled to produce this one."))
  (:metaclass abstract-class))

(defclass compiled-gate-application (compiled-instruction quil:gate-application)
  ((source-gate :initarg :source-gate
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

(defclass compiled-parameterized-gate-application (compiled-gate-application)
  ())

(defclass compiled-permutation-gate-application (compiled-gate-application)
  ()
  (:documentation "A compiled GATE-APPLICATION where the gate happens to be a permutation gate."))

(defclass compiled-measurement (compiled-instruction quil:measurement)
  ((projector-operator :initarg :projector-operator
                       :reader projector-operator
                       :documentation "Projection function which takes a WAVEFUNCTION as an argument and returns the measured bit result.")))

(defmethod quil::print-instruction-generic ((instr compiled-instruction) stream)
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
  (let ((matrix (magicl-matrix-to-quantum-operator (quil:gate-matrix op))))
    (values 'compiled-matrix-gate-application
            `(:source-gate ,op
              :gate-matrix ,matrix
              :apply-operator ,(compile-lambda
                                (generate-gate-application-code-with-matrix qubits matrix))
              #+ignore (find-or-make-apply-matrix-operator-function qubits)))))

(defmethod compile-operator ((op quil:parameterized-gate) qubits parameters)
  (let ((matrix (magicl-matrix-to-quantum-operator
                 (apply #'quil:gate-matrix op parameters))))
    (values 'compiled-matrix-gate-application
            `(:source-gate ,op
              :gate-matrix ,matrix
              :apply-operator ,(compile-lambda
                                (generate-gate-application-code-with-matrix qubits matrix))
              #+ignore (find-or-make-apply-matrix-operator-function qubits)))))

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

;;; Don't do anything with an existing compiled instruction.
(defmethod compile-instruction (qvm (isn compiled-instruction))
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

(defmethod compile-instruction ((qvm pure-state-qvm) (isn quil:measurement))
  (let ((qubit (quil:measurement-qubit isn)))
    (make-instance 'compiled-measurement
                   :source-instruction isn
                   :qubit qubit
                   :projector-operator
                   (compile-lambda
                    (generate-single-qubit-measurement-code (quil:qubit-index qubit))))))

(defmethod compile-instruction ((qvm pure-state-qvm) (isn quil:gate-application))
  (cond
    ;; We don't handle non-constant arguments yet.
    ((not (every #'quil::is-constant (quil:application-parameters isn)))
     isn)
    (t
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
            (apply #'make-instance class-name all-initargs))))))))

(defun compiled-RX-gate (isn)
  (let ((all-initargs
          (let ((qubit (qvm::nat-tuple (quil::qubit-index (first (quil:application-arguments isn))))))
            `(:source-instruction ,isn
              ;; QUIL:GATE-APPLICATION initargs
              :operator ,(quil:application-operator isn)
              :parameters ,(quil:application-parameters isn)
              :arguments ,(quil:application-arguments isn)
              :source-gate nil
              ,@(if (quil::anonymous-gate-application-p isn)
                    nil
                    `(:name-resolution ,(quil::gate-application-resolution isn)))
              ,@(if (not (slot-boundp isn 'quil::gate))
                    nil
                    `(:gate ,(quil::gate-application-gate isn)))
              :apply-operator ,(compile-lambda
                                `(lambda (theta wavefunction)
                                   (declare ,*optimize-dangerously-fast*
                                            (type quantum-state wavefunction)
                                            (type double-float theta))
                                   (funcall
                                    ,(generate-gate-application-code qubit)
                                    (magicl-matrix-to-quantum-operator
                                     (funcall ,(slot-value
                                                (gethash "RX" quil::**default-gate-definitions**)
                                                'quil::matrix-function)
                                              theta))
                                    wavefunction)))))))
    (apply #'make-instance 'compiled-parameterized-gate-application all-initargs)))

;;; Measure chains

(defclass measure-all ()
  ((storage :initarg :storage
            :reader measure-all-storage
            :documentation "A list of qubit-mref pairs to store, in order."))
  (:documentation "A pseudo-instruction for measuring all qubits simultaneously."))

(defmethod quil::print-instruction-generic ((instr measure-all) stream)
  (format stream "{MEASURE-ALL to ~D location~:P}     # pseudo-instruction"
          (length (measure-all-storage instr))))

(defun measure-chain-at (i code)
  "Is there a measure chain at I in CODE?"
  (let ((n 0))
    (loop :for k :from i :below (length code)
          :for isn := (elt code k)
          :do (if (typep isn 'quil:measurement)
                  (incf n)
                  (loop-finish))
          :finally (return n))))

(defun find-measure-chains (n code)
  "Find measure chains containing all qubits 0 <= q < n."
  (let ((chains nil))
    (loop :for i :below (length code)
          :for chain := (measure-chain-at i code)
          :when (plusp chain)
            :do (progn
                  (unless (< chain n)
                    (let ((measures (subseq code i (+ i chain))))
                      (loop :with bitset := 0
                            :for m :across measures
                            :for q := (quil:qubit-index (quil:measurement-qubit m))
                            :do (setf (ldb (byte 1 q) bitset) 1)
                            :finally (when (= bitset (1- (expt 2 n)))
                                       (push (cons i measures) chains)))))
                  (incf i chain)))
    (nreverse chains)))

(defun measure-chain-to-instruction (chain)
  "Convert a measure chain into a MEASURE-ALL pseudo-instruction."
  (make-instance 'measure-all
                 :storage (loop :for isn :across chain
                                :for q := (quil:qubit-index
                                           (quil:measurement-qubit isn))
                                :when (typep isn 'quil:measure)
                                  :collect (cons q (quil:measure-address isn)))))

(defun compile-measure-chains (code n)
  "Given a code vector CODE of N qubits, compile all measure chains."
  (let ((chains (find-measure-chains n code)))
    (if (endp chains)
        code
        (let ((code (copy-seq code)))
          (loop :for (start . chain) :in chains
                :for end := (+ start (length chain))
                :do (fill code (make-instance 'quil:no-operation) :start start :end end)
                    (setf (elt code start) (measure-chain-to-instruction chain)))
          code))))
