;;;; transition-classical-instructions.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(defun ensure-signed-integer (n bits)
  (- (mod (+ n (expt 2 (1- bits))) (expt 2 bits)) (expt 2 (1- bits))))

(defun ensure-s64 (n)
  "Ensure that an integer N is in range of a 64-bit 2's complement signed integer. Wrap-around if needed."
  (ensure-signed-integer n 64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NEG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-negate-real))
  (let ((mref (quil:classical-target instr)))
    (setf (dereference-mref qvm mref)
          (- (dereference-mref qvm mref))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-negate-integer))
  (let ((mref (quil:classical-target instr)))
    (setf (dereference-mref qvm mref)
          (ensure-s64 (- (dereference-mref qvm mref)))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NOT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-not-bit))
  (let ((mref (quil:classical-target instr)))
    (setf (dereference-mref qvm mref)
          (- 1 (dereference-mref qvm mref))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-not-octet))
  (let ((mref (quil:classical-target instr)))
    (setf (dereference-mref qvm mref)
          (ldb (byte 8 0) (lognot (dereference-mref qvm mref)))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-not-integer))
  (let ((mref (quil:classical-target instr)))
    (setf (dereference-mref qvm mref)
          (ldb (byte 64 0) (lognot (dereference-mref qvm mref)))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AND ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-binary-transition (class compute-fn accessor)
  `(defmethod transition ((qvm pure-state-qvm) (instr ,class))
     (let ((left/target (quil:classical-left-operand instr))
           (right (quil:classical-right-operand instr)))
       (,compute-fn qvm
                    left/target
                    (dereference-mref qvm left/target)
                    ,(subst 'right '* accessor)))
     (setf (pc qvm) (1+ (pc qvm)))
     qvm))

(defun %perform-and (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref) (logand left right)))

(define-binary-transition quil:classical-and-bit/bit
  %perform-and (dereference-mref qvm *))
(define-binary-transition quil:classical-and-integer/integer
  %perform-and (dereference-mref qvm *))
(define-binary-transition quil:classical-and-octet/octet
  %perform-and (dereference-mref qvm *))
(define-binary-transition quil:classical-and-bit/immediate
  %perform-and (quil:constant-value *))
(define-binary-transition quil:classical-and-integer/immediate
  %perform-and (quil:constant-value *))
(define-binary-transition quil:classical-and-octet/immediate
  %perform-and (quil:constant-value *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %perform-ior (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref) (logior left right)))

(define-binary-transition quil:classical-inclusive-or-bit/bit
  %perform-ior (dereference-mref qvm *))
(define-binary-transition quil:classical-inclusive-or-integer/integer
  %perform-ior (dereference-mref qvm *))
(define-binary-transition quil:classical-inclusive-or-octet/octet
  %perform-ior (dereference-mref qvm *))
(define-binary-transition quil:classical-inclusive-or-bit/immediate
  %perform-ior (quil:constant-value *))
(define-binary-transition quil:classical-inclusive-or-integer/immediate
  %perform-ior (quil:constant-value *))
(define-binary-transition quil:classical-inclusive-or-octet/immediate
  %perform-ior (quil:constant-value *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; XOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %perform-xor (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref) (logxor left right)))

(define-binary-transition quil:classical-exclusive-or-bit/bit
  %perform-xor (dereference-mref qvm *))
(define-binary-transition quil:classical-exclusive-or-integer/integer
  %perform-xor (dereference-mref qvm *))
(define-binary-transition quil:classical-exclusive-or-octet/octet
  %perform-xor (dereference-mref qvm *))
(define-binary-transition quil:classical-exclusive-or-bit/immediate
  %perform-xor (quil:constant-value *))
(define-binary-transition quil:classical-exclusive-or-integer/immediate
  %perform-xor (quil:constant-value *))
(define-binary-transition quil:classical-exclusive-or-octet/immediate
  %perform-xor (quil:constant-value *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %perform-move (qvm target-mref left right)
  (declare (ignore left))
  (setf (dereference-mref qvm target-mref) right))

(define-binary-transition quil:classical-move-octet/octet
  %perform-move (dereference-mref qvm *))
(define-binary-transition quil:classical-move-bit/bit
  %perform-move (dereference-mref qvm *))
(define-binary-transition quil:classical-move-integer/integer
  %perform-move (dereference-mref qvm *))
(define-binary-transition quil:classical-move-real/real
  %perform-move (dereference-mref qvm *))
(define-binary-transition quil:classical-move-octet/immediate
  %perform-move (quil:constant-value *))
(define-binary-transition quil:classical-move-bit/immediate
  %perform-move (quil:constant-value *))
(define-binary-transition quil:classical-move-integer/immediate
  %perform-move (quil:constant-value *))
(define-binary-transition quil:classical-move-real/immediate
  %perform-move (quil:constant-value *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXCHANGE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-exchange))
  (rotatef (dereference-mref qvm (quil:classical-left-operand instr))
           (dereference-mref qvm (quil:classical-right-operand instr)))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONVERT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-convert-integer/real))
  (let ((dst (quil:classical-left-operand instr))
        (src (quil:classical-right-operand instr)))
    (setf (dereference-mref qvm dst) (ensure-s64 (round (dereference-mref qvm src)))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-convert-integer/bit))
  (let ((dst (quil:classical-left-operand instr))
        (src (quil:classical-right-operand instr)))
    (setf (dereference-mref qvm dst) (ensure-s64 (dereference-mref qvm src))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-convert-real/integer))
  (let ((dst (quil:classical-left-operand instr))
        (src (quil:classical-right-operand instr)))
    (setf (dereference-mref qvm dst) (coerce (dereference-mref qvm src) 'double-float)))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-convert-real/bit))
  (let ((dst (quil:classical-left-operand instr))
        (src (quil:classical-right-operand instr)))
    (setf (dereference-mref qvm dst) (coerce (dereference-mref qvm src) 'double-float)))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-convert-bit/integer))
  (let ((dst (quil:classical-left-operand instr))
        (src (quil:classical-right-operand instr)))
    (setf (dereference-mref qvm dst) (boolean-bit (not (zerop (dereference-mref qvm src))))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-convert-bit/real))
  (let ((dst (quil:classical-left-operand instr))
        (src (quil:classical-right-operand instr)))
    (setf (dereference-mref qvm dst) (boolean-bit (not (zerop (dereference-mref qvm src))))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

;;;;;;;;;;;;;;;;;;;;;;;;; ADD, SUB, MUL, DIV ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %perform-integer-add (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref)
        (ensure-s64 (+ left right))))
(defun %perform-real-add (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref)
        (+ left right)))

(define-binary-transition quil:classical-addition-integer/integer
  %perform-integer-add (dereference-mref qvm *))
(define-binary-transition quil:classical-addition-integer/immediate
  %perform-integer-add (quil:constant-value *))
(define-binary-transition quil:classical-addition-real/real
  %perform-real-add (dereference-mref qvm *))
(define-binary-transition quil:classical-addition-real/immediate
  %perform-real-add (quil:constant-value *))

(defun %perform-integer-sub (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref)
        (ensure-s64 (- left right))))
(defun %perform-real-sub (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref)
        (- left right)))

(define-binary-transition quil:classical-subtraction-integer/integer
  %perform-integer-sub (dereference-mref qvm *))
(define-binary-transition quil:classical-subtraction-integer/immediate
  %perform-integer-sub (quil:constant-value *))
(define-binary-transition quil:classical-subtraction-real/real
  %perform-real-sub (dereference-mref qvm *))
(define-binary-transition quil:classical-subtraction-real/immediate
  %perform-real-sub (quil:constant-value *))

(defun %perform-integer-mul (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref)
        (ensure-s64 (* left right))))
(defun %perform-real-mul (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref)
        (* left right)))

(define-binary-transition quil:classical-multiplication-integer/integer
  %perform-integer-mul (dereference-mref qvm *))
(define-binary-transition quil:classical-multiplication-integer/immediate
  %perform-integer-mul (quil:constant-value *))
(define-binary-transition quil:classical-multiplication-real/real
  %perform-real-mul (dereference-mref qvm *))
(define-binary-transition quil:classical-multiplication-real/immediate
  %perform-real-mul (quil:constant-value *))

(defun %perform-integer-div (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref)
        (ensure-s64 (floor left right))))
(defun %perform-real-div (qvm target-mref left right)
  (setf (dereference-mref qvm target-mref)
        (/ left right)))

(define-binary-transition quil:classical-division-integer/integer
  %perform-integer-div (dereference-mref qvm *))
(define-binary-transition quil:classical-division-integer/immediate
  %perform-integer-div (quil:constant-value *))
(define-binary-transition quil:classical-division-real/real
  %perform-real-div (dereference-mref qvm *))
(define-binary-transition quil:classical-division-real/immediate
  %perform-real-div (quil:constant-value *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOAD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-load))
  (let* ((dst (quil:classical-target instr))
         (src-name (quil:memory-name-region-name (quil:classical-left-operand instr)))
         (src-mv (gethash src-name (classical-memories qvm)))
         (offset (dereference-mref qvm (quil:classical-right-operand instr))))
    (assert (not (null src-mv)) () "Couldn't find memory named ~S" src-name)
    (setf (dereference-mref qvm dst) (memory-view-ref src-mv offset)))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STORE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod transition ((qvm pure-state-qvm) (instr quil:classical-store))
  (let* ((dst-name (quil:memory-name-region-name (quil:classical-target instr)))
         (dst-mv (gethash dst-name (classical-memories qvm)))
         (offset (dereference-mref qvm (quil:classical-left-operand instr)))
         (src (quil:classical-right-operand instr)))
    (assert (not (null dst-mv)) () "Couldn't find memory named ~S" dst-name)
    (setf (memory-view-ref dst-mv offset) (etypecase src
                                            (quil:constant (quil:constant-value src))
                                            (quil:memory-ref (dereference-mref qvm src)))))
  (setf (pc qvm) (1+ (pc qvm)))
  qvm)

;;;;;;;;;;;;;;;;;;;;;;;;; EQ, LT, LE, GT, GE ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-indirect-comparator (comparator &rest classes)
  `(progn
     ,@(loop :for class :in classes
             :collect
             `(defmethod transition ((qvm pure-state-qvm) (instr ,class))
                (let ((dst (quil:classical-target instr))
                      (a (quil:classical-left-operand instr))
                      (b (quil:classical-right-operand instr)))
                  (setf (dereference-mref qvm dst)
                        (boolean-bit
                         (,comparator (dereference-mref qvm a)
                                      (dereference-mref qvm b))))
                  (setf (pc qvm) (1+ (pc qvm)))
		  qvm)))))

(defmacro define-direct-comparator (comparator &rest classes)
  `(progn
     ,@(loop :for class :in classes
             :collect
             `(defmethod transition ((qvm pure-state-qvm) (instr ,class))
                (let ((dst (quil:classical-target instr))
                      (a (quil:classical-left-operand instr))
                      (b (quil:classical-right-operand instr)))
                  (setf (dereference-mref qvm dst)
                        (boolean-bit
                         (,comparator (dereference-mref qvm a)
                                      (quil:constant-value b))))
                  (setf (pc qvm) (1+ (pc qvm)))
		  qvm)))))

(define-indirect-comparator =
  quil:classical-equality-bit/bit/bit
  quil:classical-equality-bit/octet/octet
  quil:classical-equality-bit/integer/integer
  quil:classical-equality-bit/real/real)

(define-direct-comparator =
  quil:classical-equality-bit/bit/immediate
  quil:classical-equality-bit/octet/immediate
  quil:classical-equality-bit/integer/immediate
  quil:classical-equality-bit/real/immediate)

(define-indirect-comparator <
  quil:classical-less-than-bit/bit/bit
  quil:classical-less-than-bit/octet/octet
  quil:classical-less-than-bit/integer/integer
  quil:classical-less-than-bit/real/real)

(define-direct-comparator <
  quil:classical-less-than-bit/bit/immediate
  quil:classical-less-than-bit/octet/immediate
  quil:classical-less-than-bit/integer/immediate
  quil:classical-less-than-bit/real/immediate)

(define-indirect-comparator <=
  quil:classical-less-equal-bit/bit/bit
  quil:classical-less-equal-bit/octet/octet
  quil:classical-less-equal-bit/integer/integer
  quil:classical-less-equal-bit/real/real)

(define-direct-comparator <=
  quil:classical-less-equal-bit/bit/immediate
  quil:classical-less-equal-bit/octet/immediate
  quil:classical-less-equal-bit/integer/immediate
  quil:classical-less-equal-bit/real/immediate)

(define-indirect-comparator >
  quil:classical-greater-than-bit/bit/bit
  quil:classical-greater-than-bit/octet/octet
  quil:classical-greater-than-bit/integer/integer
  quil:classical-greater-than-bit/real/real)

(define-direct-comparator >
  quil:classical-greater-than-bit/bit/immediate
  quil:classical-greater-than-bit/octet/immediate
  quil:classical-greater-than-bit/integer/immediate
  quil:classical-greater-than-bit/real/immediate)

(define-indirect-comparator >=
  quil:classical-greater-equal-bit/bit/bit
  quil:classical-greater-equal-bit/octet/octet
  quil:classical-greater-equal-bit/integer/integer
  quil:classical-greater-equal-bit/real/real)

(define-direct-comparator >=
  quil:classical-greater-equal-bit/bit/immediate
  quil:classical-greater-equal-bit/octet/immediate
  quil:classical-greater-equal-bit/integer/immediate
  quil:classical-greater-equal-bit/real/immediate)
