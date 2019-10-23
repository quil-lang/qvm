;;;; serial-kernels.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file provides functionality for "serial kernels". These are
;;; gate application functions which do *not* operate in parallel and
;;; which are *not* generic in qubit count. These can be faster than
;;; the general methods of the QVM for low numbers of qubits
;;; (approx. less than 30).
;;;
;;; Note that "serial kernels" are not at all necessary in the
;;; QVM. The more general function APPLY-OPERATOR is capable of
;;; working with any number of qubits without the need of special
;;; macrology. In other words, this file could be entirely deleted and
;;; things will still work.

(defmacro define-serial-kernel (name num-qubits)
  "Define a function named NAME which has the lambda list

    (OP WF Q0 ... Qn-1)

which applies the gate OP (represented as a QUANTUM-OPERATOR) to the wavefunction WF (represented as a QUANTUM-STATE) with qubits Q0 ... Qn-1, where N = NUM-QUBITS.

This function mutates WF and is unsafe.
"
  (check-type name symbol)
  (check-type num-qubits (integer 1))
  (let ((qubits (loop :for i :below num-qubits
                      :collect (alexandria:format-symbol nil "Q~D" i)))
        (qubit-bits (loop :for i :below num-qubits
                          :collect (alexandria:format-symbol nil "B~D" i)))
        (limits (loop :for i :below num-qubits
                      :collect (alexandria:format-symbol nil "L~D" i)))
        (strides (loop :for i :from 0 :below num-qubits
                       :collect (alexandria:format-symbol nil "STRIDE~D" i)))
        (loop-vars (loop :for i :from 0 :to num-qubits
                         :collect (alexandria:format-symbol nil "K~D" i)))
        (amplitude-indexes (loop :for i :below (expt 2 num-qubits)
                                 :collect (alexandria:format-symbol nil "I~v,'0B" num-qubits i)))
        (amplitudes (loop :for i :below (expt 2 num-qubits)
                          :collect (alexandria:format-symbol nil "A~v,'0B" num-qubits i))))
    `(defun ,name (op wf ,@qubits)
       ,(format nil "Apply a ~D-qubit~:P operator OP to the wavefunction WF on qubits ~{~A~^, ~}.

Note: This function is raw and unsafe. Pass bad data => get a crash."
                num-qubits
                qubits)
       (declare (type quantum-state wf)
                (type quantum-operator op)
                (type nat-tuple-element ,@qubits)
                ,*optimize-dangerously-fast*)
       (flet ((kernel-function (wf op ,@amplitude-indexes)
                (declare (type quantum-state wf)
                         (type alexandria:array-index ,@amplitude-indexes))
                (let ,(loop :for amp :in amplitudes
                            :for amp-idx :in amplitude-indexes
                            :collect `(,amp (aref wf ,amp-idx)))
                  (setf
                   ,@(loop :for row :from 0
                           :for amp-idx :in amplitude-indexes
                           :collect `(aref wf ,amp-idx)
                           :collect `(+ ,@(loop :for col :from 0
                                                :for amp :in amplitudes
                                                :collect `(* ,amp (aref op ,row ,col))))))
                  wf)))
         (declare (inline kernel-function))
         (let ((n (length wf))
               (sorted-qubits (make-array ,num-qubits :element-type 'nat-tuple-element
                                                      :initial-element 0)))
           (declare (type alexandria:array-length n)
                    (type (simple-array nat-tuple-element (,num-qubits)) sorted-qubits)
                    (dynamic-extent sorted-qubits))
           ;; We have to sort the incoming qubits so that we can
           ;; iterate through the amplitudes correctly. More
           ;; specifically, we need to know the start and end
           ;; positions in the bitstring to iterate through, and the
           ;; start-end pairs are determined by the qubits in sorted
           ;; order.
           ;;
           ;;        00c00000b00a000
           ;;
           ;; a, b, c are qubits and determine the substrings of
           ;; zeros we need to increment. But critically, to
           ;; determine the loop bounds, we need to know the pairs
           ;; (a, b) and (b, c), which requires sorting.
           ;;
           ;; We write a couple specialized sorting routines inline
           ;; here.
           ,@(case num-qubits
               ;; Nothing to sort.
               ((1)
                (list
                 `(setf (aref sorted-qubits 0) ,(first qubits))))
               ;; Trivial sort.
               ((2)
                (list
                 (let ((q0 (first qubits))
                       (q1 (second qubits)))
                   `(cond
                      ((< ,q0 ,q1)
                       (setf (aref sorted-qubits 0) ,q0
                             (aref sorted-qubits 1) ,q1))
                      (t
                       (setf (aref sorted-qubits 0) ,q1
                             (aref sorted-qubits 1) ,q0))))))
               ;; General sort.
               (otherwise
                (list
                 `(setf ,@(loop :for i :below num-qubits
                                :for qubit :in qubits
                                :collect `(aref sorted-qubits ,i)
                                :collect qubit))
                 `(setf sorted-qubits (sort sorted-qubits #'<)))))
           ;; Now we actually collect the bounds, and precompute the
           ;; qubit bits.
           (let* (,@(loop :for i :from 0
                          :for limit :in limits
                          :for qubit :in qubits
                          :for qubit-bit :in qubit-bits
                          :for stride :in strides
                          :collect `(,qubit-bit (ash 1 ,qubit))
                          :collect `(,limit (ash 1 (aref sorted-qubits ,i)))
                          :collect `(,stride (ash ,limit 1))))
             (declare (type alexandria:array-index ,@qubit-bits ,@limits ,@strides))
             (uiop:nest
              ,@(reverse
                 (loop :for loop-var :in loop-vars
                       ;; We have N because the outermost loop is
                       ;; bound by the total size of the
                       ;; wavefunction, which is a power-of-two
                       ;; length.
                       :for limit :in (append limits '(n))
                       ;; The outermost loop also starts at 0.
                       :for from :in (append (rest loop-vars) '(0))
                       ;; The innermost loop (i.e., that which is
                       ;; incrementing the LSBs) goes by 1.
                       :for stride :in (append '(1) strides)
                       :for to := `(+ ,from ,limit)
                       :collect `(loop :for ,loop-var
                                       :from ,from
                                         :below ,to
                                       ;; This needs to be EQL because STRIDE
                                       ;; is (OR UNSIGNED-BYTE SYMBOL).
                                       ,@(if (eql 1 stride)
                                             nil
                                             `(:by ,stride))
                                       :do)))
              (kernel-function
               wf op ,@(loop :for i :below (expt 2 num-qubits)
                             ;; We are collecting all of the
                             ;; amplitude addresses here. The
                             ;; first of LOOP-VARS is going to be
                             ;; the "clean" index on which bits
                             ;; are tagged on. As Erik Davis says,
                             ;; we could also have all loops
                             ;; increment from 0 (thus
                             ;; incrementing *just* the
                             ;; bit-substring), but that would
                             ;; require NUM-QUBIT 'AND'
                             ;; instructions of all of the loop
                             ;; variables here.
                             :collect `(+ ,(first loop-vars)
                                          ,@(loop :for bit :below num-qubits
                                                  :for qubit-bit :in qubit-bits
                                                  :when (logbitp bit i)
                                                    :collect qubit-bit)))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; One Qubit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-(and qvm-intrinsics avx2)
(define-serial-kernel apply-1q-operator 1)

#+(and qvm-intrinsics avx2)
(defun apply-1q-operator (operator wavefunction q)
  "Apply the matrix operator OPERATOR to the amplitudes of WAVEFUNCTION specified by the qubit Q."
  (declare (inline qvm-intrinsics::2x2matrix-to-simd
                   qvm-intrinsics::matmul2-simd)
           (type quantum-state wavefunction)
           (type quantum-operator operator)
           (type nat-tuple-element q)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let ((n/2 (half (length wavefunction))))
    ;; Load the matrix into registers.
    (with-parallel-subdivisions (start end n/2)
      (multiple-value-bind (vyr vyi xzr xzi)
          (qvm-intrinsics::2x2matrix-to-simd (aref operator 0 0)
                                             (aref operator 0 1)
                                             (aref operator 1 0)
                                             (aref operator 1 1))
        (loop :for i :from start :below end :do
          (let* ((ai (inject-bit i q))
                 (bi (dpb 1 (byte 1 q) ai)))
            (multiple-value-bind (p q)
                (qvm-intrinsics::matmul2-simd vyr vyi xzr xzi
                                              (aref wavefunction ai)
                                              (aref wavefunction bi))
              (setf (aref wavefunction ai) p
                    (aref wavefunction bi) q))))
        wavefunction))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Two Qubits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-(and qvm-intrinsics avx2)
(define-serial-kernel apply-2q-operator 2)

#+(and qvm-intrinsics avx2)
(defun apply-2q-operator (operator wavefunction q0 q1)
  "Apply the matrix operator OPERATOR to the amplitudes of WAVEFUNCTION specified by the qubits Q0 and Q1."
  (declare (inline qvm-intrinsics::2x4matrix-to-simd
                   qvm-intrinsics::matmul4-simd-half)
           (type quantum-state wavefunction)
           (type quantum-operator operator)
           (type nat-tuple-element q0)
           (type nat-tuple-element q1)
           (optimize speed (safety 0) (debug 0) (space 0)))
  (let* ((n/4 (floor (length wavefunction) 4))
         (p0  (min q0 q1))
         (p1  (max q0 q1))
         (q0-mask (ash 1 q0))
         (q1-mask (ash 1 q1))
         (matrix-storage (sb-ext:array-storage-vector operator)))
    (macrolet ((aref-matrix (i j)
                 (let ((index (+ j (* 4 i))))
                   `(aref matrix-storage ,index))))
      (with-parallel-subdivisions (start end n/4)
        (loop :for i :from start :below end :do
          (let* ((ai (inject-bit (inject-bit i p0) p1))
                 (bi (logior ai q0-mask))
                 (ci (logior ai q1-mask))
                 (di (logior bi ci)))
            (let ((a0 (aref wavefunction ai))
                  (a1 (aref wavefunction bi))
                  (a2 (aref wavefunction ci))
                  (a3 (aref wavefunction di)))
              (let ((m00 (aref-matrix 0 0))
                    (m01 (aref-matrix 0 1))
                    (m02 (aref-matrix 0 2))
                    (m03 (aref-matrix 0 3))
                    (m10 (aref-matrix 1 0))
                    (m11 (aref-matrix 1 1))
                    (m12 (aref-matrix 1 2))
                    (m13 (aref-matrix 1 3)))
                (multiple-value-bind (m0r m0i m1r m1i m2r m2i m3r m3i)
                    (qvm-intrinsics::2x4matrix-to-simd m00 m01 m02 m03 m10 m11 m12 m13)
                  (multiple-value-bind (r0 r1)
                      (qvm-intrinsics::matmul4-simd-half m0r m0i m1r m1i m2r m2i m3r m3i a0 a1 a2 a3)
                    (setf (aref wavefunction ai) r0
                          (aref wavefunction bi) r1))))
              (let ((m20 (aref-matrix 2 0))
                    (m21 (aref-matrix 2 1))
                    (m22 (aref-matrix 2 2))
                    (m23 (aref-matrix 2 3))
                    (m30 (aref-matrix 3 0))
                    (m31 (aref-matrix 3 1))
                    (m32 (aref-matrix 3 2))
                    (m33 (aref-matrix 3 3)))
                (multiple-value-bind (m4r m4i m5r m5i m6r m6i m7r m7i)
                    (qvm-intrinsics::2x4matrix-to-simd m20 m21 m22 m23 m30 m31 m32 m33)
                  (multiple-value-bind (r2 r3)
                      (qvm-intrinsics::matmul4-simd-half m4r m4i m5r m5i m6r m6i m7r m7i a0 a1 a2 a3)
                    (setf (aref wavefunction ci) r2
                          (aref wavefunction di) r3)))))))
        wavefunction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; More Qubits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-serial-kernel apply-3q-operator 3)


;;;;;;;;;;;;;;;;;;;;;;;;; Available Kernels ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The use of these kernels is dictated by
;;; QUBIT-LIMIT-FOR-USING-SERIAL-KERNELS.

(defvar *available-kernels*
  (list (cons 1 'apply-1q-operator)
        (cons 2 'apply-2q-operator)
        (cons 3 'apply-3q-operator))
  "An association list of kernels. The keys are integers representing number of qubits, and values are symbols designating function names corresponding to an operator whose lambda list is that described in DEFINE-SERIAL-KERNEL.")

(defun find-serial-kernel (n)
  "Find a serial gate application kernel for an N-qubit operator."
  (check-type n (integer 1))
  (cdr (assoc n *available-kernels* :test #'=)))
