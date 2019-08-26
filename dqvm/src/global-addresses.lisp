;;;; src/global-addresses.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(defparameter *default-block-size* 4
  "Default block size for address tables.")

(defclass global-addresses ()
  ((number-of-qubits
    :reader number-of-qubits
    :initarg :number-of-qubits
    :type alexandria:non-negative-fixnum
    :initform (error-missing-initform :number-of-qubits)
    :documentation "Number of qubits in the QVM.")
   (number-of-processes
    :accessor number-of-processes
    :initarg :number-of-processes
    :documentation "Number of parallel MPI ranks.")
   (block-size
    :reader block-size
    :initarg :block-size
    :type alexandria:non-negative-fixnum
    :documentation "Minimum number of amplitudes per single block matrix-vector multiplication. This must be an even number and is typically chosen to be equal to two times the arity of the instruction with the maximum arity in the program to be executed.")
   (permutation
    :reader permutation
    :writer update-permutation
    :initarg :permutation
    :type permutation
    :documentation "Last qubit permutation evaluated, stored in a format suitable for use by APPLY-QUBIT-PERMUTATION.")

   ;; The following attributes are calculated during instantiation.
   (blocks-per-process
    :reader blocks-per-process
    :type alexandria:non-negative-fixnum
    :documentation "Regular blocks per process (i.e., without including a possibly remaining block).")
   (remainder-blocks
    :reader remainder-blocks
    :type alexandria:non-negative-fixnum
    :documentation "Total number of remainder blocks.")
   (number-of-blocks
    :reader number-of-blocks
    :type alexandria:non-negative-fixnum
    :documentation "Total number of blocks among all address tables."))

  (:default-initargs
   :number-of-processes (mpi-comm-size)
   :block-size *default-block-size*
   :permutation nil)
  (:documentation "Global address table."))

(defmethod initialize-instance :after ((global-addresses global-addresses) &rest initargs)
  (declare (ignore initargs))

  (assert (not (minusp (number-of-qubits global-addresses))))
  (assert (plusp (number-of-processes global-addresses)))
  (assert (evenp (block-size global-addresses)))

  (multiple-value-bind (bpp rem) (%blocks-per-process global-addresses)
    (setf (slot-value global-addresses 'number-of-blocks) (%number-of-blocks global-addresses)
          (slot-value global-addresses 'blocks-per-process) bpp
          (slot-value global-addresses 'remainder-blocks) rem)))

(defmethod print-object ((global-addresses global-addresses) stream)
  (let ((*print-readably* nil)
        (*print-pretty* nil))

    (print-unreadable-object (global-addresses stream :type t :identity t)

      (format stream "~{~A ~A~^ ~}"
              (list (prin1-to-string :number-of-qubits) (number-of-qubits global-addresses)
                    (prin1-to-string :number-of-processes) (number-of-processes global-addresses)
                    (prin1-to-string :block-size) (block-size global-addresses)
                    (prin1-to-string :permutation) (permutation global-addresses))))))

(defmethod copy-global-addresses ((global-addresses global-addresses))
  (let ((number-of-qubits (number-of-qubits global-addresses))
        (number-of-processes (number-of-processes global-addresses))
        (block-size (block-size global-addresses))
        (permutation (permutation global-addresses)))

    (make-instance 'global-addresses :number-of-qubits number-of-qubits
                                     :number-of-processes number-of-processes
                                     :block-size block-size
                                     :permutation permutation)))

(defmethod global-addresses= ((g1 global-addresses) (g2 global-addresses))
  (and (= (number-of-qubits g1) (number-of-qubits g2))
       (= (number-of-processes g1) (number-of-processes g2))
       (= (block-size g1) (block-size g2))
       (equalp (permutation g1) (permutation g2))))

(defmethod number-of-addresses ((global-addresses global-addresses))
  (expt 2 (number-of-qubits global-addresses)))

(defun %number-of-blocks (global-addresses)
  "Return the total number of block matrices to consider in the simulation."
  (nth-value 0 (ceiling (number-of-addresses global-addresses)
                        (block-size global-addresses))))

(defmethod %blocks-per-process ((global-addresses global-addresses))
  "Return the number of regular blocks per process in GLOBAL-ADDRESSES and the number of remaining blocks.

Note that this may not be the number of blocks in the processes. To obtain that, you must check the rank and the remaining blocks."
  (floor (%number-of-blocks global-addresses)
         (number-of-processes global-addresses)))

(defmethod get-initial-address ((global-addresses global-addresses) address)
  "Apply the inverse of the latest permutation in ADDRESSES to ADDRESS."
  (apply-inverse-qubit-permutation (permutation global-addresses) address))

(defmethod get-block-by-address ((global-addresses global-addresses) address)
  "Get the (global) block index of ADDRESS."
  (assert (not (minusp address)))
  (assert (< address (number-of-addresses global-addresses)))

  (let ((initial-address (get-initial-address global-addresses address)))
    (nth-value 0 (floor initial-address (block-size global-addresses)))))

(defmethod get-rank-by-address ((global-addresses global-addresses) address)
  "Find the MPI rank of ADDRESS using ADDRESSES.

This method returns the rank where the address is located. If ADDRESS is NIL, the return value is also NIL."
  (let ((number-of-processes (number-of-processes global-addresses))
        (block-size (block-size global-addresses))
        (blocks-per-process (blocks-per-process global-addresses))
        (remainder-blocks (remainder-blocks global-addresses))
        (number-of-blocks (number-of-blocks global-addresses)))

    (assert (not (minusp address)))
    (assert (< address (number-of-addresses global-addresses)))

    (let ((initial-address (get-initial-address global-addresses address)))
      (if (< initial-address (* (- number-of-blocks remainder-blocks) block-size))
          (nth-value 0 (floor initial-address (* blocks-per-process block-size)))
          (mod (floor initial-address block-size) number-of-processes)))))

(defmethod get-rank-by-block ((global-addresses global-addresses) block-index)
  (let ((number-of-blocks (number-of-blocks global-addresses))
        (number-of-processes (number-of-processes global-addresses))
        (blocks-per-process (blocks-per-process global-addresses))
        (remainder-blocks (remainder-blocks global-addresses)))

    (assert (and (not (minusp block-index))
                 (< block-index number-of-blocks)))

    (if (< block-index (- number-of-blocks remainder-blocks))
        (nth-value 0 (floor block-index blocks-per-process))
        (- block-index (* blocks-per-process number-of-processes)))))

(defmacro do-addresses-in-block ((var global-addresses block-index
                                  &optional (result nil result-p))
                                 &body body)
  "Iterate VAR over addresses in BLOCK-INDEX. If RESULT is non-NIL, evaluate and return RESULT."
  (alexandria:once-only (global-addresses block-index)
    (alexandria:with-gensyms (block-size permutation initial-address)
      `(loop :with ,block-size := (block-size ,global-addresses)
             :with ,permutation := (permutation ,global-addresses)
             :for ,initial-address :from (* ,block-index ,block-size)
               :below (* (1+ ,block-index) ,block-size)
             :for ,var := (apply-qubit-permutation ,permutation ,initial-address)
             :when (< ,block-index (number-of-blocks ,global-addresses))
               :do (progn
                     ,@body)
             :finally (when ,result-p
                        (return ,result))))))
