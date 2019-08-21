;;;; distributed-qvm.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(declaim (optimize (debug 3) (safety 3)))

(defclass distributed-qvm (qvm:classical-memory-mixin)
  ((addresses
    :accessor addresses
    :initarg :addresses
    :initform (error-missing-initform :addresses)
    :type addresses
    :documentation "Address table.")
   (amplitudes
    :accessor amplitudes
    :initarg :amplitudes
    :documentation "The components of the (permuted) wave function.")
   (scratch
    :accessor scratch
    :initarg :scratch
    :documentation "Temporary memory to store amplitudes."))

  (:default-initargs
   :amplitudes nil
   :scratch nil)
  (:documentation "A distributed implementation of the Quantum Abstract Machine. A DQVM object keeps track of data owned by a single MPI rank."))

(defun make-distributed-qvm (&rest rest)
  "Convenience function to instantiate DISTRIBUTED-QVM objects."
  (make-instance 'distributed-qvm :addresses (apply #'make-addresses rest)))

(defmethod initialize-instance :after ((qvm distributed-qvm) &rest initargs)
  (declare (ignore initargs))

  (reset-wavefunction qvm))

(defmethod qvm:number-of-qubits ((qvm distributed-qvm))
  (number-of-qubits (addresses qvm)))

(defmethod rank ((qvm distributed-qvm))
  (rank (addresses qvm)))

(defmethod number-of-processes ((qvm distributed-qvm))
  (number-of-processes (addresses qvm)))

(defmethod reset-wavefunction ((qvm distributed-qvm) &optional (address 0))
  "Initialize the wavefunction in QVM with a delta distribution concentrated at ADDRESS, which is the zero ket by default."
  (let* ((rank (rank qvm))
         (number-of-processes (number-of-processes qvm))
         (number-of-qubits (number-of-qubits qvm))
         (maximum-arity (get-maximum-arity (qvm::program qvm)))
         (block-size (if (plusp maximum-arity)
                         (* 2 maximum-arity)
                         *default-block-size*))
         (global-addresses (make-instance 'global-addresses :number-of-processes number-of-processes
                                                            :number-of-qubits number-of-qubits
                                                            :block-size block-size))
         (addresses (make-instance 'addresses :rank rank
                                              :global-addresses global-addresses))
         (number-of-addresses (number-of-addresses addresses)))

    ;; Initialize address table.
    (setf (addresses qvm) addresses)

    (let ((allocation (make-instance 'qvm:c-allocation :length number-of-addresses)))
      ;; Initialize wave function proper.
      (multiple-value-bind (buf fin)
          (qvm:allocate-vector allocation)

        (setf (slot-value qvm 'amplitudes) buf)
        (trivial-garbage:finalize qvm fin)

        (when (address-member address addresses)
          (setf (aref (amplitudes qvm) (offset addresses address))
                (qvm:cflonum 1))))

      ;; Initialize temporary memory.
      (multiple-value-bind (buf fin)
          (qvm:allocate-vector allocation)

        (setf (slot-value qvm 'scratch) buf)
        (trivial-garbage:finalize qvm fin))))

  qvm)

(defmethod reset-wavefunction-debug ((qvm distributed-qvm))
  (let ((amplitudes (amplitudes qvm))
        (addresses (addresses qvm)))
    (do-addresses (address addresses)
      (setf (aref amplitudes (offset addresses address))
            (qvm:cflonum address)))))

(defun load-instructions (qvm instructions)
  "Load the code vector INSTRUCTIONS into QVM."
  ;; XXX ensure the qubits needed match the current qvm qubit count.
  (setf (qvm::program qvm) instructions)
  (reset-wavefunction qvm)
  qvm)

(defmethod print-object ((qvm distributed-qvm) stream)
  (let ((*print-readably* nil)
        (*print-pretty* nil))
    (print-unreadable-object (qvm stream :type t :identity t)
      (format stream "~{~A ~A~^ ~}"
              (list (prin1-to-string :addresses) (addresses qvm)
                    (prin1-to-string :amplitudes) (amplitudes qvm)
                    (prin1-to-string :scratch) (scratch qvm))))))

(defun qubit-permutation (instruction)
  "Return an association list representing the qubits we must transpose to execute INSTRUCTION."
  (check-type instruction quil:instruction)

  (loop :with transpositions := nil
        :for i :from 0
        ;; We reverse the arguments of INSTRUCTION to account for the
        ;; ordering of the standard gates.
        :for qubit :in (reverse (quil::arguments instruction))
        :unless (= i (quil:qubit-index qubit)) :do
          (pushnew (cons i (quil:qubit-index qubit)) transpositions
                   :test (lambda (x y)
                           (or (equal x y)
                               (and (= (car x) (cdr y))
                                    (= (cdr x) (car y))))))
        :finally (return (reverse transpositions))))

(defmethod save-wavefunction ((qvm distributed-qvm) filename)
  "Save wavefunction in QVM to FILENAME in parallel.

The file format is a consecutive set of bytes containing the floating point representations of the real and imaginary parts of the ordered amplitudes."
  (format-log :debug "Saving wave function to ~S" filename)

  (uiop:delete-file-if-exists filename)

  (let ((addresses (addresses qvm))
        (amplitudes (amplitudes qvm))
        (amode (logior mpi::+mpi-mode-create+ mpi::+mpi-mode-wronly+)))

    (mpi::with-open-mpi-file (fh filename amode)
      (loop :for offset :from 0 :by qvm::+octets-per-cflonum+
            :for ptr-amplitude := (static-vector-pointer amplitudes :offset offset)
            :for index :from 0 :below (number-of-addresses addresses)
            :for address := (get-address-by-offset addresses index)
            :for file-offset := (* address qvm::+octets-per-cflonum+)
            :do (mpi::%mpi-file-write-at fh file-offset ptr-amplitude 1 +mpi-cflonum+
                                         (cffi:null-pointer))))))

;;; XXX Add functions to handle the case when a program is reloaded and block-size changes.

(defmethod load-wavefunction ((qvm distributed-qvm) filename)
  "Load wavefunction from FILENAME into QVM's AMPLITUDES slot."
  (when (zerop +rank+)
    (let ((wf (amplitudes qvm)))
      (mpi::with-open-mpi-file (fh filename mpi::+mpi-mode-rdonly+)
        (cffi:with-foreign-object (status '(:struct mpi::mpi-status))
          (mpi::%mpi-file-read fh (static-vector-pointer wf) (length wf) +mpi-cflonum+ status)
          (unless (plusp (mpi-get-count status mpi::+mpi-double-complex+))
            (error "Unable to read wavefunction from ~s." filename)))))))
