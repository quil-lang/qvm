;;;; sbcl.lisp

(in-package #:qvm)

(defun call-at-exit (fun)
  "Ensure that FUN is called when the Lisp implementation stops."
  (pushnew fun sb-ext:*exit-hooks* :test 'eq))

(defun shm-vector-header-size ()
  (* sb-vm:vector-data-offset sb-vm:n-word-bytes))

(defun shm-vector-allocation-size (num-elements element-type)
  "Return the size, in octets, needed to store a simple-array of
NUM-ELEMENTS ELEMENT-TYPE objects."
  (multiple-value-bind (widetag n-bits)
      (static-vectors::vector-widetag-and-n-bits element-type)
    (static-vectors::%allocation-size num-elements widetag n-bits)))

(defun shm-vector-from-pointer (pointer num-elements element-type)
  "Return two values: a specialized vector of NUM-ELEMENTS for
ELEMENT-TYPE created at POINTER, and a finalizer that is called to
clean up that vector."
  (multiple-value-bind (widetag n-bits)
      (static-vectors::vector-widetag-and-n-bits element-type)
    (declare (ignore n-bits))
    (values (static-vectors::vector-from-pointer pointer widetag num-elements)
            ;; No finalizer is needed for the vector itself in SBCL.
            (lambda () nil))))

