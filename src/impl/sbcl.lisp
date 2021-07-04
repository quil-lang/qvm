;;;; sbcl.lisp

(in-package #:qvm)

(defun call-at-exit (fun)
  "Ensure that FUN is called when the Lisp implementation stops."
  (pushnew fun sb-ext:*exit-hooks* :test 'eq))

(defun shm-vector-header-size ()
  (* sb-vm:vector-data-offset sb-vm:n-word-bytes))

;; +array-header-size+ and %allocation-size were originally in
;; static-vectors but commit c85d2826955c0a8b7ffce87b270687cb8d3ed254
;; removed them. Depending on not-exported functions is bad for this
;; reason.
(defconstant +array-header-size+
  (* sb-vm:vector-data-offset sb-vm:n-word-bytes))

(defun %allocation-size (length widetag n-bits)
  (flet ((string-widetag-p (widetag)
           (or (= widetag sb-vm:simple-base-string-widetag)
               #+sb-unicode
               (= widetag sb-vm:simple-character-string-widetag))))
    (+ (* 2 sb-vm:n-word-bytes
          (ceiling
           (* (if (string-widetag-p widetag)
                  (1+ length)           ; for the final #\Null
                  length)
              n-bits)
           (* 2 sb-vm:n-word-bits)))
       (shm-vector-header-size))))

(defun shm-vector-allocation-size (num-elements element-type)
  "Return the size, in octets, needed to store a simple-array of
NUM-ELEMENTS ELEMENT-TYPE objects."
  (multiple-value-bind (widetag n-bits)
      (static-vectors::vector-widetag-and-n-bits element-type)
    (%allocation-size num-elements widetag n-bits)))

(defun shm-vector-from-pointer (pointer num-elements element-type)
  "Return two values: a specialized vector of NUM-ELEMENTS for
ELEMENT-TYPE created at POINTER, and a finalizer that is called to
clean up that vector."
  (multiple-value-bind (widetag n-bits)
      (static-vectors::vector-widetag-and-n-bits element-type)
    (declare (ignore n-bits))
    (setf (sb-sys:sap-ref-word pointer 0) widetag
          (sb-sys:sap-ref-word pointer sb-vm:n-word-bytes) (sb-vm:fixnumize num-elements))
    (values
     (sb-kernel:%make-lisp-obj (logior (cffi:pointer-address pointer)
                                       sb-vm:other-pointer-lowtag))
     ;; No finalizer is needed for the vector itself in SBCL.)
     (lambda () nil))))
