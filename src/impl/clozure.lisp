;;;; clozure.lisp

(in-package #:qvm)

(defconstant +ivector-prefix-size+ 25
  "Taken from ccl::%make-heap-ivector")

(defun shm-vector-header-size ()
  #+32-bit-target (error "32-bit target not supported")
  ;; This is the point at which vector data appears for complex
  ;; double-floats - a combination of the +ivector-prefix-size+ and a
  ;; pad word that applies only to that element-type.
  32)

(defun shm-vector-allocation-size (num-elements element-type)
  "Return the size, in octets, needed to store a simple-array of
NUM-ELEMENTS ELEMENT-TYPE objects."
  (let* ((subtag (ccl::element-type-subtype element-type))
         (data-size (ccl::subtag-bytes subtag num-elements)))
    (+ +ivector-prefix-size+ data-size)))

(defun free-shm-vector (v)
  (when (ccl::%heap-ivector-p v)
    (ccl:with-lock-grabbed (ccl::*heap-ivector-lock*)
      (setq ccl::*heap-ivectors* (ccl::delq v ccl::*heap-ivectors*)))))

(defun shm-vector-cleanup-thunk (v)
  (lambda ()
    (free-shm-vector v)))

(defun shm-vector-from-pointer (pointer num-elements element-type)
  (let ((subtype (ccl::element-type-subtype element-type)))
    (ccl:with-macptrs ((ptr pointer))
      (let ((vect (ccl::fudge-heap-pointer ptr subtype num-elements))
            (p (ccl:%null-ptr)))
        (ccl::%vect-data-to-macptr vect p)
        (ccl::with-lock-grabbed (ccl::*heap-ivector-lock*)
          (push vect ccl::*heap-ivectors*))
        (values vect
                (shm-vector-cleanup-thunk vect))))))


(defun call-at-exit (fun)
  (push fun ccl:*lisp-cleanup-functions*))
