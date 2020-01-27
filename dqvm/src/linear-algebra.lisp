;;;; src/linear-algebra.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:dqvm2)

(defconstant +foreign-flonum+ (ecase qvm::+octets-per-flonum+
                                (4 :float)
                                (8 :double))
  "Foreign type corresponding to a FLONUM.")

(defconstant +gemm+ (ecase qvm::+octets-per-flonum+
                      (4 #'magicl.blas-cffi::%%cgemm)
                      (8 #'magicl.blas-cffi::%%zgemm))
  "Generalized matrix-matrix multiplication routine.")

(defun compute-matrix-vector-products (matrix input-array output-array start-offset end-offset)
  (magicl.cffi-types:with-array-pointers ((ptr-a (magicl::matrix-data matrix)))
    (let* ((m (ncols matrix))
           (n (/ (- end-offset start-offset) m))
           (pos (* start-offset qvm::+octets-per-cflonum+))
           (ptr-x (static-vector-pointer input-array :offset pos))
           (ptr-y (static-vector-pointer output-array :offset pos)))

      (cffi:with-foreign-objects ((ptr-m :int32)
                                  (ptr-n :int32)
                                  (inc :int32)
                                  (alpha +foreign-flonum+ 2)
                                  (beta +foreign-flonum+ 2))
        (setf (cffi:mem-ref ptr-m :int32) m
              (cffi:mem-ref ptr-n :int32) n
              (cffi:mem-ref inc :int32) 1
              (cffi:mem-aref alpha +foreign-flonum+ 0) (qvm:flonum 1)
              (cffi:mem-aref alpha +foreign-flonum+ 1) (qvm:flonum 0)
              (cffi:mem-aref beta +foreign-flonum+ 0) (qvm:flonum 0)
              (cffi:mem-aref beta +foreign-flonum+ 1) (qvm:flonum 0))

        (funcall +gemm+ "N" "N" ptr-m ptr-n ptr-m alpha ptr-a ptr-m ptr-x ptr-m beta ptr-y ptr-m)))))
