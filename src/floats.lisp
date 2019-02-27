;;;; floats.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; This file deals with the different floating point representations
;;; made use of in the QVM.
;;;
;;; These are agreed upon *everywhere* in the QVM. These are allowed
;;; to change, but only here.

(defconstant +octets-per-flonum+ 8)

(deftype flonum (&optional (min '*))
  "The float type used in computations."
  (cond
    ((numberp min) `(double-float ,(coerce min 'double-float)))
    ((eq '* min)   `double-float)
    (t (error "Malformed type: (FLONUM ~S)" min))))

(defconstant +octets-per-cflonum+ (* 2 +octets-per-flonum+))

(deftype cflonum ()
  "The complex float type used in computations. Typically these will represent wavefunction amplitudes."
  `(complex flonum))

(defun flonum (x)
  "Coerce X into a FLONUM."
  (coerce x 'flonum))

(define-compiler-macro flonum (&whole whole &environment env x)
  (if (and (constantp x env)
           (numberp x))
      (coerce x 'flonum)
      whole))

(deftype flonum-vector (&optional (length '*))
  `(simple-array flonum (,length)))

(defun cflonum (x)
  "Coerce X into a CFLONUM."
  (coerce x 'cflonum))

(define-compiler-macro cflonum (&whole whole &environment env x)
  (if (and (constantp x env)
           (numberp x))
      (coerce x 'cflonum)
      whole))

(deftype cflonum-vector (&optional (length '*))
  `(simple-array cflonum (,length)))
