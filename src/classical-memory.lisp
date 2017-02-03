;;;; classical-memory.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

(deftype classical-memory ()
  "The type representing classical memory."
  'unsigned-byte)

(defun make-classical-memory (n)
  "Make a classical memory of N bits."
  (declare (ignore n))
  0)

(defun peek-bit (mem n)
  "Get the Nth bit of the classical memory MEM."
  (ldb (byte 1 n) mem))

(defun poke-bit (mem n new-bit)
  "Deposit the bit NEW-BIT at the Nth element of classical memory MEM."
  (check-type new-bit bit)
  (dpb new-bit (byte 1 n) mem))

(deftype bit-range ()
  "Representation of a range of bits."
  `(cons (integer 0) (integer 1)))

(defun make-bit-range (a b)
  "Construct a new bit range whose endpoints are A and B."
  (check-type a (integer 0))
  (check-type b (integer 1))
  (assert (< a b))
  (cons a b))

(defun bit-range-left (br)
  "Get the left endpoint of the bit range BR."
  (car br))

(defun bit-range-right (br)
  "Get the right endpoint of the bit range BR."
  (cdr br))

(defun bit-range-width (br)
  "Compute the width of the bit range BR."
  (1+ (- (bit-range-right br) (bit-range-left br))))

(defun peek-bits (mem br)
  "Extract the bits (as an UNSIGNED-BYTE) from the classical memory MEM designated by the bit range BR."
  (ldb (byte (bit-range-width br) (bit-range-left br))
       mem))

(defun poke-bits (mem br new-bits)
  "Deposit the bits NEW-BITS represented as an unsigned-byte into the classical memory MEM within the bit range BR."
  (let ((range-width (bit-range-width br))
        (len (integer-length new-bits)))
    (assert (<= len range-width) (new-bits)
            "The value being set in the bit range [~D-~D] exceeds the width of the range."
            (bit-range-left br)
            (bit-range-right br))
    (dpb new-bits
         (byte range-width (bit-range-left br))
         mem)))
