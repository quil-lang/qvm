;;;; misc.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Miscellaneous routines that don't have a home.

(defun program-matrix (pp)
  "Compute the matrix of the parsed program PP by way of simulating it on every basis element."
  (let* ((n (quil:qubits-needed pp))
         (dim (expt 2 n))
         (m (magicl:zeros (list dim dim))))
    (dotimes (basis-state dim m)
      (let ((q (make-qvm n)))
        (rotatef (aref (amplitudes q) 0)
                 (aref (amplitudes q) basis-state))
        (load-program q pp)
        (run q)
        ;; write out the amplitudes
        (dotimes (row dim)
          (setf (magicl:tref m row basis-state)
                (aref (amplitudes q) row)))))))
