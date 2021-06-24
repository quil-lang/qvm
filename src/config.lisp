;;;; src/config.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm)

;;; Configuration for QVM compile-time and run-time behavior.

(deftype parallelization-limit ()
  "A limit on the number of qubits that can be parallelized across."
  `(integer 0 50))

(declaim (type parallelization-limit *qubits-required-for-parallelization*))
(defvar *qubits-required-for-parallelization* 19
  "The number of qubits required of a quantum state before it gets operated on in parallel.

See also *QUBIT-LIMIT-FOR-USING-SERIAL-KERNELS*.")

(declaim (type parallelization-limit *qubit-limit-for-using-serial-kernels*))
(defvar *qubit-limit-for-using-serial-kernels* 29
  "The maximum number of qubits allowed in order to use a specified serial kernel.")

(defun qubit-limit-for-using-serial-kernels ()
  "The maximum number of qubits allowed in order to use a specified serial kernel. The value of this function also takes into account *QUBITS-REQUIRED-FOR-PARALLELIZATION*."
  (max *qubit-limit-for-using-serial-kernels*
       *qubits-required-for-parallelization*))

(defvar *compile-time-operator-cache-limit* 8
  "At compile time, a cache of optimized \"matrix application\" operators are computed. Further ones will be cached at runtime. This configuration parameter controls how many qubits this cache is warmed for.

This parameter should be optimized for developer convenience.")

(defvar *executable-time-operator-cache-limit* 30
  "Like *COMPILE-TIME-OPERATOR-CACHE-LIMIT*, but the amount of cache to warm before an executable is created. (There is no guarantee executable creation software will respect this suggested parameter.)

This parameter should be optimized for end-user convenience.")

(defvar *transition-verbose* nil
  "Controls whether each transition is printed with a timing.")

(defvar *compile-before-running* nil
  "Compile programs loaded into the QVM before running them.")

(defvar *fuse-gates-during-compilation* nil
  "Should gates be fused during compilation?")

(defvar *inline-static-gates-during-compilation* nil
  "Inline the actual gate matrices into the compiled instructions.")

(defvar *compile-measure-chains* nil
  "Compile chains of measures into an efficient sampling.")

(defun enable-all-qvm-optimizations ()
  (setf *compile-before-running* t
        *fuse-gates-during-compilation* t
        *inline-static-gates-during-compilation* t
        *compile-measure-chains* t)
  nil)

(defvar *optimize-dangerously-fast*
  '(optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0))
  "Optimization qualities for when the code should go as fast as possible.")

(defvar *optimize-briskly*
  '(optimize speed (safety 1) (debug 1) (space 0))
  "Optimization qualities for when the code should go fast, but have safety.")

(defvar *optimize-safely*
  '(optimize (speed 0) (safety 3) (debug 3) (space 3))
  "Optimization qualities for when the code should emphasize safety and debugability.")

;;;
;;; ""64K ought to be enough for anybody." -Bill Gates" -Michael Scott
;;;
(global-vars:define-global-parameter **classical-memory-size-limit** (* 64 1024)
  "The limit of the number of octets that can be allocated for classical memory. Default is 64KiB.")
