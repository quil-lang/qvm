;;; some simple demos
(in-package #:qvm)

(defun execute (num-qbits qprog)
  "Execute the quantum program QPROG and compute the state probabilities."
  (flet ((probabilities (qvm)
           (map 'vector #'probability (amplitudes qvm))))
    (let ((qvm (run-program num-qbits qprog)))
      (values qvm (probabilities qvm)))))

;;; Test that we can make the state |11>.

(defun test-inversion ()
  (execute 2 '((not 0) (not 1))))

;;; Hadamard initialization of qubits.

(defparameter *hadamard-program*
  '((hadamard 0)
    (hadamard 1))
  "Two qubit Hadamard initialization.")

(defun test-hadamard-2 ()
  "Test initialization of two Hadamard states."
  (execute 2 *hadamard-program*))

(defun hadamard-initialization (num-qubits)
  "Generate a quantum program which will apply H to all "
  (loop :for i :below num-qubits
        :collect `(hadamard ,i)))

(defun test-hadamard-n ()
  "Generate a circuit that will initialize a between 2 and 6 Hadamard states. "
  (let ((n (+ 2 (random 5))))
    (format t "Hadamard initializing ~D qubits~%" n)
    (execute n (hadamard-initialization n))))

;;; Bell state creation.

(defparameter *bell-00*
  '((hadamard 0)
    (cnot 0 1))
  "Program to construct a Bell pair.")

(defun test-bell-pair ()
  "Test the construction of a Bell pair."
  (execute 2 *bell-00*))

(defun bell-state (n)
  "Construct an N-qubit Bell state."
  (cons
   '(hadamard 0)
   (loop :for i :from 1 :below n
         :collect `(cnot 0 ,i))))

(defun test-bell-quadruple ()
  "1/sqrt2 * (|0000> + |1111>)"
  (execute 4 (bell-state 4)))

(defun grid-bell-state (width height)
  "Construct an N-qubit Bell state on a qubit grid of width by height."
  (cons
   '(hadamard 0)
    (loop :for i :from 1 :below (* width height)
          :collect `(CNOT ,(if (zerop (mod i width))
                               (- i width)
                               (- i 1))
                          ,i))))

(defun test-grid-bell (width height)
  (execute (* width height) (grid-bell-state width height)))

;;; Test that we can create |01>.

(defparameter *test-swap*
  '((not 1)
    (print-probabilities "Before swap")
    (swap 0 1)
    (print-probabilities "After swap "))
  "Program to swap from |10> to |01>, printing useful intermediate information.")

(defun test-swap ()
  "Test that we can swap from |10> to |01>."
  (execute 2 *test-swap*))

;;; Test the QFT.

(defun fourier-test-program (type)
  "Generate a test program for the QFT algorithm for two qubits. The types are as follows:

    TYPE     STATE
    ----     -----
    0        |00>
    1        |01>
    2        |10>
    3        |11>
"
  (let ((initialization
          (ecase type
            ((0)                                ; |00>
             nil)
            ((1)                                ; |01>
             '((NOT 0)))
            ((2)                                ; |10>
             '((NOT 1)))
            ((3)                                ; |11>
             '((NOT 0)
               (NOT 1))))))
    `(,@initialization
      (print-amplitudes ,(format nil "Initial state |~2,'0B> " type))
      ,@(qvm-examples:qft-circuit '(0 1))
      (print-amplitudes ,(format nil "Final state QFT|~2,'0B>" type)))))

;; 1 0 0 0 => 0.5    0.5     0.5    0.5
;; 0 1 0 0 => 0.5    0.5i   -0.5   -0.5i
;; 0 0 1 0 => 0.5   -0.5     0.5   -0.5
;; 0 0 0 1 => 0.5   -0.5i   -0.5    0.5i
(defun test-fourier ()
  (dotimes (type 4)
    (run-program 2 (fourier-test-program type))))


;;; Quantum Teleportation

(defparameter *quantum-teleport*
  ;; 1 and 2 are the shared qubits
  ;;
  ;; 0 is the value to transport to 2.
  ;;
  ;; Measurement of teleported value will be in [2].
  `(;; Teleport classical bit 1
    (NOT 0)
    ;; Teleportation circuit
    (HADAMARD 1)
    (CNOT 1 2)
    (CNOT 0 1)
    (HADAMARD 0)
    (MEASURE 0 0)
    (MEASURE 1 1)
    (WHEN 1
      (PAULI-X 2))
    (WHEN 0
      (PAULI-Z 2))
    (MEASURE 2 2)))

(defun test-quantum-teleport ()
  "Test quantum teleportation. We should have the value of 1 in [2] all the time.

This function should always return T."
  (let ((qvm (execute 3 *quantum-teleport*)))
    (= 1 (classical-bit qvm 2))))
