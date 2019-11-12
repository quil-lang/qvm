;;;; qaoa.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-examples)

(defun qaoa (p cost-ham)
  "Produce a QAOA program for the list of Pauli (CL-QUIL.CLIFFORD:PAULI) terms COST-HAM representing a cost Hamiltonian to optimize. Return three values:

    1. The Quil program (as a PARSED-PROGRAM)

    2. A list of gamma parameters (as MEMORY-REFs) which parameterize the cost unitary.

    3. A list of beta parameters (as MEMORY-REFs) which parameterize the driver unitary.

COST-HAM should consist of commuting Pauli terms only. (This is *not* checked.)"
  (check-type p (integer 1))
  ;; Every Pauli in COST-HAM should have the same number of qubits.
  (let* ((n (cl-quil.clifford:num-qubits
             (alexandria:extremum cost-ham #'> :key #'cl-quil.clifford:num-qubits)))
         (quil (quil:parse-quil (format nil "DECLARE beta REAL[~D]; DECLARE gamma REAL[~D]" p p)))
         ;; Parameters
         (betas (loop :for i :below p :collect (quil:mref "beta" i)))
         (gammas (loop :for i :below p :collect (quil:mref "gamma" i)))
         ;; Hamiltonian
         (driver-ham (loop :for q :below n
                           :collect (cl-quil.clifford:embed cl-quil.clifford:+X+ n (list q))))
         (isns (quil:with-inst ()
                 ;; Initialize
                 (dotimes (q n)
                   (quil:inst "H" () q))
                 (loop :for beta :in betas
                       :for gamma :in gammas
                       :do
                          (let ((beta (quil::make-delayed-expression nil nil beta))
                                (gamma (quil::make-delayed-expression nil nil gamma)))
                            ;; Cost. All the terms are assumed to commute.
                            (dolist (pauli cost-ham)
                              (mapc #'quil:inst (cl-quil.clifford::exp-pauli pauli gamma)))

                            ;; Driver
                            (dolist (pauli driver-ham)
                              (mapc #'quil:inst (cl-quil.clifford::exp-pauli pauli beta))))))))
    (setf (quil:parsed-program-executable-code quil) (coerce isns 'simple-vector))
    (values quil betas gammas)))

(defun maxcut (graph)
  "Produce a cost Hamiltonian for the graph GRAPH as a list of Pauli terms.

GRAPH should be a list of edges, each represented as a pair (A B) of integer vertices."
  (loop :with n := (1+ (loop :for (from to) :in graph
                             :maximize (max from to)))
        :with ZZ := (cl-quil.clifford:pauli-from-string "ZZ")
        :for vertex :in graph
        :collect (cl-quil.clifford:embed ZZ n vertex)))

(defun complete-graph (n)
  "Build a complete graph of N vertices."
  (loop :for i :from 1 :below n
        :append (loop :for j :below i
                      :collect (list j i))))

(defun line-graph (n)
  "Build a line graph of N vertices."
  (loop :for i :below  (1- n)
        :collect (list i (1+ i))))

(defun cut-weight (graph cut)
  "Given a graph GRAPH and a cut (a list of vertices), compute the cut weight."
  (loop :for (from to) :in graph
        :sum (logxor (ldb (byte 1 from) cut)
                     (ldb (byte 1 to) cut))))

;;; Test fixtures

(defun qaoa-dump (graph file &key (shots 1000) (width 25))
  "Create a \"landscape plot\" for p=1 QAOA on the graph GRAPH. Write the output to the file FILE.

The landscape will be a plot

    cut_cost : [0, pi) x [0, pi) -> R
               gamma x beta |-> cut_cost(GRAPH)

After FILE is created, one may plot the data with gnuplot. The following commands will plot (and live-update) the data:

    set view map
    splot \"FILE\" u 1:2:3 w pm3d
    pause 2
    reread"
  (flet ((out (string)
           (write-string string)
           (terpri)
           (finish-output)))
    (out "writing program")
    (multiple-value-bind (program betas gammas) (qaoa 1 (maxcut graph))
      (let* ((n (quil:qubits-needed program))
             (beta (first betas))
             (gamma (first gammas))
             (qvm (qvm:make-qvm n)))
        (qvm:load-program qvm program :supersede-memory-subsystem t)
        (out "compiling program")
        (qvm::enable-all-qvm-optimizations)
        (qvm::compile-loaded-program qvm)
        (with-open-file (s file :direction ':output
                                :if-exists ':supersede
                                :if-does-not-exist ':create)
          (out "run")
          (let* ((start-range (qvm:flonum 0.0))
                 (end-range (qvm:flonum pi))
                 (step (/ (- end-range start-range) width)))
            (loop :with time := (get-internal-real-time)
                  :with count := 0
                  :for gamma-angle :from start-range :below end-range :by step :do
                    (loop :for beta-angle :from start-range :below end-range :by step :do
                      (setf (qvm::dereference-mref qvm beta) beta-angle
                            (qvm::dereference-mref qvm gamma) gamma-angle)
                      (qvm:run qvm)
                      (let ((samples (qvm::sample-wavefunction-multiple-times (qvm::amplitudes qvm) shots)))
                        (format s "~F ~F ~F~%" gamma-angle beta-angle
                                (loop :for bitstring :across samples
                                      :sum (cut-weight graph bitstring) :into s
                                      :finally (return (/ s shots)))))
                      (qvm::reset-quantum-state qvm)
                      (setf time (/ (- (get-internal-real-time) time) internal-time-units-per-second))
                      (format t " ~D: ~F s (est ~F s left)~%" (incf count) time (* time (- (* width width) count)))
                      (setf time (get-internal-real-time)))
                    (out " line")
                    (terpri s)
                    (finish-output s))))))))

(defun produce-qvm-for-qaoa-problem (graph)
  (multiple-value-bind (program betas gammas) (qaoa 1 (maxcut graph))
    (let* ((n (quil:qubits-needed program))
           (qvm (qvm:make-qvm n)))
      (qvm:load-program qvm program :supersede-memory-subsystem t)
      ;(qvm::enable-all-qvm-optimizations)
      ;(qvm::compile-loaded-program qvm)
      (values qvm (first betas) (first gammas)))))

(defun qaoa-serial (graph width &key (shots 1000)
                                     (start-range 0.0d0)
                                     (end-range   pi))
  (let ((results (make-array (list width width) :element-type 'qvm:flonum
                                                :initial-element (qvm:flonum -100)))
        (step (qvm:flonum (/ (- end-range start-range) width)))
        (start-time (get-internal-real-time)))
    (multiple-value-bind (qvm beta gamma) (produce-qvm-for-qaoa-problem graph)
      (let ((time (get-internal-real-time)))
        (dotimes (index (* width width))
          (multiple-value-bind (row col) (floor index width)
            (let ((gamma-angle (* col step))
                  (beta-angle (* row step)))
              (setf (qvm::dereference-mref qvm beta) beta-angle
                    (qvm::dereference-mref qvm gamma) gamma-angle)
              (qvm:run qvm)
              (let ((samples (qvm::sample-wavefunction-multiple-times (qvm::amplitudes qvm) shots)))
                (setf (aref results row col) (/ (loop :for bitstring :across samples
                                                      :sum (cut-weight graph bitstring))
                                                (qvm:flonum shots))))
              (qvm::reset-quantum-state qvm)
              (setf time (/ (- (get-internal-real-time) time) internal-time-units-per-second))
              (setf time (get-internal-real-time)))))))
    (values results (- (get-internal-real-time) start-time))))

(defun qaoa-parallel (graph width &key (shots 1000)
                                       (start-range 0.0d0)
                                       (end-range   pi))
  (let ((results (make-array (list width width) :element-type 'qvm:flonum
                                                :initial-element (qvm:flonum -100)))
        (step (qvm:flonum (/ (- end-range start-range) width)))
        (start-time (get-internal-real-time)))
    (qvm::with-parallel-subdivisions (start end (* width width)
                                            :num-divisions 12
                                            :force-parallel t)
      (multiple-value-bind (qvm beta gamma) (produce-qvm-for-qaoa-problem graph)
        (loop :for index :from start :below end :do
          (multiple-value-bind (row col) (floor index width)
            (let ((gamma-angle (* col step))
                  (beta-angle (* row step)))
              (setf (qvm::dereference-mref qvm beta) beta-angle
                    (qvm::dereference-mref qvm gamma) gamma-angle)
              (qvm:run qvm)
              (let ((samples (qvm::sample-wavefunction-multiple-times (qvm::amplitudes qvm) shots)))
                (setf (aref results row col) (/ (loop :for bitstring :across samples
                                                      :sum (cut-weight graph bitstring))
                                                (qvm:flonum shots))))
              ;;(qvm::reset-quantum-state qvm)
              (let ((amps (qvm::amplitudes qvm)))
                (declare (optimize speed (safety 0) (debug 0) (space 0))
                         (type qvm::quantum-state amps))
                (fill amps (qvm:cflonum 0))))))))
    (values results (- (get-internal-real-time) start-time))))

(defun compare-serial-vs-parallel ()
  (qvm::prepare-for-parallelization)
  (sb-ext:gc :full t)
  (loop :with width := 25
        :for n :from 2 :to 26
        :for time-serial := (nth-value 1 (qaoa-serial (line-graph n) width))
        :for time-parallel := (nth-value 1 (qaoa-parallel (line-graph n) width))
        :do (format t "~2Dq: ~10,3F ~20T| ~10,3F ~10T[~Dx]~%" n
                    (/ time-serial internal-time-units-per-second)
                    (/ time-parallel internal-time-units-per-second)
                    (round time-serial time-parallel))
            (sb-ext:gc :full t)
        :collect (cons time-serial time-parallel)))
