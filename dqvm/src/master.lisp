;;;; master.lisp

(in-package #:dqvm.master)

;;; This file deals with everything the master node needs to do. This
;;; is usually the coordination of the computation and the management
;;; of the cluster.

(defun initial-cluster (qubit-count)
  "Create an initial cluster suitable for QUBIT-COUNT."
  (make-cluster
   :qubit-count qubit-count
   :ordering nil
   :current-instruction nil
   :operating-qubits nil))

(defun broadcast-cluster (cluster)
  ;; We don't use BCAST because the receivers don't know the length.
  (check-type cluster cluster)
  (loop :with serialized-cluster := (serialize-cluster cluster)
        :for r :from 1 :to +worker-count+ :do
          (mpi:mpi-send serialized-cluster r)))

(defun qubit-indices (instruction)
  "Return list of qubit indices on which INSTRUCTION operates."
  (mapcar #'quil:qubit-index (quil:application-arguments instruction)))

(defun set-cluster-instruction (cluster instruction)
  "Update the current instruction and current qubits that the cluster is to act on."
  (check-type cluster cluster)
  (check-type instruction quil:gate-application)
  (setf (current-instruction cluster) (instruction->string instruction))
  (setf (operating-qubits cluster) (qubit-indices instruction)))

(defun set-ordering-from-instruction (cluster instruction)
  "Set the current ordering of the amplitudes of the wavefunction.

Warning! This doesn't actually change the ordering. This is for bookkeeping purposes only, and if not used carefully, can cause the recorded ordering to be out of sync with the actual wavefunction ordering."
  (check-type cluster cluster)
  (check-type instruction quil:gate-application)
  (setf (ordering cluster) (qubit-indices instruction)))


(defun %main-master (&key program)
  (assert (master-node-p))
  (check-type program quil:parsed-program)

  (let ((qubit-count (cl-quil:qubits-needed program)))
    ;; set up cluster state
    (setf **cluster** (initial-cluster qubit-count))

    ;; sync with the team
    (format-locked "Synchronizing **cluster**~%")
    (broadcast-cluster **cluster**)

    ;; wait for the workers to initialize
    (everybody-synchronize "Waiting for workers to initialize")

    ;; Enter processing loop.
    (loop :for instruction :across (quil:parsed-program-executable-code program) :do
      (set-cluster-instruction **cluster** instruction)
      (everybody-synchronize "Waiting to broadcast operating qubits update")
      (broadcast-cluster **cluster**)
      ;; wait for everybody to finish executing their instruction
      (everybody-synchronize "gratuitous send/recv barrier")
      (everybody-synchronize "Waiting for everybody to finish executing an instruction")
      (set-ordering-from-instruction **cluster** instruction)
      (broadcast-cluster **cluster**))
    (everybody-synchronize "Waiting for everybody to finish execution [x]")
    ;; We are all done. Reverse it all.
    (setf (current-instruction **cluster**) nil)
    (setf (operating-qubits **cluster**) nil)
    (broadcast-cluster **cluster**)
    ;; wait for amplitudes to be reorganized
    (everybody-synchronize "f t p")
    (everybody-synchronize "Waiting for everybody to reorganize for final result")
    (format-locked "Finished ~D qubit program~%" (qubit-count **cluster**))))
