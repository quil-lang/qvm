;;;; subsystem-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-subsystem-sundries ()
  (let ((ss0 (qvm::make-subsystem-on-qubits))
        (ss1 (qvm::make-subsystem-on-qubits 0))
        (ss2 (qvm::make-subsystem-on-qubits 0 2))
        (ss3 (qvm::make-subsystem-on-qubits 0 2 4))
        (ss3- (qvm::make-subsystem-on-qubits 0 1 1 2)))
    (is (and (= 0 (qvm::subsystem-num-qubits ss0))
             (= 1 (qvm::subsystem-num-qubits ss1))
             (= 2 (qvm::subsystem-num-qubits ss2))
             (= 3 (qvm::subsystem-num-qubits ss3))
             (= 3 (qvm::subsystem-num-qubits ss3-))))
    (is (= 8 (length (qvm::subsystem-state ss3-))))
    (is (and (qvm::subsystem-contains-qubit-p ss3 0)
             (qvm::subsystem-contains-qubit-p ss3 2)
             (qvm::subsystem-contains-qubit-p ss3 4)))
    (is (not (or (qvm::subsystem-contains-qubit-p ss3 1)
                 (qvm::subsystem-contains-qubit-p ss3 3)
                 (qvm::subsystem-contains-qubit-p ss3 5))))
    (is (and (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss3))
             (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss2))
             (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss1))
             (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss0))))
    (is (not (qvm::subsystem-contains-qubits-p ss3 (qvm::subsystem-qubits ss3-))))
    (is (and (= 0 (qvm::subsystem-physical-to-logical-qubit ss3 0))
             (= 1 (qvm::subsystem-physical-to-logical-qubit ss3 2))
             (= 2 (qvm::subsystem-physical-to-logical-qubit ss3 4))))))

(deftest test-join-subsystems ()
  (let ((ss001 (qvm::make-subsystem-on-qubits 0))
        (ss010 (qvm::make-subsystem-on-qubits 1))
        (ss100 (qvm::make-subsystem-on-qubits 2)))
    (is (every #'cflonum=
               (qvm::subsystem-state (qvm::join-subsystems ss001 ss010))
               (qvm::subsystem-state (qvm::join-subsystems ss010 ss100))))
    (is (every #'cflonum=
               (qvm::subsystem-state (qvm::join-subsystems ss010 ss100))
               (qvm::subsystem-state (qvm::join-subsystems ss100 ss001))))
    (is (every #'cflonum=
               (qvm:wf 0 1 0 0)
               (qvm::subsystem-state
                (qvm::join-subsystems (qvm::make-subsystem :qubits #b01
                                                           :state (qvm:wf 0 1))
                                      (qvm::make-subsystem :qubits #b100
                                                           :state (qvm:wf 1 0))))))
    (let ((superposition (qvm:wf (/ (sqrt 2)) (/ (sqrt 2)))))
      (is (every #'cflonum=
                 (qvm:wf 0.5 0.5 0.5 0.5)
                 (qvm::subsystem-state
                  (qvm::join-subsystems (qvm::make-subsystem :qubits #b01
                                                             :state superposition)
                                        (qvm::make-subsystem :qubits #b10
                                                             :state superposition)))))
      (is (every #'cflonum=
                 (apply #'qvm:wf (make-list 16 :initial-element 0.25))
                 (qvm::subsystem-state
                  (qvm::join-subsystems
                   (qvm::join-subsystems (qvm::make-subsystem :qubits #b0001
                                                              :state superposition)
                                         (qvm::make-subsystem :qubits #b0100
                                                              :state superposition))
                   (qvm::join-subsystems (qvm::make-subsystem :qubits #b0010
                                                              :state superposition)
                                         (qvm::make-subsystem :qubits #b1000
                                                              :state superposition)))))))))

(defun phase= (a b)
  (flet ((same-or-zero-p (items)
           (loop :with model-item := nil
                 :for item :in items
                 :if (and (null model-item)
                          (not (zerop item)))
                   :do (setf model-item item)
                 :else
                   :do (unless (double-float= item model-item)
                         (return-from same-or-zero-p nil)))
           ;; If we got here, we win!
           t))
    (same-or-zero-p (map 'list (lambda (x y)
                                 (if (cflonum= 0 y)
                                     0.0d0
                                     (phase (/ x y))))
                         a b))))

(deftest test-eject-qubit-from-subsystem ()
  (let* ((ss010 (qvm::make-subsystem :qubits #b010
                                     :state (qvm:wf (/ (sqrt 2)) (/ (sqrt 2)))))
         (ss101 (qvm::make-subsystem :qubits #b101
                                     :state (qvm::randomize-wavefunction (qvm:wf 1 0 0 0))))
         (ss111 (qvm::join-subsystems ss010 ss101))
         (ss-spook (qvm::make-subsystem :qubits #b11
                                        :state (qvm:wf (/ (sqrt 2)) 0 0 (/ (sqrt 2))))))
    ;; Trivial ejections
    (multiple-value-bind (new-state ejected-state) (qvm::eject-qubit-from-subsystem ss010 31337)
      (is (eq new-state ss010))
      (is (every #'cflonum= ejected-state (qvm:wf 1 0)))
      (is (= #b010 (qvm::subsystem-qubits new-state))))
    (multiple-value-bind (new-state ejected-state) (qvm::eject-qubit-from-subsystem ss010 1)
      (is (zerop (qvm::subsystem-qubits new-state)))
      (is (every #'cflonum= ejected-state (qvm::subsystem-state ss010))))

    ;; Bad ejection
    (signals error (qvm::eject-qubit-from-subsystem ss-spook 0))

    ;; Non-trivial ejection
    (multiple-value-bind (new-state ejected-state) (qvm::eject-qubit-from-subsystem ss111 1)
      (is (phase= (qvm::subsystem-state ss101)
                  (qvm::subsystem-state new-state)))
      (is (phase= ejected-state (qvm::subsystem-state ss010))))))
