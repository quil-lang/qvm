(in-package #:qvm-app-ng-tests)

(deftest test-concurrent-handlers-pqvm-create->info->run->delete-locking ()
  "Test that locking around persistent QVM CRUD operations is working."
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (* 2 (lparallel:kernel-worker-count)))
        (channel (lparallel:make-channel)))
    (labels ((create-qvm ()
               (let* ((response (qvm-app-ng::handle-create-qvm
                                 :allocation-method "native"
                                 :simulation-method "pure-state"
                                 :num-qubits 1))
                      (token (gethash "token" (qvm-app-ng::response-data response))))
                 (lparallel:submit-task channel #'qvm-info token)))
             (qvm-info (token)
               (qvm-app-ng::handle-qvm-info :qvm-token token)
               (lparallel:submit-task channel #'run-program token))
             (run-program (token)
               (qvm-app-ng::handle-run-program :qvm-token token
                                               :addresses +all-ro-addresses+
                                               :compiled-quil +generic-x-0-quil-program+)
               (lparallel:submit-task channel #'delete-qvm token))
             (delete-qvm (token)
               (qvm-app-ng::handle-delete-qvm :qvm-token token)))

      (lparallel:task-handler-bind ((error  #'lparallel:invoke-transfer-error))
        (loop :repeat num-tasks :do
          (lparallel:submit-task channel #'create-qvm))
        (loop :repeat (* 4 num-tasks) :do
          (lparallel:receive-result channel)))

      (is (zerop (qvm-app-ng::persistent-qvms-count))))))

(deftest test-concurrent-handlers-pqvm-create->delete-locking ()
  "Test that locking around persistent QVM creation/deletion is working."
  ;; This test is better than the create->info->run->delete pipeline, above, at weeding out locking
  ;; issues because it puts more stress on the global db lock.
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (* 10 (lparallel:kernel-worker-count)))
        (channel (lparallel:make-channel)))
    (labels ((create-qvm ()
               (let* ((response (qvm-app-ng::handle-create-qvm
                                 :allocation-method "native"
                                 :simulation-method "pure-state"
                                 :num-qubits 1))
                      (token (gethash "token" (qvm-app-ng::response-data response))))
                 (lparallel:submit-task channel #'delete-qvm token)))
             (delete-qvm (token)
               (qvm-app-ng::handle-delete-qvm :qvm-token token)))

      (lparallel:task-handler-bind ((error  #'lparallel:invoke-transfer-error))
        (loop :repeat num-tasks :do
          (lparallel:submit-task channel #'create-qvm))
        (loop :repeat (* 2 num-tasks) :do
          (lparallel:receive-result channel)))

      (is (zerop (qvm-app-ng::persistent-qvms-count))))))

(deftest test-concurrent-handlers-pqvm-run-program-locking ()
  "Test that locking around persistent QVM run-program is working."
  (qvm:prepare-for-parallelization)
  (let* ((num-tasks (* 2 (lparallel:kernel-worker-count)))
         (channel (lparallel:make-channel))
         (response (qvm-app-ng::handle-create-qvm :allocation-method "native"
                                                  :simulation-method "pure-state"
                                                  :num-qubits 1))
         (token (gethash "token" (qvm-app-ng::response-data response))))
    (unwind-protect
         (lparallel:task-handler-bind ((error  #'lparallel:invoke-transfer-error))
           (loop :repeat num-tasks :do
             (lparallel:submit-task channel
                                    (lambda ()
                                      (qvm-app-ng::handle-run-program
                                       :qvm-token token
                                       :addresses +all-ro-addresses+
                                       :compiled-quil +generic-x-0-quil-program+))))
           (loop :repeat num-tasks :do
             (lparallel:receive-result channel)))
      (qvm-app-ng::delete-persistent-qvm token))))
