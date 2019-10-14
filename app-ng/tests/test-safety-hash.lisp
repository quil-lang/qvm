(in-package #:qvm-app-ng-tests)

;;; Basic API sanity tests

(deftest test-make-safety-hash ()
  ;; exhaustive test of :TEST keyword
  (dolist (test (list 'eq 'eql 'equal 'equalp #'eq #'eql #'equal #'equalp))
    (is (typep (qvm-app-ng::make-safety-hash :test test)
               'qvm-app-ng::safety-hash)))

  ;; a smattering of other standard args
  (is (typep (qvm-app-ng::make-safety-hash :size 1 :rehash-size 1 :rehash-threshold 1)
             'qvm-app-ng::safety-hash))
  (is (typep (qvm-app-ng::make-safety-hash :size 10 :rehash-size 4.5 :rehash-threshold 0.5)
             'qvm-app-ng::safety-hash)))

(deftest test-safety-hash-clrhash ()
  (let ((h (qvm-app-ng::make-safety-hash)))
    (is (= 0 (qvm-app-ng::safety-hash-table-count h)))
    (qvm-app-ng::safety-hash-clrhash h)
    (is (= 0 (qvm-app-ng::safety-hash-table-count h)))
    (dotimes (i 10)
      (setf (qvm-app-ng::safety-hash-gethash i h) 0))
    (is (= 10 (qvm-app-ng::safety-hash-table-count h)))
    (qvm-app-ng::safety-hash-clrhash h)
    (is (= 0 (qvm-app-ng::safety-hash-table-count h)))))

(deftest test-safety-hash-gethash ()
  (let ((h (qvm-app-ng::make-safety-hash)))
    (is (null (nth-value 0 (qvm-app-ng::safety-hash-gethash 0 h))))
    (is (null (nth-value 1 (qvm-app-ng::safety-hash-gethash 0 h))))
    (is (= 2  (nth-value 0 (qvm-app-ng::safety-hash-gethash 0 h 2))))
    (setf (qvm-app-ng::safety-hash-gethash 0 h) 0)
    (is (= 0  (nth-value 0 (qvm-app-ng::safety-hash-gethash 0 h))))
    (is (not (null (nth-value 1 (qvm-app-ng::safety-hash-gethash 0 h)))))
    (is (= 0  (nth-value 0 (qvm-app-ng::safety-hash-gethash 0 h 2))))
    (setf (qvm-app-ng::safety-hash-gethash 0 h) 1)
    (is (= 1  (nth-value 0 (qvm-app-ng::safety-hash-gethash 0 h))))
    (is (not (null (nth-value 1 (qvm-app-ng::safety-hash-gethash 0 h)))))
    (is (= 1  (nth-value 0 (qvm-app-ng::safety-hash-gethash 0 h 2))))))

(deftest test-safety-hash-table-count ()
  (let ((h (qvm-app-ng::make-safety-hash)))
    (dotimes (i 10)
      (is (= i (qvm-app-ng::safety-hash-table-count h)))
      (setf (qvm-app-ng::safety-hash-gethash i h) 0))
    (setf (qvm-app-ng::safety-hash-gethash 0 h) 0)
    (is (= 10 (qvm-app-ng::safety-hash-table-count h)))))

(deftest test-safety-hash-remhash ()
  (let ((h (qvm-app-ng::make-safety-hash)))
    (is (null (qvm-app-ng::safety-hash-remhash 0 h)))
    (dotimes (i 10)
      (setf (qvm-app-ng::safety-hash-gethash i h) 0))
    (dotimes (i 10)
      (is (= (- 10 i) (qvm-app-ng::safety-hash-table-count h)))
      (is (not (null (qvm-app-ng::safety-hash-remhash i h)))))
    (is (= 0 (qvm-app-ng::safety-hash-table-count h)))))

(deftest test-safety-hash-gethash-or-lose ()
  (let ((h (qvm-app-ng::make-safety-hash)))
    (signals error (qvm-app-ng::safety-hash-gethash-or-lose 0 h))
    (setf (qvm-app-ng::safety-hash-gethash 0 h) 0)
    (is (= 0 (qvm-app-ng::safety-hash-gethash-or-lose 0 h)))))

(deftest test-safety-hash-insert-unique ()
  (let ((h (qvm-app-ng::make-safety-hash)))
    (is (= 0 (setf (qvm-app-ng::safety-hash-gethash 0 h) 0)))
    (signals error (qvm-app-ng::safety-hash-insert-unique 0 0 h))))

(deftest test-call-with-locked-safety-hash ()
  (let ((h (qvm-app-ng::make-safety-hash)))
    (is (= 0 (qvm-app-ng::call-with-locked-safety-hash h (lambda (hash-table)
                                                           (setf (gethash 0 hash-table) 0)))))
    (is (= 0 (qvm-app-ng::safety-hash-gethash 0 h)))))

(deftest test-with-locked-safety-hash ()
  (let ((h (qvm-app-ng::make-safety-hash)))
    (qvm-app-ng::with-locked-safety-hash (hash-table) h
      (setf (gethash 0 hash-table) 0))
    (is (= 0 (qvm-app-ng::safety-hash-gethash 0 h)))))

;;; Concurrency tests

(deftest test-safety-hash-concurrent-writers-one-key ()
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (max 2 (lparallel:kernel-worker-count)))
        (writes-per-task 10000)
        (channel (lparallel:make-channel))
        (start-queue (lparallel.queue:make-queue))
        (h (qvm-app-ng::make-safety-hash)))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (loop :for task-id :below num-tasks :do
        (lparallel:submit-task channel
                               (lambda (task-id)
                                 (lparallel.queue:pop-queue start-queue)
                                 (loop :repeat writes-per-task :do
                                   (setf (qvm-app-ng::safety-hash-gethash 0 h) task-id)))
                               task-id))
      (loop :repeat num-tasks :do
        (lparallel.queue:push-queue t start-queue))
      (loop :repeat num-tasks :do
        (lparallel:receive-result channel))
      (is (= 1 (qvm-app-ng::safety-hash-table-count h)))
      (is (member (qvm-app-ng::safety-hash-gethash 0 h)
                  (alexandria:iota num-tasks))))))


(deftest test-safety-hash-concurrent-writers-many-unique-keys ()
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (max 2 (lparallel:kernel-worker-count)))
        (writes-per-task 10000)
        (channel (lparallel:make-channel))
        (start-queue (lparallel.queue:make-queue))
        (h (qvm-app-ng::make-safety-hash)))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (loop :for i :below num-tasks :do
        (lparallel:submit-task channel
                               (lambda (task-id)
                                 (lparallel.queue:pop-queue start-queue)
                                 (loop :for k :from task-id :below (+ task-id writes-per-task) :do
                                   (setf (qvm-app-ng::safety-hash-gethash k h) task-id)))
                               (* i writes-per-task)))
      (loop :repeat num-tasks :do
        (lparallel.queue:push-queue t start-queue))
      (loop :repeat num-tasks :do
        (lparallel:receive-result channel))
      (is (= (qvm-app-ng::safety-hash-table-count h) (* num-tasks writes-per-task))))))

(deftest test-safety-hash-concurrent-write-and-clrhash ()
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (* 2 (lparallel:kernel-worker-count)))
        (writes-per-task 10000)
        (channel (lparallel:make-channel))
        (start-queue (lparallel.queue:make-queue))
        (h (qvm-app-ng::make-safety-hash)))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (loop :with num-tasks/2 := (floor num-tasks 2)
            :for i :below num-tasks/2 :do
              (lparallel:submit-task channel
                                     (lambda (task-id)
                                       (lparallel.queue:pop-queue start-queue)
                                       (loop :with start := (* task-id writes-per-task)
                                             :for k :from start :below (+ start writes-per-task) :do
                                               (setf (qvm-app-ng::safety-hash-gethash i h) task-id)))
                                     i)
              (lparallel:submit-task channel
                                     (lambda ()
                                       (lparallel.queue:pop-queue start-queue)
                                       (loop :repeat writes-per-task :do
                                         (qvm-app-ng::safety-hash-clrhash h)))))
      (loop :repeat num-tasks :do
        (lparallel.queue:push-queue t start-queue))
      (loop :repeat num-tasks :do
        (lparallel:receive-result channel))
      (is (< (qvm-app-ng::safety-hash-table-count h) (* num-tasks writes-per-task))))))

(deftest test-safety-hash-concurrent-write-and-remhash ()
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (* 2 (lparallel:kernel-worker-count)))
        (writes-per-task 10000)
        (channel (lparallel:make-channel))
        (start-queue (lparallel.queue:make-queue))
        (keys-queue (lparallel.queue:make-queue :fixed-capacity 1000))
        (h (qvm-app-ng::make-safety-hash)))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (loop :with num-tasks/2 := (floor num-tasks 2)
            :for i :below num-tasks/2 :do
              (lparallel:submit-task channel
                                     (lambda (start)
                                       (lparallel.queue:pop-queue start-queue)
                                       (loop :for k :from start :below (+ start writes-per-task) :do
                                         (setf (qvm-app-ng::safety-hash-gethash k h) start)
                                         (lparallel.queue:push-queue k keys-queue)))
                                     (* i writes-per-task))
              (lparallel:submit-task channel
                                     (lambda ()
                                       (lparallel.queue:pop-queue start-queue)
                                       (loop :repeat writes-per-task
                                             :for k := (lparallel.queue:pop-queue keys-queue) :do
                                               (assert (qvm-app-ng::safety-hash-remhash k h))))))
      (loop :repeat num-tasks :do
        (lparallel.queue:push-queue t start-queue))
      (loop :repeat num-tasks :do
        (lparallel:receive-result channel))
      (is (= 0 (qvm-app-ng::safety-hash-table-count h))))))
