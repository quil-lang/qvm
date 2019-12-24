(in-package #:qvm-app-ng-tests)

;;; Basic API sanity tests

(deftest test-safety-hash-make-safety-hash ()
  ;; exhaustive test of :TEST keyword
  (dolist (test (list 'eq 'eql 'equal 'equalp #'eq #'eql #'equal #'equalp))
    (is (safety-hash:safety-hash-p (safety-hash:make-safety-hash :test test))))

  ;; a smattering of other standard args
  (is (safety-hash:safety-hash-p
       (safety-hash:make-safety-hash :size 1 :rehash-size 1 :rehash-threshold 1)))
  (is (safety-hash:safety-hash-p
       (safety-hash:make-safety-hash :size 10 :rehash-size 4.5 :rehash-threshold 0.5))))

(deftest test-safety-hash-clrhash ()
  (let ((h (safety-hash:make-safety-hash)))
    (is (= 0 (safety-hash:hash-table-count h)))
    (safety-hash:clrhash h)
    (is (= 0 (safety-hash:hash-table-count h)))
    (dotimes (i 10)
      (setf (safety-hash:gethash i h) 0))
    (is (= 10 (safety-hash:hash-table-count h)))
    (safety-hash:clrhash h)
    (is (= 0 (safety-hash:hash-table-count h)))))

(deftest test-safety-hash-gethash ()
  (let ((h (safety-hash:make-safety-hash)))
    (is (null (nth-value 0 (safety-hash:gethash 0 h))))
    (is (null (nth-value 1 (safety-hash:gethash 0 h))))
    (is (= 2  (nth-value 0 (safety-hash:gethash 0 h 2))))
    (setf (safety-hash:gethash 0 h) 0)
    (is (= 0  (nth-value 0 (safety-hash:gethash 0 h))))
    (is (not (null (nth-value 1 (safety-hash:gethash 0 h)))))
    (is (= 0  (nth-value 0 (safety-hash:gethash 0 h 2))))
    (setf (safety-hash:gethash 0 h) 1)
    (is (= 1  (nth-value 0 (safety-hash:gethash 0 h))))
    (is (not (null (nth-value 1 (safety-hash:gethash 0 h)))))
    (is (= 1  (nth-value 0 (safety-hash:gethash 0 h 2))))))

(deftest test-safety-hash-hash-table-count ()
  (let ((h (safety-hash:make-safety-hash)))
    (dotimes (i 10)
      (is (= i (safety-hash:hash-table-count h)))
      (setf (safety-hash:gethash i h) 0))
    (setf (safety-hash:gethash 0 h) 0)
    (is (= 10 (safety-hash:hash-table-count h)))))

(deftest test-safety-hash-remhash ()
  (let ((h (safety-hash:make-safety-hash)))
    (is (null (safety-hash:remhash 0 h)))
    (dotimes (i 10)
      (setf (safety-hash:gethash i h) 0))
    (dotimes (i 10)
      (is (= (- 10 i) (safety-hash:hash-table-count h)))
      (is (not (null (safety-hash:remhash i h)))))
    (is (= 0 (safety-hash:hash-table-count h)))))

(deftest test-safety-hash-gethash-or-lose ()
  (let ((h (safety-hash:make-safety-hash)))
    (signals error (safety-hash:gethash-or-lose 0 h))
    (setf (safety-hash:gethash 0 h) 0)
    (is (= 0 (safety-hash:gethash-or-lose 0 h)))))

(deftest test-safety-hash-insert-unique ()
  (let ((h (safety-hash:make-safety-hash)))
    (is (= 0 (setf (safety-hash:gethash 0 h) 0)))
    (signals error (safety-hash:insert-unique 0 0 h))))

(deftest test-safety-hash-call-with-locked-safety-hash ()
  (let ((h (safety-hash:make-safety-hash)))
    (is (= 0 (safety-hash:call-with-locked-safety-hash h (lambda (hash-table)
                                                           (setf (gethash 0 hash-table) 0)))))
    (is (= 0 (safety-hash:gethash 0 h)))))

(deftest test-safety-hash-with-locked-safety-hash ()
  (let ((h (safety-hash:make-safety-hash)))
    (safety-hash:with-locked-safety-hash (hash-table) h
      (setf (gethash 0 hash-table) 0))
    (is (= 0 (safety-hash:gethash 0 h)))))

(deftest test-safety-hash-recursive-locking ()
  (let ((h (safety-hash:make-safety-hash)))
    (safety-hash:with-locked-safety-hash (hash-table-1) h
      (setf (gethash 0 hash-table-1) 0)
      (safety-hash:with-locked-safety-hash (hash-table-2) h
        (is (eq hash-table-1 hash-table-2))
        (setf (gethash 1 hash-table-1) 1)
        (setf (gethash 2 hash-table-2) 2)))
    (is (= 0 (safety-hash:gethash 0 h)))
    (is (= 1 (safety-hash:gethash 1 h)))
    (is (= 2 (safety-hash:gethash 2 h)))))

;;; Concurrency tests

(deftest test-safety-hash-concurrent-writers-one-key ()
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (max 2 (lparallel:kernel-worker-count)))
        (writes-per-task 10000)
        (channel (lparallel:make-channel))
        (start-queue (lparallel.queue:make-queue))
        (h (safety-hash:make-safety-hash)))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (loop :for task-id :below num-tasks :do
        (lparallel:submit-task channel
                               (lambda (task-id)
                                 (lparallel.queue:pop-queue start-queue)
                                 (loop :repeat writes-per-task :do
                                   (setf (safety-hash:gethash 0 h) task-id)))
                               task-id))
      (loop :repeat num-tasks :do
        (lparallel.queue:push-queue t start-queue))
      (loop :repeat num-tasks :do
        (lparallel:receive-result channel))
      (is (= 1 (safety-hash:hash-table-count h)))
      (is (member (safety-hash:gethash 0 h)
                  (alexandria:iota num-tasks))))))

(deftest test-safety-hash-concurrent-writers-many-unique-keys ()
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (max 2 (lparallel:kernel-worker-count)))
        (writes-per-task 10000)
        (channel (lparallel:make-channel))
        (start-queue (lparallel.queue:make-queue))
        (h (safety-hash:make-safety-hash)))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (loop :for i :below num-tasks :do
        (lparallel:submit-task channel
                               (lambda (task-id)
                                 (lparallel.queue:pop-queue start-queue)
                                 (loop :for k :from task-id :below (+ task-id writes-per-task) :do
                                   (setf (safety-hash:gethash k h) task-id)))
                               (* i writes-per-task)))
      (loop :repeat num-tasks :do
        (lparallel.queue:push-queue t start-queue))
      (loop :repeat num-tasks :do
        (lparallel:receive-result channel))
      (is (= (safety-hash:hash-table-count h) (* num-tasks writes-per-task))))))

(deftest test-safety-hash-concurrent-write-and-clrhash ()
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (* 2 (lparallel:kernel-worker-count)))
        (writes-per-task 10000)
        (channel (lparallel:make-channel))
        (start-queue (lparallel.queue:make-queue))
        (h (safety-hash:make-safety-hash)))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (loop :with num-tasks/2 := (floor num-tasks 2)
            :for i :below num-tasks/2 :do
              (lparallel:submit-task channel
                                     (lambda (task-id)
                                       (lparallel.queue:pop-queue start-queue)
                                       (loop :with start := (* task-id writes-per-task)
                                             :for k :from start :below (+ start writes-per-task) :do
                                               (setf (safety-hash:gethash i h) task-id)))
                                     i)
              (lparallel:submit-task channel
                                     (lambda ()
                                       (lparallel.queue:pop-queue start-queue)
                                       (loop :repeat writes-per-task :do
                                         (safety-hash:clrhash h)))))
      (loop :repeat num-tasks :do
        (lparallel.queue:push-queue t start-queue))
      (loop :repeat num-tasks :do
        (lparallel:receive-result channel))
      (is (< (safety-hash:hash-table-count h) (* num-tasks writes-per-task))))))

(deftest test-safety-hash-concurrent-write-and-remhash ()
  (qvm:prepare-for-parallelization)
  (let ((num-tasks (* 2 (lparallel:kernel-worker-count)))
        (writes-per-task 10000)
        (channel (lparallel:make-channel))
        (start-queue (lparallel.queue:make-queue))
        (keys-queue (lparallel.queue:make-queue :fixed-capacity 1000))
        (h (safety-hash:make-safety-hash)))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (loop :with num-tasks/2 := (floor num-tasks 2)
            :for i :below num-tasks/2 :do
              (lparallel:submit-task channel
                                     (lambda (start)
                                       (lparallel.queue:pop-queue start-queue)
                                       (loop :for k :from start :below (+ start writes-per-task) :do
                                         (setf (safety-hash:gethash k h) start)
                                         (lparallel.queue:push-queue k keys-queue)))
                                     (* i writes-per-task))
              (lparallel:submit-task channel
                                     (lambda ()
                                       (lparallel.queue:pop-queue start-queue)
                                       (loop :repeat writes-per-task
                                             :for k := (lparallel.queue:pop-queue keys-queue) :do
                                               (assert (safety-hash:remhash k h))))))
      (loop :repeat num-tasks :do
        (lparallel.queue:push-queue t start-queue))
      (loop :repeat num-tasks :do
        (lparallel:receive-result channel))
      (is (= 0 (safety-hash:hash-table-count h))))))
