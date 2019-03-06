;;;; stress-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

;;; This file contains general stress tests, like aggressive memory
;;; allocation, to gauge the stability of the QVM.

(deftest test-qvm-garbage-collection ()
  "Test that large QVMs get garbage collected and do so without erroring."
  (format t "~&    [Test output: ") (finish-output)
  (tg:gc :full t)
  (loop :with big-qubit-amount := 23
        ;; This should allocate about
        ;;
        ;;     (8 + 8) * 2^23 / 1024^2 = 128 MiB
        ;;
        ;; per iteration.
        :repeat #-sbcl 128
                #+sbcl (1+ (ceiling (/ (sb-ext:dynamic-space-size) 1024 1024) 128))
        :with qvm := nil
        :do (setf qvm (qvm:make-qvm big-qubit-amount))
            (qvm::reset-quantum-state qvm)
            (setf qvm nil)
            (write-char #\a) (finish-output)
            (tg:gc :full t))
  (write-char #\]) (finish-output)
  nil)

#+unix
(deftest test-posix-shared-memory-allocation/deallocation ()
  "Test allocation and deallocation of large blocks of POSIX shared memory. This test may result in total system failure."
  (run-unless-environment-has "DISABLE_SHARED_MEMORY_QVM_TESTS"
    (format t "~&    [Test output: ") (finish-output)
    (let ((len (* 2 1024 1024 1024)))
      (flet ((write-ones (shm)
               (cffi:foreign-funcall
                "memset"
                :pointer (qvm::posix-shared-memory-pointer shm)
                :int 1
                qvm::size_t len)))
        (loop :with dummy := nil
              :for seq :below 16
              :for name := (format nil "TEST_A__~D" (mod seq 8)) ; Make sure we repeat names
              :do (setf dummy (qvm::make-posix-shared-memory name len))
                  (is (not (cffi:null-pointer-p (qvm::posix-shared-memory-pointer dummy))))
                  (write-char #\a) (finish-output)
                  (write-ones dummy)
                  (write-char #\w) (finish-output)
                  (qvm::free-posix-shared-memory dummy)
                  (is (cffi:null-pointer-p (qvm::posix-shared-memory-pointer dummy)))
                  (write-char #\d) (finish-output)
                  (setf dummy nil))))
    (write-char #\]) (finish-output)
    (tg:gc :full t)))

#+unix
(deftest test-shared-array-allocation/deallocation ()
  "Make sure allocation and deallocation of shared arrays works."
  (run-unless-environment-has "DISABLE_SHARED_MEMORY_QVM_TESTS"
    (format t "~&    [Test output: ") (finish-output)
    (tg:gc :full t)
    (flet ((write-ones (array)
             (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0))
                      (type (simple-array (unsigned-byte 8) (*)) array))
             (fill array 1)))
      (declare (inline write-ones))
      (loop :with vect := nil
            :with free := nil
            :for seq :below 16
            :for name := (format nil "TEST_B__~D" (mod seq 8)) ; make sure we repeat names
            :do (multiple-value-setq (vect free)
                  (qvm::make-shared-array name (* 2 1024 1024 1024) '(unsigned-byte 8)))
                (write-char #\a) (finish-output)
                (write-ones vect)
                (write-char #\w) (finish-output)
                (setf vect nil)
                (funcall free)
                (write-char #\d) (finish-output)
                (setf vect nil
                      free nil))
      (write-char #\]) (finish-output))
    nil))

#+unix
(deftest test-shared-qvm-garbage-collection ()
  "Test that large shared QVMs get garbage collected and do so without erroring."
  (run-unless-environment-has "DISABLE_SHARED_MEMORY_QVM_TESTS"
    (format t "~&    [Test output: ") (finish-output)
    (tg:gc :full t)
    (loop :with big-qubit-amount := 26
          ;; This should allocate about
          ;;
          ;;     (8 + 8) * 2^26 / 1024^2 = 1 GiB
          ;;
          ;; per iteration.
          :with qvm := nil
          :for seq :below 16
          :for name := (format nil "TEST_C__~D" (mod seq 8)) ; make sure we repeat names
          :do (setf qvm (qvm:make-qvm big-qubit-amount :allocation name))
              (write-char #\a) (finish-output)
              (qvm::reset-quantum-state qvm) ; Do something with the
                                             ; memory.
              (setf qvm nil)
              ;; We actually have to explicitly garbage collect to make
              ;; sure the memory gets freed.
              (tg:gc :full t))
    (write-char #\]) (finish-output)
    nil))

#+(and sbcl unix)
(deftest test-shared-memory-exit-hook-presence ()
  "Ensure that that the shared memory deallocation hook is present."
  (is (member 'qvm::deallocate-all-shared-memories sb-ext:*exit-hooks*)))

(declaim (type fixnum **dummy-count**))
(global-vars:define-global-var **dummy-count** 0)

(defclass dummy-allocation ()
  ())

(defmethod qvm:allocation-length ((a dummy-allocation))
  8)

(defmethod qvm:allocate-vector ((a dummy-allocation))
  (values (make-vector (qvm:allocation-length a))
          (lambda ()
            (sb-ext:atomic-incf **dummy-count**)
            nil)))

;;; XXX: This test seems quite brittle, relying on lots of low-level
;;; details.
(deftest test-custom-allocator ()
  "Test that the allocator functionality seems to be working."
  (setf **dummy-count** 0)
  (tg:gc :full t)
  (let ((num-loops 100))
    ;; Keep allocations localized in the lexical environment below.
    (let ((a (make-instance 'dummy-allocation)))
      (loop
        ;; Boot stuff off to the next page/card.
        ;;
        ;; SBCL marks object liveness on a per page (more
        ;; specifically, per "card", which is SBCL lingo) basis. So we
        ;; want to make sure each object gets its own page by
        ;; allocating slightly more than a page each time. We actually
        ;; do 8 pages just to be safe, and in the case of SBCL, use an
        ;; internal constant that tells us precisely the size of each
        ;; allocated slab of memory.
        :with junk := (make-array #+sbcl (1+ sb-vm:gencgc-card-bytes)
                                  #-sbcl (1+ (* 8 (qvm::getpagesize)))
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0)
        :repeat num-loops
        :sum (let ((q (make-qvm 3 :allocation a)))
               ;; Do a little bit of nonsense with the QVM so it
               ;; doesn't get compiled away...
               (+ (length junk)
                  (length (copy-wavefunction (qvm::amplitudes q))))))
      ;; Do some allocation of at least 5 quarter page sizes to kick us
      ;; off to a new page since SBCL marks garbage on a page-per-page
      ;; basis.
      (loop :with alloc-size := #+sbcl (1- sb-vm:gencgc-card-bytes)
                                #-sbcl (1- (* 8 (qvm::getpagesize)))
            :repeat 1000
            :do (make-array alloc-size :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
    ;; Now garbage collect, hopefully hitting all of the finalizers.
    (format t "~&    [GCing... ") (finish-output)
    (tg:gc :full t)

    ;; We sleep to try to hit all finalizers. Stuff might be happening
    ;; in different threads...
    (format t "Sleeping for 1 second...")  (finish-output)
    (sleep 1.0)
    (format t "]")            (finish-output)
    ;; Check that we deallocated everything.
    (is (= num-loops **dummy-count**))
    (setf **dummy-count** 0)
    nil))
