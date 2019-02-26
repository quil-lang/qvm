;;;; stress-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

;;; This file contains general stress tests, like aggressive memory
;;; allocation, to gauge the stability of the QVM.

(deftest test-qvm-garbage-collection ()
  "Test that large QVMs get garbage collected and do so without erroring."
  (format t "~&[Test output: ") (finish-output)
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
    (format t "~&[Test output: ") (finish-output)
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
    (format t "~&[Test output: ") (finish-output)
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
    (format t "~&[Test output: ") (finish-output)
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
          :do (setf qvm (qvm:make-qvm big-qubit-amount :shared-memory name))
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
