(in-package #:dqvm2-tests)

(deftest test-offset-arrays ()
  (let* ((g (make-instance 'global-addresses :number-of-qubits 5 :number-of-processes 3))
         (o (dqvm2::make-offset-arrays g)))

    ;; Fill the offset array for rank #0
    (dotimes (i (* (block-size g)
                   (+ (blocks-per-process g)
                      (remainder-blocks g))))
      (dqvm2::offset-arrays-push (+ 10 i) 0 o))

    ;; Check that the offsets were correctly stored.
    (let ((offsets (aref (slot-value o 'dqvm2::offsets) 0)))

      (dotimes (i (* (block-size g)
                     (+ (blocks-per-process g)
                        (remainder-blocks g))))

        (is (= (cffi:mem-aref offsets :int32 i)
               (+ 10 i)))))

    ;; Ensure that overflows are correctly handled.
    (signals error (dqvm2::offset-arrays-push 1000 0 o))

    ;; Reset, then test that the counters have been reinitialized.
    (dqvm2::reset-offset-arrays o)
    (loop :for count :across (slot-value o 'dqvm2::counts) :do (is (zerop count)))))
