;;;; bench/suite.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-benchmarks)

(defun serialize-alist (alist)
  (yason:with-object nil
    (loop :for (aspect value) :on alist :by #'cddr
          :for serialized-aspect := (prin1-to-string aspect)
          :for serialized-value := (if (integerp value)
                                       value
                                       (float value))
          :do (yason:encode-object-element serialized-aspect
                                           serialized-value))))

(defun results-as-json (results)
  "Encode the results table RESULTS of the benchmark run."
  (yason:with-output-to-string* (:indent nil)
    (yason:with-object ()
      (maphash (lambda (k v)
                 (let ((benchmark-name (prin1-to-string k)))
                   (yason:with-object-element (benchmark-name)
                     (loop :for (aspect . statistics) :in v
                           :for aspect-name := (prin1-to-string aspect)
                           :do (yason:with-object nil
                                 (yason:with-object-element (aspect-name)
                                   (serialize-alist statistics)))))))
               results))))

(defun run-benchmarks (&key (headless nil))
  "Run all QVM benchmarks. If HEADLESS is T, quit on completion."
  (qvm:prepare-for-parallelization)
  (cond
    ((null headless)
     (run-package-benchmarks :package ':qvm-benchmarks
                             :verbose t))
    (t
     (let ((successp (run-package-benchmarks :package ':qvm-benchmarks
                                             :verbose t)))
       (uiop:quit (if successp 0 1))))))

