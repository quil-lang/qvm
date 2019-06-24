(ql:quickload :qvm-tune-threads)
(use-package :qvm-tune-threads)

(defparameter *tm* (scan-num-threads 8 :print-info t))
