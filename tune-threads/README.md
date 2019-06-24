# Tuning the number of threads (workers) used by the qvm

This code is designed to determine the optimal number of threads to use when running the qvm,
with optimality defined by smallest wall-clock execution time.

## Examples

* Find the optimal number of threads to use for 4, 15, and 18 qubit programs (on a four-core machine)
```lisp
(ql:quickload :qvm-tune-threads)
(use-package :qvm-tune-threads)
CL-USER> (optimal-num-threads 4)
1
CL-USER> (optimal-num-threads 15)
3
CL-USER> (optimal-num-threads 18)
4
```

* Print the optimal number of threads, together with relative times for each number of threads
for number of qubits ranging from 2 through 14.
```lisp
(defparameter *tms* (scan-num-qubits (alexandria:iota 14 :start 2) :print-info t))
```

```lisp
1: #(1.00 1.66 1.20 2.83)
1: #(1.00 1.63 1.57 15.61)
1: #(1.00 1.68 1.72 3.72)
1: #(1.00 1.46 1.65 14.93)
1: #(1.00 1.23 1.33 11.23)
2: #(1.01 1.00 1.10 10.00)
2: #(1.09 1.00 1.11 6.28)
2: #(1.15 1.00 1.02 7.41)
3: #(1.40 1.03 1.00 5.84)
3: #(1.80 1.14 1.00 3.88)
3: #(2.11 1.35 1.00 2.78)
3: #(2.55 1.36 1.00 1.92)
3: #(2.72 1.43 1.00 1.33)
3: #(2.74 1.49 1.00 1.03)
```

* Examine timing data used when optimizing the number of threads for 8-qubit programs.

```
CL-USER> (defparameter *tm* (scan-num-threads 8))
*TM*
CL-USER> (optimal-num-threads-from-scan *tm*) ; compute the optimal number of threads
2
CL-USER> (alexandria:hash-table-values *tm*)
(#S(QVM-TUNE-THREADS::TIMING
    :PRE-TIMING #S(QVM-TUNE-THREADS::PRE-TIMING
                   :INIT-NUM-TRIALS 1
                   :NUM-TRIALS 16
                   :TRIAL-TIME 0.177)
    :NUM-TRIALS 91
    :NUM-THREADS 4
    :TARGET-TIME 1.0
    :RUN-TIME 0.991)
 #S(QVM-TUNE-THREADS::TIMING
    :PRE-TIMING #S(QVM-TUNE-THREADS::PRE-TIMING
                   :INIT-NUM-TRIALS 1
                   :NUM-TRIALS 128
                   :TRIAL-TIME 0.136)
    :NUM-TRIALS 942
    :NUM-THREADS 3
    :TARGET-TIME 1.0
    :RUN-TIME 1.018)
 #S(QVM-TUNE-THREADS::TIMING
    :PRE-TIMING #S(QVM-TUNE-THREADS::PRE-TIMING
                   :INIT-NUM-TRIALS 1
                   :NUM-TRIALS 256
                   :TRIAL-TIME 0.166)
    :NUM-TRIALS 1543
    :NUM-THREADS 2
    :TARGET-TIME 1.0
    :RUN-TIME 0.999)
 #S(QVM-TUNE-THREADS::TIMING
    :PRE-TIMING #S(QVM-TUNE-THREADS::PRE-TIMING
                   :INIT-NUM-TRIALS 1
                   :NUM-TRIALS 512
                   :TRIAL-TIME 0.179)
    :NUM-TRIALS 2861
    :NUM-THREADS 1
    :TARGET-TIME 1.0
    :RUN-TIME 0.992))
```
