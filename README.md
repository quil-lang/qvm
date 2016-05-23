# qvm

[![Build Status](https://semaphoreci.com/api/v1/projects/ba9d589a-9d74-400d-980f-785dec5657aa/811586/badge.svg)](https://semaphoreci.com/spikecurtis/qvm)

This directory contains a classical implementation of the Quantum
Abstract Machine, called a "Quantum Virtual Machine" or QVM.

## Dependencies

System `qvm` and tests depend on:

- Relatively up-to-date SBCL and ASDF. Follow the instructions in
  `doc/lisp-setup.md` for details.

Otherwise, system `qvm` is intended to be written portably.

System `QVM-APP` depends on

- `libev`: Mac: Install `libev` with `brew`; Linux: Install `libev4` with `apt-get`.

This will only run on a UNIX-like.

## How To Run Interactively

The QVM is written in ANSI Common Lisp. An efficient, optimizing,
machine-code compiler called SBCL is recommended for its
execution. This is a free and open source implementation of Common
Lisp. Follow the instructions in `doc/lisp-setup.md` for details.

Once setup, load the QVM by the following commands.

```
$ cd path/to/qvm
$ sbcl
<output elided>
* (load "qvm.asd")
* (ql:quickload :qvm)
```

The system will get compiled and loaded, and now can be interacted
with.

To do a fun test, load the examples package and the demo code and try
running the QFT.

```
* (load "qvm-examples.asd")
* (ql:quickload :qvm-examples)
* (load "demo/demo.lisp")
* (qvm::test-fourier)
```

If all goes right, you'll see the output of four 2-qubit Fourier
transforms. Note that `#C(a b)` is syntax for the complex number `a + bi`.

```
Initial state |00> : #C(1.0d0 0.0d0), #C(0.0d0 0.0d0), #C(0.0d0 0.0d0), #C(0.0d0 0.0d0)
Final state QFT|00>: #C(0.4999999999999999d0 0.0d0), #C(0.4999999999999999d0 0.0d0), #C(0.4999999999999999d0 0.0d0), #C(0.4999999999999999d0 0.0d0)
Initial state |01> : #C(0.0d0 0.0d0), #C(1.0d0 0.0d0), #C(0.0d0 0.0d0), #C(0.0d0 0.0d0)
Final state QFT|01>: #C(0.4999999999999999d0 0.0d0), #C(3.0616169978683824d-17 0.4999999999999999d0), #C(-0.4999999999999999d0 0.0d0), #C(-3.0616169978683824d-17 -0.4999999999999999d0)
Initial state |10> : #C(0.0d0 0.0d0), #C(0.0d0 0.0d0), #C(1.0d0 0.0d0), #C(0.0d0 0.0d0)
Final state QFT|10>: #C(0.4999999999999999d0 0.0d0), #C(-0.4999999999999999d0 0.0d0), #C(0.4999999999999999d0 0.0d0), #C(-0.4999999999999999d0 0.0d0)
Initial state |11> : #C(0.0d0 0.0d0), #C(0.0d0 0.0d0), #C(0.0d0 0.0d0), #C(1.0d0 0.0d0)
Final state QFT|11>: #C(0.4999999999999999d0 0.0d0), #C(-3.0616169978683824d-17 -0.4999999999999999d0), #C(-0.4999999999999999d0 0.0d0), #C(3.0616169978683824d-17 0.4999999999999999d0)
```

If you ever find yourself in a debugger, like so:

```
debugger invoked on a SIMPLE-ERROR in thread
#<THREAD "main thread" RUNNING {1002FDE873}>:
  oops!

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE] Continue running from this point
  1: [ABORT   ] Exit debugger, returning to top level.

(SB-INT:SIMPLE-EVAL-IN-LEXENV (CERROR "Continue running from this point" "oops!") #<NULL-LEXENV>)
```

you can exit by choosing the appropriate option number (usually that
which is labeled `ABORT`, in this case `1`), or pressing control-d.

To quit, type control-d.

## How To Test

There is what might be considered beta support for Semaphore CI. Every
build will get built and tested. A failure can indicate failure to
build or failure of the test suite. However, per Issue #5, the output
of *test failures* isn't so instructive. However, if a test failure is
encountered, one should try to reproduce with the steps below.

Within Lisp, do this once to install all of the requisite packages:

```
(ql:quickload :qvm-tests)
```

If you get an error saying that the system `QVM-TESTS` cannot be
found, look in the directory above `qvm` for a `system-index.txt` file
and delete it, and then retry.

Once installed, once you've loaded `qvm` using the normal means, you
can test the system by doing

```
(asdf:test-system :qvm)
```

This will load all of the tests and run them. Here is example output
for passing tests.

```
CL-USER> (asdf:test-system :qvm)
<...elided compilation output...>
QVM-TESTS (Suite)
  TEST-NAT-TUPLE-OPERATIONS                                               [ OK ]
  TEST-INDEX-TO-ADDRESS                                                   [ OK ]
  TEST-HADAMARD                                                           [ OK ]
  TEST-INVERSION                                                          [ OK ]
  TEST-BELL                                                               [ OK ]
  TEST-SWAP                                                               [ OK ]
  TEST-FOURIER                                                            [ OK ]

; 
; compilation unit finished
;   printed 40 notes
T
```

The `T` (Lisp true value) indicates there was no error in all of the
tests.
