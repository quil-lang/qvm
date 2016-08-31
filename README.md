# qvm

[![Build Status](https://semaphoreci.com/api/v1/projects/ba9d589a-9d74-400d-980f-785dec5657aa/811586/badge.svg)](https://semaphoreci.com/spikecurtis/qvm)

This directory contains a classical implementation of the Quantum
Abstract Machine, called a "Quantum Virtual Machine" or QVM.

## Dependencies

System `qvm` and tests depend on:

- Relatively up-to-date SBCL and ASDF. Follow the instructions in
  `doc/lisp-setup.md` for details.

- Up-to-date [`cl-quil`](https://github.com/rigetticomputing/cl-quil)
  (which itself depends on up-to-date
  [`alexa`](https://github.com/rigetticomputing/alexa)).

Otherwise, system `qvm` is intended to be written portably.

## How to Run Interactively

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

The system will get compiled and loaded, and can now be interacted
with.

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

## How to Test

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

# How to Build a Stand-Alone Executable

Building an executable requires `buildapp`. See the Lisp instructions
in `doc/lisp-setup.md` for details.

Before building, you must ensure that all dependencies are downloaded
from the internet. To do this, follow the instructions to run `qvm`
interactively, but do the following step additionally:

```
(load "qvm-app.asd")
(ql:quickload :qvm-app)
```

This only needs to be done once (any time new third-party dependencies
are introduced).

Building is otherwise easy, just call `make` in the `qvm`
directory. This should produce output like the following:

```
$ make
buildapp --output qvm \
       		 --dynamic-space-size 1024 \
       		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
       		 --asdf-tree "./../" \
       		 --load-system qvm-app \
       		 --logfile build-output.log \
       		 --entry qvm-app::%main
;; loading system "qvm-app"
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into qvm:
writing 4832 bytes from the read-only space at 0x20000000
writing 4624 bytes from the static space at 0x20100000
writing 82509824 bytes from the dynamic space at 0x1000000000
done]
```

If there were issues building it, then a full log of compilation
output can be found in `build-output.log`.

By default, the QVM reserves 1 GB of workspace for computations. This is probably good enough for around 20 or so qubits. If you are doing large computations or you find that you get errors akin to "heap exhausted", you may opt to build the QVM with a larger workspace. To do this, run `make` with the variable `QVM_WORKSPACE` equal to the number of megabytes desired. For example,

```
$ make QVM_WORKSPACE=4096
```

Now you should have an executable called `qvm`. A simple test is to
run the `bell.quil` example to produce a Bell state, using the QVM's `-e` option.

```
$ ./qvm -e examples/bell.quil
[2016-08-31 14:16:02] Welcome to the Rigetti QVM.
[2016-08-31 14:16:02] (Configured with 1024 MiB of workspace.)
[2016-08-31 14:16:02] Reading program.
[2016-08-31 14:16:03] Allocating memory for QVM of 2 qubits.
[2016-08-31 14:16:03] Allocation completed in 8 ms.
[2016-08-31 14:16:03] Loading quantum program.
[2016-08-31 14:16:03] Executing quantum program.
[2016-08-31 14:16:03] Execution completed in 75 ms.
[2016-08-31 14:16:03] Printing state.
[2016-08-31 14:16:03] Amplitudes: 0.7071067811865475, 0.0, 0.0, 0.7071067811865475
[2016-08-31 14:16:03] Probabilities: 0.4999999999999999, 0.0, 0.0, 0.4999999999999999
[2016-08-31 14:16:03] Classical memory (MSB -> LSB): 0000000000000000000000000000000000000000000000000000000000000000
```

As seen, a classical Bell state was produced.

You can get a brief help message with `qvm --help`.

# Deployment to AWS
Instructions can be found [here](https://github.com/rigetticomputing/qvm/wiki).
