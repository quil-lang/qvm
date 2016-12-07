# Rigetti Quantum Virtual Machine

[![Build Status](https://semaphoreci.com/api/v1/projects/ba9d589a-9d74-400d-980f-785dec5657aa/811586/badge.svg)](https://semaphoreci.com/spikecurtis/qvm)

This directory contains a classical implementation of the Quantum
Abstract Machine, called a "Quantum Virtual Machine" or QVM.

## Dependencies

The system `qvm` and associated tests depend on:

- For UNIX systems, a C compiler accessible from the `cc` command.

- Relatively up-to-date SBCL and ASDF. Follow the instructions in
  [`lisp-setup.md`](doc/lisp-setup.md) for details.

- Up-to-date [`cl-quil`](https://github.com/rigetticomputing/cl-quil)
  (which itself depends on up-to-date
  [`alexa`](https://github.com/rigetticomputing/alexa)).

Otherwise, system `qvm` is intended to be written portably across operating systems
and processor architectures.

## How to Run Interactively

The QVM is written in ANSI Common Lisp. An efficient, optimizing,
machine-code compiler called SBCL is recommended for its
execution. This is a free and open source implementation of Common
Lisp. Follow the instructions in [`lisp-setup.md`](doc/lisp-setup.md)
for details.

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
build or failure of the test suite. You should check below.

### If you just want to run tests

Just call `make test` at the command line.

### If you want to run tests *and* debug them

Within Lisp, do this once to install all of the requisite packages:

```
(ql:quickload :qvm-tests)
```

If you get an error saying that the system `QVM-TESTS` cannot be
found, try clearing the cache with `make cleanall` at the command line.

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

## How to Build a Stand-Alone Executable

Building an executable requires `buildapp`. See the Lisp instructions
in [`lisp-setup.md`](doc/lisp-setup.md) for details.

Before building, you must ensure that all dependencies are downloaded
from the internet. To do this, just type the command

```
make deps
```

This only needs to be done once (any time new third-party dependencies
are introduced). This also updates Quicklisp if needed.

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
[defragmenting immobile space... done]
[saving current Lisp image into qvm:
writing 4800 bytes from the read-only space at 0x20000000
writing 4048 bytes from the static space at 0x20100000
writing 2273280 bytes from the immobile space at 0x20300000
writing 18917136 bytes from the immobile space at 0x21b00000
writing 56164352 bytes from the dynamic space at 0x1000000000
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
******************************
* Welcome to the Rigetti QVM *
******************************
(Configured with 1024 MiB of workspace and 7 workers.)

[2016-12-05 10:28:31] Reading program.
[2016-12-05 10:28:31] Allocating memory for QVM of 2 qubits.
[2016-12-05 10:28:31] Allocation completed in 3 ms.
[2016-12-05 10:28:31] Loading quantum program.
[2016-12-05 10:28:31] Executing quantum program.
[2016-12-05 10:28:31] Execution completed in 62 ms.
[2016-12-05 10:28:31] Printing state.
[2016-12-05 10:28:31] Amplitudes:
[2016-12-05 10:28:31]   |00>: 0.7071067811865475, P= 50.0%
[2016-12-05 10:28:31]   |01>: 0.0, P=  0.0%
[2016-12-05 10:28:31]   |10>: 0.0, P=  0.0%
[2016-12-05 10:28:31]   |11>: 0.7071067811865475, P= 50.0%
[2016-12-05 10:28:31] Classical memory (MSB -> LSB): 0000000000000000000000000000000000000000000000000000000000000000
```

As seen, a classical Bell state was produced.

You can get a brief help message with `qvm --help`.

### Clearing the Cache
Lisp caches a lot of builds so that not every single file needs to be recompiled. In rare instances, there's confusion and the cache doesn't get properly invalidated. (This can happen when moving files across machines, for example.) Lisp's cache and Quicklisp's system index can be cleaned by doing the following command:

```
make cleanall
```

This will delete any built executables as well.

## Deploying to AWS
Instructions can be found [here](https://github.com/rigetticomputing/qvm/wiki).

## Installing using Docker
If you have docker installed and have been added to our private Dockerhub (ask Will, Robert, or Spike for this) then you can use docker to set up a local copy of the QVM with only a few commands and no other installation. First pull the docker image
```
docker pull rigetticomputing/qvm
```
This will download the qvm code.  To run this qvm run
```
docker run rigetticomputing/qvm
```
This should start the QVM on the default port:5000.  You should see a welcome message!
