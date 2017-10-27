# Rigetti Quantum Virtual Machine

[![Build Status](http://bamboo.lab.rigetti.com/plugins/servlet/wittified/build-status/QCS-QVM)](http://bamboo.lab.rigetti.com/browse/QCS-QVM)

This directory contains a classical implementation of the Quantum
Abstract Machine, called a "Quantum Virtual Machine" or QVM.

This directory only contains the library code. For executables, see `qvm-app` and `quilbasic`.

## Dependencies

The `qvm` has only been tested to run on Linux and macOS on AMD64 and ARM.

The system `qvm` and associated tests depend on:

- For UNIX systems, a C compiler accessible from the `cc` command.
- Relatively up-to-date SBCL and ASDF. Follow the instructions in
  [`lisp-setup.md`](doc/lisp-setup.md) for details.
- Up-to-date [`cl-quil`](https://github.com/rigetticomputing/cl-quil). This itself requires [`alexa`](https://github.com/rigetticomputing/alexa) and [`magicl`](https://github.com/rigetticomputing/magicl), the latter of which is slightly non-trivial to get going.

## How to Run Interactively

The QVM is written in ANSI Common Lisp. An efficient, optimizing, machine-code compiler called SBCL is recommended for its execution. This is a free and open source implementation of Common Lisp. Follow the instructions in [`lisp-setup.md`](doc/lisp-setup.md) for details.

Once setup, load the QVM by the following commands.

```
$ cd path/to/qvm
$ sbcl
<output elided>
* (load "qvm.asd")
* (ql:quickload :qvm)
```

The system will get compiled and loaded, and can now be interacted with.

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

you can exit by choosing the appropriate option number (usually that which is labeled `ABORT`, in this case `1`), or pressing control-d.

To quit, type control-d.

## How to Test

There is what might be considered alpha support for Semaphore CI. Every build will get built and tested. A failure can indicate failure to build or failure of the test suite. You should check below.

### If you just want to run tests

Just call `make test` at the command line.

### If you want to run tests *and* debug them

Within Lisp, do this once to install all of the requisite packages:

```
(ql:quickload :qvm-tests)
```

If you get an error saying that the system `QVM-TESTS` cannot be found, try clearing the cache with `make cleanall` at the command line.

Once installed, once you've loaded `qvm` using the normal means, you can test the system by doing

```
(asdf:test-system :qvm)
```

This will load all of the tests and run them. Here is example output for passing tests.

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

The `T` (Lisp true value) indicates there was no error in all of the tests.

### Clearing the Cache
Lisp caches a lot of builds so that not every single file needs to be recompiled. In rare instances, there's confusion and the cache doesn't get properly invalidated. (This can happen when moving files across machines, for example.) Lisp's cache and Quicklisp's system index can be cleaned by doing the following command:

```
make cleanall
```

This will delete any built executables as well.
