# Rigetti Quantum Virtual Machine

This directory contains two projects. The first, `qvm`, is a classical
implementation of the Quantum Abstract Machine (QAM), called a
"Quantum Virtual Machine" (QVM). The second, `qvm-app`, is the
application interface to interacting with the QVM, either directly
through the `qvm` binary or via its server interface.

The definition of the QAM was developed at Rigetti in a paper titled [A
Practical Quantum Instruction Set Architecture](https://arxiv.org/pdf/1608.03355.pdf).

## QVM, the library

The QVM library is contained within `./src/`, and provides the
implementation of the Quantum Abstract Machine. It evaluates Quil
programs (parsed by `cl-quil`) on a virtual machine that can model
various characteristics of (though without needing access to) a true
quantum computer.

### Usage

Follow the instructions in [`lisp-setup.md`](doc/lisp-setup.md) to
satisfy the dependencies required to load the QVM library. Afterwhich,
the `qvm` library can be loaded

```
$ sbcl

* (ql:quickload :qvm)
;;; <snip>compilation output</snip>
(:QVM)
* (qvm:make-qvm 10)
#<QVM:PURE-STATE-QVM {10090A7C23}>
```

## QVM, the application

The QVM application is contained with `./app/src/`, and provides a
stand-alone interface to the QVM library. It can be invoked directly
with the binary executable, or alternatively it can provide a server
that can be used over the network. Each has their benefits: the former
permits a simplified interface using the command-line switches (see
output of `qvm --help`), while the latter allows many remote
connections to a single in-memory QVM.

### Usage

To build the QVM application follow instructions in
[`lisp-setup.md`](doc/lisp-setup.md). In the top-level directory, run
the `Makefile` with

```
$ make qvm-app
```

The "workspace" size can be configured at compile-time with

```
$ make QVM_WORKSPACE=4096
```

This will produce a binary executable `qvm` in the same directory. The
QVM application has a few command-line switches used to configure the
QVM. To explore those options, see the output of 

```
$ ./qvm --help
```

To provide a Quil program directly to the QVM, provide the program on
stdin and use the `-e` switch

```
$ echo "H 0" | ./qvm -e
******************************
* Welcome to the Rigetti QVM *
******************************
Copyright (c) 2018 Rigetti Computing.

(Configured with 2048 MiB of workspace and 8 workers.)

[2018-12-13 14:07:06] Selected simulation method: pure-state
[2018-12-13 14:07:06] Reading program.
[2018-12-13 14:07:06] Allocating memory for QVM of 1 qubits.
[2018-12-13 14:07:06] Allocation completed in 4 ms.
[2018-12-13 14:07:06] Loading quantum program.
[2018-12-13 14:07:06] Executing quantum program.
[2018-12-13 14:07:06] Execution completed in 4 ms.
[2018-12-13 14:07:06] Printing 1-qubit state.
[2018-12-13 14:07:06] Amplitudes:
[2018-12-13 14:07:06]   |0>: 0.7071067811865475, P= 50.0%
[2018-12-13 14:07:06]   |1>: 0.7071067811865475, P= 50.0%
[2018-12-13 14:07:06] No classical memory present.
```

Alternatively the QVM can be started as a server that will accept
instructions over a network connection

```
$ ./qvm -S
******************************
* Welcome to the Rigetti QVM *
******************************
Copyright (c) 2018 Rigetti Computing.

(Configured with 2048 MiB of workspace and 8 workers.)

[2018-12-13 14:36:20] Selected simulation method: pure-state
[2018-12-13 14:36:20] Starting server on port 5000.
```

This is how the [pyQuil](https://github.com/rigetti/pyquil) Python
library communicates with a QVM.

## Testing

Tests can be run from the `Makefile` 

```
make test
```

or from within SBCL

```
(ql:quickload :qvm-tests)
(asdf:test-system :qvm)
```

Any contribution to this project should foremost not break any current
tests (run tests before making a pull request), and should be
accompanied by relevant *new* tests.

## Clearing the Cache

Lisp caches a lot of builds so that not every single file needs to be
recompiled. In rare instances, there's confusion and the cache doesn't
get properly invalidated. (This can happen when moving files across
machines, for example.) Lisp's cache and Quicklisp's system index can
be cleaned by doing the following command:

```
make cleanall
```

This will delete any built executables as well.
