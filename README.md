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

The QVM library is best loaded using `quicklisp`. Please read and follow
the instructions in [`lisp-setup.md`](doc/lisp-setup.md) to get `quicklisp` installed. Pay
particular attention to the section "Telling Quicklisp Where Your Code
Is".

Download both this repository *and* [QUILC](http://github.com/rigetti/quilc) into the
`ql:*local-project-directories*` location. If all is correct, the `qvm`
library can be loaded with

```shell
$ sbcl

```

```common-lisp
(ql:quickload :qvm)
;;; <snip>compilation output</snip>
(:QVM)
(qvm:make-qvm 10)
#<QVM:PURE-STATE-QVM {10090A7C23}>
```

### Examples

The QVM comes with some example code to illustrate usage of the
QVM. The example code can be found under `./examples/`. To run the
example code, first load `qvm-examples`

``` common-lisp
(ql:quickload :qvm-examples)

```

The function `bit-reversal-circuit` takes a list of qubit indices and
returns a list of instructions that will reverse the qubit amplitudes:

``` common-lisp
(qvm-examples:bit-reversal-circuit '(1 2 3 4))
(#<SWAP 1 4> #<SWAP 2 3>)
```

For a given list of qubit indices, the function `qft-circuit` returns a
[Quantum Fourier transform](https://en.wikipedia.org/wiki/Quantum_Fourier_transform) Quil program ready to be passed to QUILC for
compilation.

``` common-lisp
(qvm-examples:qft-circuit '(1 2 3 4))
#<CL-QUIL:PARSED-PROGRAM {10040ABEE3}>
```

To inspect the object, we can use the `cl-quil::print-parsed-program` function

``` common-lisp
(cl-quil::print-parsed-program (qvm-examples:qft-circuit '(1 2 3 4)))
H 4
CPHASE(pi/2) 3 4
H 3
CPHASE(pi/4) 2 4
CPHASE(pi/2) 2 3
H 2
CPHASE(pi/8) 1 4
CPHASE(pi/4) 1 3
CPHASE(pi/2) 1 2
H 1
SWAP 1 4
SWAP 2 3
NIL
```

## QVM, the application

The QVM application is contained with `./app/src/`, and provides a
stand-alone interface to the QVM library. It can be invoked directly
with the binary executable, or alternatively it can provide a server
that can be used over the network. Each has their benefits: the former
permits a simplified interface using the command-line switches (see
output of `qvm --help`), while the latter allows many remote connections
to a single in-memory QVM.

### Usage

To build the QVM application follow instructions in
[`lisp-setup.md`](doc/lisp-setup.md). In the top-level directory, run
the `Makefile` with

```
$ make qvm
```

This will produce a binary executable `qvm` in the same directory.

In some situtations, using a large number of qubits may caus heap
exhaustion. To increase the memory available for the QVM, recompile
and specify the workspace size (in MB)

```
$ make QVM_WORKSPACE=4096 qvm
```

The QVM application has a few command-line switches used to configure
the QVM. To explore those options, see the output of

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

<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Selected simulation method: pure-state
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Reading program.
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Allocating memory for QVM of 1 qubits.
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Allocation completed in 3 ms.
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Loading quantum program.
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Executing quantum program.
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Execution completed in 3 ms.
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Printing 1-qubit state.
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - Amplitudes:
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - -   |0>: 0.7071067811865475, P= 50.0%
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - -   |1>: 0.7071067811865475, P= 50.0%
<134>1 2019-01-28T17:00:53Z workstation.local qvm 98179 - - No classical memory present.
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

<134>1 2019-01-28T19:06:07Z workstation.local qvm 3118 - - Selected simulation method: pure-state
<134>1 2019-01-28T19:06:07Z workstation.local qvm 3118 - - Starting server on port 5000.
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
