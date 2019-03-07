# Rigetti Quantum Virtual Machine

[![pipeline status](https://gitlab.com/rigetti/qvm/badges/master/pipeline.svg)](https://gitlab.com/rigetti/qvm/commits/master)

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
programs (parsed and compiled by [quilc](http://github.com/rigetti/quilc)) on a virtual machine that can
model various characteristics of (though without needing access to) a
true quantum computer.

The library is released under the [Apache license 2.0](LICENSE.txt).

### Usage

The QVM library is best loaded using Quicklisp. Please read and follow
the instructions in [`lisp-setup.md#install-quicklisp`](doc/lisp-setup.md) to get Quicklisp
installed. Pay particular attention to the section "Telling Quicklisp
Where Your Code Is".

>**Note**: `qvm` is not yet available through Quicklisp's distribution
>mechanism, and must be installed manually.

Download both this repository *and* [quilc](http://github.com/rigetti/quilc) into the
`ql:*local-project-directories*` location. If all is correct, the `qvm`
library can be loaded with

```shell
$ sbcl
```

```common-lisp
* (ql:quickload :qvm)
(:QVM)
```

QVM objects are created with `(qvm:make-qvm n)` where `n` is the number of
qubits the QVM should support; a program can then be loaded into the
QVM object with `(qvm:load-program *qvm* *program*)` where `*qvm*` is a
QVM object and `*program*` is a `cl-quil:parsed-program`
object.

Alternatively, the `qvm:run-program` function will handle QVM object
creation. For example,

``` common-lisp
* (setq *qvm* (qvm:run-program 2 (cl-quil:parse-quil-string "H 0")))
```

creates a 2-qubit QVM object and on it runs the Quil program `H 0`.

The qubit amplitudes can be inspected

``` common-lisp
* (qvm::amplitudes *qvm*)
#(#C(0.7071067811865475d0 0.0d0) #C(0.7071067811865475d0 0.0d0)
  #C(0.0d0 0.0d0) #C(0.0d0 0.0d0))
```

which shows, as expected, that `H 0` has put qubit-0 (the first two
complex numbers above) into an equal superposition of states `|0>` and
`|1>`.

Measurement of a quantum state causes it to collapse into one of its
basis states (`|0>` or `|1>`). This can be simulated with

``` common-lisp
* (qvm:measure-all *qvm*)
#<PURE-STATE-QVM {1004039753}>
(0 0)
```

Inspecting the QVM object's state shows that this effect mutates the
information stored on the QVM; i.e. the previous state information is lost

``` common-lisp
* (qvm::amplitudes *qvm*)
#(#C(1.0d0 0.0d0) #C(0.0d0 0.0d0)
  #C(0.0d0 0.0d0) #C(0.0d0 0.0d0))
```

Qubit zero's state has collapsed into the state `|0>`. Repeating this
process (from creating the QVM object to measuring qubits) would show
that both states would each come up with probability 0.5.

``` common-lisp
* (loop :with results := (vector 0 0)
        :with program := (cl-quil:parse-quil-string "H 0")
        :repeat 100
        :for (qvm state) := (multiple-value-list (qvm:measure (qvm:run-program 1 program) 0))
        :do (incf (aref results state))
        :finally (return results))
#(54 46)
```

### Examples

The QVM comes with some example code to illustrate usage of the
QVM. The example code can be found under `./examples/`. To run the
example code, first load `qvm-examples`

``` common-lisp
* (ql:quickload :qvm-examples)
(:QVM-EXAMPLES)
```

The function `bit-reversal-circuit` takes a list of qubit indices and
returns a list of instructions that will reverse the qubit amplitudes
in "bit-reversal order" (e.g., the coefficient of `|1110>` gets
mapped to `|0111>`):
that the amplitudes get reversed

``` common-lisp
(qvm-examples:bit-reversal-circuit '(1 2 3 4))
(#<SWAP 1 4> #<SWAP 2 3>)
```

For a given list of qubit indices, the function `qft-circuit` returns a
[Quantum Fourier transform](https://en.wikipedia.org/wiki/Quantum_Fourier_transform) Quil program ready to be passed to quilc for
compilation.

``` common-lisp
* (qvm-examples:qft-circuit '(1 2 3 4))
#<CL-QUIL:PARSED-PROGRAM {10040ABEE3}>
```

To inspect the object, we can use the `cl-quil::print-parsed-program` function

``` common-lisp
* (cl-quil::print-parsed-program (qvm-examples:qft-circuit '(1 2 3 4)))
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
```

## QVM, the application

The QVM application is contained with `./app/src/`, and provides a
stand-alone interface to the QVM library. It can be invoked directly
with the binary executable, or alternatively it can provide a server
that can be used over the network. Each has their benefits: the former
permits a simplified interface using the command-line switches (see
output of `qvm --help`), while the latter allows many remote connections
to a single in-memory QVM.

The application is released under the [GNU Affero General Public
License v3.0](app/LICENSE.txt).

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
the QVM. To explore those options, see the output of the following command:

```
$ ./qvm --help
```

By default, the QVM accepts programs from stdin and writes
results to stdout. Log messages are written to stderr.

```
$ echo 'H 0' | ./qvm
******************************
* Welcome to the Rigetti QVM *
******************************
Copyright (c) 2016-2019 Rigetti Computing.

(Configured with 8192 MiB of workspace and 8 workers.)

<134>1 2019-03-07T22:56:55Z workstation.local qvm 21177 - - Selected simulation method: pure-state
<134>1 2019-03-07T22:56:55Z workstation.local qvm 21177 - - Reading program.
<134>1 2019-03-07T22:56:55Z workstation.local qvm 21177 - - Allocating memory for QVM of 1 qubits.
<134>1 2019-03-07T22:56:55Z workstation.local qvm 21177 - - Allocation completed in 7 ms.
<134>1 2019-03-07T22:56:55Z workstation.local qvm 21177 - - Loading quantum program.
<134>1 2019-03-07T22:56:55Z workstation.local qvm 21177 - - Executing quantum program.
<134>1 2019-03-07T22:56:55Z workstation.local qvm 21177 - - Execution completed in 4 ms.
<134>1 2019-03-07T22:56:55Z workstation.local qvm 21177 - - Printing classical memory and 1-qubit state.
Classical memory (low -> high indexes):
    No memory.
Amplitudes:
    |0>: 0.7071067811865475,                                    P= 50.0%
    |1>: 0.7071067811865475,                                    P= 50.0%
```

Alternatively the QVM can be started as a server that will accept
instructions over a network connection

```
$ ./qvm -S
******************************
* Welcome to the Rigetti QVM *
******************************
Copyright (c) 2016-2019 Rigetti Computing.

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
* (asdf:test-system :qvm)
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

## Automated Packaging with Docker

The CI pipeline for `qvm` produces a Docker image, available at
[`rigetti/qvm`](https://hub.docker.com/r/rigetti/qvm).

To get the latest stable
version of `qvm`, run `docker pull rigetti/qvm`.

## Running the QVM with Docker

As outlined above, the QVM supports two modes of operation: stdin and server.

To run the `qvm` in stdin mode, do the following:

```shell
echo "H 0" | docker run --rm -i rigetti/qvm
```

To run the `qvm` in server mode, do the following:

```shell
docker run --rm -it -p 5000:5000 rigetti/qvm -S
```

If you would like to change the port of the server to `PORT`, you can alter the command as follows:

```shell
docker run --rm -it -p PORT:PORT rigetti/qvm -S -p PORT
```

## Release Process

1. Update `VERSION.txt` and dependency versions (if applicable) and push the commit to `master`.
2. Push a git tag `vX.Y.Z` that contains the same version number as in `VERSION.txt`.
3. Verify that the resulting build (triggered by pushing the tag) completes successfully.
4. Publish a [release](https://github.com/rigetti/qvm/releases) using the tag as the name.
5. Close the [milestone](https://github.com/rigetti/qvm/milestones) associated with this release,
   and migrate incomplete issues to the next one.
