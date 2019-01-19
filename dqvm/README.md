# The Distributed Quantum Virtual Machine

*Note*: In this directory is an **experimental**, **unsupported**, and **incomplete** version of a pure state quantum virtual machine (QVM) that can be distributed on a compute cluster.

## Running

Prerequisites:

* SBCL, Quicklisp, and buildapp
* Open MPI
* The `qvm` library and dependencies

To build, do:

```
make
```

This will make an MPI-compatible binary called `dqvm`. Unfortunately, the binary currently needs to be installed on machines which also have a compiled implementation of `cl-mpi`, which can be obtained simply by doing

```
(ql:quickload :cl-mpi)
```

once on the machine of interest. (This is because `cl-mpi` makes a stub dynamic library which is evidently required to be loaded upon application startup.)

To run locally, with `<n>` processes, do:

```
mpirun -np <n> --oversubscribe ./dqvm -e <quil-file>
```

To run on a cluster of `<n>` nodes, write out your hosts in a host file `<hostfile>`, and do:

```
mpirun -np <n> --hostfile <hostfile> ./dqvm -e <quil-file>
```

## History

This project was started by Robert Smith and Lauren Capelluto in February 2018 at Rigetti Computing. It was initially created purely locally using threads with concurrent mailboxes. It was prototyped on a ten node cluster called "rackpup". Each node is a Raspberry Pi Model 3 Model B with a 1.2 GHz quad-core ARM Cortex-A53 and 1 GB of RAM, running on a 64-bit pre-release of Debian.