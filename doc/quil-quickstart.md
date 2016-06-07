# Quil Quickstart

This is intended to be a very quick and to-the-point guide to the syntax of Quil, the Quantum Instruction language.

## Qubits and Bits

**Qubits**: Qubits are written as integers starting from `0`. There is no need to "declare" qubits.

**Bits**: The QVM has a notion of classical memory. The 5th bit in classical memory is written `[5]`.

## Gate Definitions

New gates can be defined with `DEFGATE`. You can either define a simple gate or a parameterized gate.

**Simple gates** are defined like so:

```
DEFGATE sqrt-not:
    1/sqrt(2) -1/sqrt(2)
    1/sqrt(2) 1/sqrt(2)
```
Note the four spaces of indentation of the entries. Entries can be numeric or simple arithmetic expressions. Complex numbers are written with `i`, like `1.0-3.0i`. Supported functions are `+-*^`, `sin`, `cos`, `sqrt`, `exp`, and `cis`. The `cis` function ("cosine i sine") is defined as

```
cis(x) = cos(x) + i*sin(x) = exp(i*x),
```

and `exp` is the exponential function. The constant `pi` is also available.

**Parameterized gates** are gates that contain numeric parameters. Parameters are written with a `%` sign in front:

```
DEFGATE phase(%theta):
    1, 0
    0, cis(%theta)
```

You can have as many parameters as you'd like.

## Gate Applications

Gates are applied by just writing the gate name followed by qubits. For example,

```
H 0
CPHASE(1.4142135) 1 0
Rx(3.14159) 2
```

## Including Files

You can include files/gate libraries with `INCLUDE`. A popular one to include is `stdgates.quil`:

```
INCLUDE "path/to/stdgates.quil"
```

## Measurements

You can do a measurement (collapsing the wavefunction), and optionally deposit the result into classical memory (measured in the computational basis).

This example measures qubit `3` and discards the result:

```
MEASURE 3
```

This example measures qubit `5` and depsosits it at memory address 7:

```
MEASURE 3 [7]
```

Measurements are especially useful for branching and looping.

## Branching and Looping

It is possible to jump to a location conditionally and unconditionally. Locations to jump to are called **jump targets** and are specified with the `LABEL` directive. The label has to be preceded with an `@` sign.

```
LABEL @start
G 0
CH 0 1
```

This label `@start` marks the point in the code where the gate `G` is executed.

We can unconditionally jump to this label using `JUMP`:

```
JUMP @start
```

There are two ways to conditionally jump based off of the contents at a memory address. If we are interested in jumping only when the contents of `[23]` is `1`, then we can do

```
JUMP-WHEN @start [23]
```

If we want to jump only when the contents of `[55]` is `0`, then we can do

```
JUMP-UNLESS @start [55]
```

A conditional branch on `[c]`, like

```
if (*c) {
    instrs...
}
rest...
```

can be implemented as follows:

```
JUMP-UNLESS @end [c]
instrs...
LABEL @end
rest...
```

**Exercise**: How do you implement a `while`-loop (in C notation) branching on address `[c]`?

```
while(*c) {
    A...
}
```

## Miscellaneous Operations

The following miscellaneous but nonetheless important instructions are permitted:

* `HALT`: Stop all execution.
* `NOP`: Do nothing.
* `RESET`: Reset the state of all qubits to |00...0〉.
* `WAIT [c]`: Wait until the bit in `[c]` is set. (Used for classical synchronization.)