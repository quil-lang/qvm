# qvm

This directory contains an implementation of the Quantum Abstract
Machine, called a "Quantum Virtual Machine" or QVM.

## How To Run

The QVM is written in ANSI Common Lisp. An efficient, optimizing,
machine-code compiler called SBCL is recommended for its
execution. This is a free and open source implementation of Common
Lisp.

You need to install SBCL. You can do this in a few ways:

    1. From somewhere like `apt-get`.
    2. From the distributed binaries [here](http://sbcl.org/platform-table.html).
    3. From the git, also referred to by the above link.

Once SBCL is installed, to start, do the following.

```
$ cd path/to/qvm
$ sbcl
<output elided>
* (load "qvm.asd")
* (asdf:load-system :qvm)
```

The system will get compiled and loaded, and now can be interacted
with. To test, load the demo code and try running the QFT.

```
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