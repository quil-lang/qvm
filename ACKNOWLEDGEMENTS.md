# Acknowledgements

## Pre-Release History

Starting in 2016, the née `qsim`, Robert Smith started the Rigetti
QVM, which began as a substrate to study syntax and semantics for
quantum programming languages. At the time, the predominant syntax for
quantum programs was the graphical circuit notation, along with its
accompanying LaTeX syntax
[qasm](https://www.media.mit.edu/quanta/qasm2circ/). It also served as
a foundation for understanding "computational quantum computation".

The first Rigetti QVM implemented something akin to the "many worlds"
interpretation of quantum mechanics. Quantum programs were written
directly as Lisp programs, and each measurement split the "state of
the universe", and simulation tracked across each universe. This
proved neither efficient nor useful in the study and execution of
near-term algorithms, so it was changed to operate in the usual mode
with stochastic collapse of the wavefunction.

With Will Zeng's motivation, the QVM moved in a direction of being
more accessible. In order to make quantum programs easier to write, a
text-based S-expression syntax was used, until the development of
CL-QUIL and `quilc`, which included a parser for a quantum programming
language—an assembly-like language called Quil. The QVM ended up being
an implementation of Quil's quantum abstract machine.

The Rigetti QVM eventually implemented the full Quil language, and was
deployed as the primary backend to the Forest 1.0 service by the end
of 2016.

The QVM implemented a relatively efficient form of pure-state
evolution, though it didn't go through a handful of microarchitecture
optimizations until later. With Nick Rubin constantly challenging the
speed of the QVM, the QVM saw improvements over the course of a year,
bringing it to a level that was relatively competitive with other
simulators. Later, full multithreading was implemented.

Additional modes of execution for noisy simulations were added by
Nikolas Tezak and Erik Davis in 2017 and 2018 respectively. In 2017,
Robert Smith implemented the JIT compiler for Quil, causing it to
outperform OpenMP-optimized C programs, sometimes by a factor between
2 and 10.

During the development of the QVM, many people have made a range of
contributions. A summary of these can be found at the end of this
document.

The Rigetti QVM has benefited extensively from internal testers at
Rigetti. We provide special acknowledgement to Johannes Otterbach, who
found countless functional and performance issues. The QVM also
benefited from the thousands of users of Forest and Quantum Cloud
Services.

## Credits Roll

For security and privacy, the git history has been removed. The
contributors of the project, roughly in order of their commit count
according to `git shortlog --summary --numbered`, _prior to 2019_
were:

* **Robert Smith** (main developer)
* **Erik Davis** (density matrix simulation)
* **Zach Beane** (porting to CCL & LispWorks)
* **Nikolas Tezak** (stochastic simulation)
* **Mark Skilbeck** (porting and testing on Windows)
* **Peter Karalekas** (release management, automated testing, versioning, `nat-tuple` testing ☺️)
* **Eric Peterson** (fixes and improvements to the app)
* **Will Zeng** (installation, Dockerization, deployment, GHZ-on-a-grid in original QIL)
* **Nick Rubin** (VQE improvements)
* **Anthony Polloreno** (wavefunction sampling)
* **Aaron Vontell** (runtime safety checks)
* **Johannes Otterbach** (extensive testing and documentation improvements)  

## Open Source

The Rigetti QVM proudly depends on the work of the open source
community, as well as [SBCL](http://www.sbcl.org/),
[ASDF](https://common-lisp.net/project/asdf/), and Zach Beane's
continued maintenance of the [Quicklisp](https://www.quicklisp.org/)
project and repository.