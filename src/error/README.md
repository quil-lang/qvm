# `qvm.error`

This subpackage implements two classes:

1. `fowler-qvm`: An abstract class which decomposes depolarizing noise into five
   subchannels which are applied following (1) single-qubit identity gates,
   (2) non-identity single-qubit gates, (3) two-qubit gates, (4) reset
   operators, and (5) measurement operators.  These can be selective enabled,
   permitting one to study the relative effects of these noise sources on
   measurement circuits commonly found in parity check measurement circuits, as
   proposed in Section VII.B of [Fowler et al](https://arxiv.org/abs/1208.0928).
   This class is extended by two subclasses, `fowler-pure-state-qvm` and
   `fowler-stabilizer-qvm`, which back this noise model by a `pure-state-qvm`
   and a `stabilizer-qvm` respectively.
2. `error-qvm`: A separate subclass of `fowler-qvm` backed by "Pauli
   propagation".  Rather than tracking anything like the full state of a quantum
   circuit, a Pauli propagation model rather assumes that the circuit being
   executed is meant to measure stabilizer checks (so, in particular, would
   return a constant value of "no error" in the absence of noise), injects Pauli
   flip noise according to some noise model, and tracks how the flips propagate
   through the circuit to effect an eventual measurement.  This limitation is
   severe, but it means that the measurement results can be computed in space
   and time which are _linear_ in the qubit count, a marked improvement over
   even the stabilizer-/tableau-based QVM.
