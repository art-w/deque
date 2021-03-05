[Purely Functional, Real-Time Deques with Catenation](http://www.cs.tau.ac.il/~haimk/papers/jacm-deq.ps) \[284ko postscript\]
by H. Kaplan and R. E. Tarjan, journal of the ACM 31:11-16 (1999) 1709-1723 https://doi.org/10.1145/324133.324139

Following the paper, this package provides 3 implementations of double-ended
queues which let you push, pop and append elements at both ends of the list:

| Module  | cons | uncons | snoc | unsnoc          | append          | rev             | nth                |
|---------|:----:|:------:|:----:|:---------------:|:---------------:|:---------------:|:------------------:|
| Dequeue | O(1) | O(1)   | O(1) | O(1)            | :no_entry_sign: | O(1)            | O(log min(i, N-i)) |
| Steque  | O(1) | O(1)   | O(1) | :no_entry_sign: | O(1)            | :no_entry_sign: | :no_entry_sign:    |
| Deck    | O(1) | O(1)   | O(1) | O(1)            | O(1)            |                 | :no_entry_sign:    |

Note that the constant time complexities are strict and "real time", not
amortized nor lazy.

See [deque.mli](src/deque.mli) and [deque_sig.ml](src/deque_sig.ml) for the
full interface -- currently a rough subset of the standard
[List](https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html)
module.

This code would not have been possible without the fantastic support for GADTs
in OCaml. The invariants are encoded inside each datatypes; the algorithms then
follow, guided by the type checker. This does not result in the most readable
code however, so you should read the paper if you want to understand the big
ideas:

- Skew binary numbers to deamortize carry propagation
- Recursive slowdown and bootstrapping
- Datastructures with a hole / with a preferred path for `O(1)` finger inside a
  purely functional tree
