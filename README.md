> **[Purely Functional, Real-Time Deques with Catenation]** \[284ko postscript\] \
> by Haim Kaplan and Robert E. Tarjan \
> journal of the ACM 31:11-16 (1999) 1709-1723 https://doi.org/10.1145/324133.324139

Following the paper, this library provides 3 implementations of double-ended
queues which let you push, pop and append elements at both ends of the list in
worst-case constant time (strict! not amortized) :

| Module  | cons | uncons | snoc | unsnoc          | append          | rev             | nth                |
|---------|:----:|:------:|:----:|:---------------:|:---------------:|:---------------:|:------------------:|
| Dequeue | O(1) | O(1)   | O(1) | O(1)            | :no_entry_sign: | O(1)            | O(log min(i, N-i)) |
| Steque  | O(1) | O(1)   | O(1) | :no_entry_sign: | O(1)            | :no_entry_sign: | :no_entry_sign:    |
| Deck    | O(1) | O(1)   | O(1) | O(1)            | O(1)            |                 | :no_entry_sign:    |

Check out the [online documentation] for the full interface -- which should be
mostly compatible with OCaml's standard [List] module.

Example applications of these deques include:

- [ngrams.ml](examples/ngrams.ml) uses a sliding window to enumerate the ngrams
  of a string.
- [knuth_plass.ml](examples/knuth_plass.ml) implements the optimal line
  breaking algorithm of Knuth & Plass (of TeX fame) as described by Oege de
  Moor and Jeremy Gibbons in [Bridging the algorithm gap: A linear-time
  functional program for paragraph formatting].
- [string_builder.ml](examples/string_builder.ml) shows how to improve the
  asymptotic of a monoidal "concat" operator, a common design pattern in purely
  functional libraries. Surprisingly, this benchmark reveals that the simpler
  [difference lists] may exhibit weird edge cases in addition to being less
  flexible. See [Reflection without remorse] by Atze van der Ploeg and Oleg
  Kiselyov for a less obvious application to monadic computations.
- [zipper.ml](examples/zipper.ml) is a classic zipper to iterate over an
  ordered collection, with the added benefit that one can instantly close the
  traversal in `O(1)` rather than `O(length traversed)`. Such a zipper is a
  prerequisite for Brodal's [Fast Join-trees] and the [Functional Link-Cut
  trees] of Erik Demaine.
- Finally, these deques are suitable for marshalling and serialization as they
  do not use lazy or functional values in their spine.

---

None of this code would have been possible without the fantastic support for
GADTs in OCaml. The invariants are encoded inside each datatypes: the
algorithms then follow, guided by the type checker and the lack of recursion.
As this does not result in the most readable code, you should read the paper if
you want to understand the big ideas:

- [Skewed number systems] to deamortize carry propagation
- Recursive slowdown and bootstrapping
- Datastructures with a hole / with a preferred path to define `O(1)` fingers
  inside a purely functional tree

The core types and algorithms described in the paper can be found in the
`src/*_internal.ml` files.

[Purely Functional, Real-Time Deques with Catenation]: http://www.cs.tau.ac.il/~haimk/papers/jacm-deq.ps
[online documentation]: https://art-w.github.io/deque/deque/Deque
[List]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
[Skewed number systems]: https://en.wikipedia.org/wiki/Skew_binary_number_system
[Bridging the algorithm gap: A linear-time functional program for paragraph formatting]: https://doi.org/10.1016/S0167-6423(99)00005-2
[difference lists]: https://en.wikipedia.org/wiki/Difference_list
[Reflection without remorse]: https://doi.org/10.1145/2775050.2633360
[Fast Join-trees]: https://doi.org/10.1007/11841036_18
[Functional Link-Cut trees]: http://erikdemaine.org/papers/ConfluentTries_Algorithmica/
