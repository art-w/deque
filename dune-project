(lang dune 2.8)
(generate_opam_files true)

(name deque)
(source (github art-w/deque))
(license MIT)
(authors "Arthur Wendling")
(maintainers "art.wendling@gmail.com")
(version 0.1)

(package
  (name deque)
  (synopsis "Purely Functional, Real-Time Deques with Catenation")
  (depends (ocaml (>= "4.08")))
  (description
    "\
A double-ended queue is an ordered collection for which elements can be \
added and removed from the front and the back. This library provides a \
purely functional implementation, such that the following operations are \
all worst-case constant time:
- `cons` adds an element to the front of the deque
- `uncons` pops the left-most element
- `snoc` adds an element at the back
- `unsnoc` pops the right-most element
- `append` concatenates two deques
This datastructure was invented by Kaplan and Tarjan.")
)
