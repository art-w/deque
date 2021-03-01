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
  (depends (ocaml (>= "4.08"))))
