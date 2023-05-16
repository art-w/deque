type zero = ZERO
type 'a succ = SUCC of 'a

module Elt = struct
  type ('a, 'b) t = Elt : 'x -> ('size * ('x -> 'xs) * 'ys, 'size succ * 'xs * ('ys -> 'x)) t
end

module D = Dequeue_arrow.Make (Elt)

open Elt

type a = A
type b = B
type c = C
type d = D
type e = E
type nil = NIL

let test : (zero * _ * nil, _ * nil * _) D.t =
  D.snoc
    (D.snoc
      (D.cons (Elt A)
        (D.cons (Elt B)
          (D.snoc
            D.empty
            (Elt C))))
      (Elt D))
    (Elt E)

let () =
  let D.Uncons (Elt A, test) = D.uncons test in
  let D.Unsnoc (test, Elt E) = D.unsnoc test in
  let D.Uncons (Elt B, test) = D.uncons test in
  let D.Unsnoc (test, Elt D) = D.unsnoc test in
  let D.Uncons (Elt C, test) = D.uncons test in
  match D.uncons test with
  | D.Uncons_empty -> ()
  | D.Uncons (Elt D, test) -> (* absurd! *)
      let D.Unsnoc (test, Elt C) = D.unsnoc test in
      let D.Unsnoc (test, Elt B) = D.unsnoc test in
      let D.Unsnoc (test, Elt A) = D.unsnoc test in
      (match D.unsnoc test with D.Unsnoc _ -> .)
