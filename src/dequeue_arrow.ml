module type ELEMENT = sig
  type ('a, 'b) t
end

module Make (E : ELEMENT) : sig

  type ('a, 'b) elt = ('a, 'b) E.t
  type ('a, 'b) t

  val empty : ('a, 'a) t
  val cons : ('a, 'b) elt -> ('b, 'c) t -> ('a, 'c) t
  val snoc : ('a, 'b) t -> ('b, 'c) elt -> ('a, 'c) t

  type ('a0, 'a2) uncons =
    | Uncons_empty : ('a0, 'a0) uncons
    | Uncons : ('a0, 'a1) elt * ('a1, 'a2) t -> ('a0, 'a2) uncons

  val uncons : ('a, 'b) t -> ('a, 'b) uncons

  type ('a0, 'a2) unsnoc =
    | Unsnoc_empty : ('a0, 'a0) unsnoc
    | Unsnoc : ('a0, 'a1) t * ('a1, 'a2) elt -> ('a0, 'a2) unsnoc

  val unsnoc : ('a, 'b) t -> ('a, 'b) unsnoc

end = struct

type ('a, 'b) elt = ('a, 'b) E.t

type zero = ZERO [@@ocaml.warning "-37"]
type 'a succ = SUCC of 'a [@@ocaml.warning "-37"]

type ('a, 'b, 'h) pair =
  | Elt : ('a, 'b) elt -> ('a, 'b, zero) pair
  | Pair : ('a, 'b, 'h) pair * ('b, 'c, 'h) pair -> ('a, 'c, 'h succ) pair

type ('a, 'b, 'h, 'color) buffer =
  | B0 : ('a, 'a, 'h, [`red]) buffer
  | B1 : ('a0, 'a1, 'h) pair -> ('a0, 'a1, 'h, [`yellow]) buffer
  | B2 : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h) pair -> ('a0, 'a2, 'h, [< `green | `yellow]) buffer
  | B3 : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h) pair * ('a2, 'a3, 'h) pair -> ('a0, 'a3, 'h, [< `green | `yellow]) buffer
  | B4 : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h) pair * ('a2, 'a3, 'h) pair * ('a3, 'a4, 'h) pair -> ('a0, 'a4, 'h, [`yellow]) buffer
  | B5 : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h) pair * ('a2, 'a3, 'h) pair * ('a3, 'a4, 'h) pair * ('a4, 'a5, 'h) pair -> ('a0, 'a5, 'h, [`red]) buffer

type ('a0, 'a1, 'h_in, 'h_out, 'b0, 'b1, 'color) deque =
  | HOLE : ('a0, 'a1, 'h, 'h, 'a0, 'a1, [`kont]) deque

  | Yellow : ('a0, 'a1, 'h, [< `green | `yellow]) buffer
           * ('a1, 'a2, 'h succ, 'h_out, 'b0, 'b1, [< `yellow | `kont]) deque
           * ('a2, 'a3, 'h, [< `green | `yellow]) buffer
          -> ('a0, 'a3, 'h, 'h_out, 'b0, 'b1, [`yellow]) deque

  | Green : ('a0, 'a1, 'h, [`green]) buffer
          * ('a1, 'a2, 'h succ, 'h_out, 'b0, 'b1, [< `yellow | `kont]) deque
          * ('a2, 'a3, 'h, [`green]) buffer
         -> ('a0, 'a3, 'h, 'h_out, 'b0, 'b1, [`green]) deque

  | Red : ('a0, 'a1, 'h, [< `green | `yellow | `red]) buffer
        * ('a1, 'a2, 'h succ, 'h_out, 'b0, 'b1, [< `yellow | `kont]) deque
        * ('a2, 'a3, 'h, [< `green | `yellow | `red]) buffer
       -> ('a0, 'a3, 'h, 'h_out, 'b0, 'b1, [`red]) deque

type ('a, 'b, 'h, 'color) kont =
  | Small : ('a0, 'a1, 'h, _) buffer -> ('a0, 'a1, 'h, [`green]) kont
  | G : ('a0, 'a1, 'h, 'h_out, 'b0, 'b1, [`green ]) deque
      * ('b0, 'b1, 'h_out, [< `green | `red]) kont
     -> ('a0, 'a1, 'h, [`green ]) kont
  | Y : ('a0, 'a1, 'h, 'h_out, 'b0, 'b1, [`yellow]) deque
      * ('b0, 'b1, 'h_out, [`green]) kont
     -> ('a0, 'a1, 'h, [`yellow]) kont
  | R : ('a0, 'a1, 'h, 'h_out, 'b0, 'b1, [`red]) deque
      * ('b0, 'b1, 'h_out, [`green]) kont
     -> ('a0, 'a1, 'h, [`red]) kont

type ('a0, 'a1) t = T : ('a0, 'a1, zero, [< `green | `yellow]) kont -> ('a0, 'a1) t

let empty = T (Small B0)

type ('a0, 'a1, 'h) yellow_buffer =
  Yellowish : ('a0, 'a1, 'h, [< `green | `yellow]) buffer -> ('a0, 'a1, 'h) yellow_buffer

type ('a0, 'a1, 'h) any_buffer =
  Any : ('a0, 'a1, 'h, [< `green | `yellow | `red ]) buffer -> ('a0, 'a1, 'h) any_buffer

let green_prefix_cons
: type a0 a1 a2 h. (a0, a1, h) pair -> (a1, a2, h, [`green]) buffer -> (a0, a2, h, [`yellow]) buffer
= fun x buf ->
  match buf with
  | B2 (a, b) -> B3 (x, a, b)
  | B3 (a, b, c) -> B4 (x, a, b, c)

let green_suffix_snoc
: type a0 a1 a2 h. (a0, a1, h, [`green]) buffer -> (a1, a2, h) pair -> (a0, a2, h, [`yellow]) buffer
= fun buf x ->
  match buf with
  | B2 (a, b)    -> B3 (a, b, x)
  | B3 (a, b, c) -> B4 (a, b, c, x)

let yellow_prefix_cons
: type a0 a1 a2 h. (a0, a1, h) pair -> (a1, a2, h) yellow_buffer -> (a0, a2, h) any_buffer
= fun x (Yellowish buf) ->
  match buf with
  | B1 a -> Any (B2 (x, a))
  | B2 (a, b) -> Any (B3 (x, a, b))
  | B3 (a, b, c) -> Any (B4 (x, a, b, c))
  | B4 (a, b, c, d) -> Any (B5 (x, a, b, c, d))

let yellow_suffix_snoc
: type a0 a1 a2 h. (a0, a1, h) yellow_buffer -> (a1, a2, h) pair -> (a0, a2, h) any_buffer
= fun (Yellowish buf) x ->
  match buf with
  | B1 a -> Any (B2 (a, x))
  | B2 (a, b) -> Any (B3 (a, b, x))
  | B3 (a, b, c) -> Any (B4 (a, b, c, x))
  | B4 (a, b, c, d) -> Any (B5 (a, b, c, d, x))

let buffer_cons
: type a0 a1 a2 h c. (a0, a1, h) pair -> (a1, a2, h, c) buffer -> (a0, a2, h, [`green]) kont
= fun x buf ->
  match buf with
  | B0 -> Small (B1 x)
  | B1 a -> Small (B2 (x, a))
  | B2 (a, b) -> Small (B3 (x, a, b))
  | B3 (a, b, c) -> Small (B4 (x, a, b, c))
  | B4 (a, b, c, d) -> Small (B5 (x, a, b, c, d))
  | B5 (a, b, c, d, e) ->
      G (Green (B3 (x, a, b), HOLE, B3 (c, d, e)), Small B0)

let buffer_snoc
: type a0 a1 a2 h c. (a0, a1, h, c) buffer -> (a1, a2, h) pair -> (a0, a2, h, [`green]) kont
= fun buf x ->
  match buf with
  | B0 -> Small (B1 x)
  | B1 a -> Small (B2 (a, x))
  | B2 (a, b) -> Small (B3 (a, b, x))
  | B3 (a, b, c) -> Small (B4 (a, b, c, x))
  | B4 (a, b, c, d) -> Small (B5 (a, b, c, d, x))
  | B5 (a, b, c, d, e) ->
      G (Green (B3 (a, b, c), HOLE, B3 (d, e, x)), Small B0)

type (_, _, _) green_uncons = Green_uncons : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h) yellow_buffer -> ('a0, 'a2, 'h) green_uncons

let green_uncons
: type a0 a1 h. (a0, a1, h, [`green]) buffer -> (a0, a1, h) green_uncons
= function
  | B2 (a, b)    -> Green_uncons (a, Yellowish (B1 b))
  | B3 (a, b, c) -> Green_uncons (a, Yellowish (B2 (b, c)))

type (_, _, _) green_unsnoc = Green_unsnoc : ('a0, 'a1, 'h) yellow_buffer * ('a1, 'a2, 'h) pair -> ('a0, 'a2, 'h) green_unsnoc

let green_unsnoc
: type a0 a1 h. (a0, a1, h, [`green]) buffer -> (a0, a1, h) green_unsnoc
= function
  | B2 (a, b)    -> Green_unsnoc (Yellowish (B1 a), b)
  | B3 (a, b, c) -> Green_unsnoc (Yellowish (B2 (a, b)), c)

type (_, _, _) yellow_uncons = Yellow_uncons : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h) any_buffer -> ('a0, 'a2, 'h) yellow_uncons

let yellow_uncons : type a0 a1 h. (a0, a1, h) yellow_buffer -> (a0, a1, h) yellow_uncons
= fun (Yellowish buf) ->
  match buf with
  | B1 a            -> Yellow_uncons (a, Any B0)
  | B2 (a, b)       -> Yellow_uncons (a, Any (B1  b))
  | B3 (a, b, c)    -> Yellow_uncons (a, Any (B2 (b, c)))
  | B4 (a, b, c, d) -> Yellow_uncons (a, Any (B3 (b, c, d)))

type (_, _, _) yellow_unsnoc = Yellow_unsnoc : ('a0, 'a1, 'h) any_buffer * ('a1, 'a2, 'h) pair -> ('a0, 'a2, 'h) yellow_unsnoc

let yellow_unsnoc : type a0 a1 h. (a0, a1, h) yellow_buffer -> (a0, a1, h) yellow_unsnoc
= fun (Yellowish buf) ->
  match buf with
  | B1 a            -> Yellow_unsnoc (Any B0,             a)
  | B2 (a, b)       -> Yellow_unsnoc (Any (B1  a),        b)
  | B3 (a, b, c)    -> Yellow_unsnoc (Any (B2 (a, b)),    c)
  | B4 (a, b, c, d) -> Yellow_unsnoc (Any (B3 (a, b, c)), d)

type ('a0, 'a1, 'h) buffer_uncons =
  | Buffer_uncons_empty : ('a0, 'a0, 'h) buffer_uncons
  | Buffer_uncons : ('a0, 'a1, 'h) yellow_uncons -> ('a0, 'a1, 'h) buffer_uncons

let buffer_uncons : type a0 a1 h c. (a0, a1, h, c) buffer -> (a0, a1, h) buffer_uncons
= function
  | B0  -> Buffer_uncons_empty
  | (B1 _) as buf -> Buffer_uncons (yellow_uncons (Yellowish buf))
  | (B2 _) as buf -> Buffer_uncons (yellow_uncons (Yellowish buf))
  | (B3 _) as buf -> Buffer_uncons (yellow_uncons (Yellowish buf))
  | (B4 _) as buf -> Buffer_uncons (yellow_uncons (Yellowish buf))
  | B5 (a, b, c, d, e) -> Buffer_uncons (Yellow_uncons (a, Any (B4 (b, c, d, e))))

type ('a0, 'a1, 'h) buffer_unsnoc =
  | Buffer_unsnoc_empty : ('a0, 'a0, 'h) buffer_unsnoc
  | Buffer_unsnoc : ('a0, 'a1, 'h) yellow_unsnoc -> ('a0, 'a1, 'h) buffer_unsnoc

let buffer_unsnoc : type a0 a1 h c. (a0, a1, h, c) buffer -> (a0, a1, h) buffer_unsnoc
= function
  | B0  -> Buffer_unsnoc_empty
  | (B1 _) as buf -> Buffer_unsnoc (yellow_unsnoc (Yellowish buf))
  | (B2 _) as buf -> Buffer_unsnoc (yellow_unsnoc (Yellowish buf))
  | (B3 _) as buf -> Buffer_unsnoc (yellow_unsnoc (Yellowish buf))
  | (B4 _) as buf -> Buffer_unsnoc (yellow_unsnoc (Yellowish buf))
  | B5 (a, b, c, d, e) -> Buffer_unsnoc (Yellow_unsnoc (Any (B4 (a, b, c, d)), e))

type ('a0, 'a2, 'h, 'c) prefix_rot = Prefix_rot : ('a0, 'a1, 'h, 'c) buffer * ('a1, 'a2, 'h) pair -> ('a0, 'a2, 'h, 'c) prefix_rot

let prefix_rot
: type a0 a1 a2 h c. (a0, a1, h) pair -> (a1, a2, h, c) buffer -> (a0, a2, h, c) prefix_rot
= fun x buf -> match buf with
  | B0                 -> Prefix_rot (B0, x)
  | B1 a               -> Prefix_rot (B1  x, a)
  | B2 (a, b)          -> Prefix_rot (B2 (x, a), b)
  | B3 (a, b, c)       -> Prefix_rot (B3 (x, a, b), c)
  | B4 (a, b, c, d)    -> Prefix_rot (B4 (x, a, b, c), d)
  | B5 (a, b, c, d, e) -> Prefix_rot (B5 (x, a, b, c, d), e)

type ('a0, 'a2, 'h, 'c) suffix_rot = Suffix_rot : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h, 'c) buffer -> ('a0, 'a2, 'h, 'c) suffix_rot

let suffix_rot
: type a0 a1 a2 h c. (a0, a1, h, c) buffer -> (a1, a2, h) pair -> (a0, a2, h, c) suffix_rot
= fun buf x -> match buf with
  | B0                 -> Suffix_rot (x, B0)
  | B1 a               -> Suffix_rot (a, B1 x)
  | B2 (a, b)          -> Suffix_rot (a, B2 (b, x))
  | B3 (a, b, c)       -> Suffix_rot (a, B3 (b, c, x))
  | B4 (a, b, c, d)    -> Suffix_rot (a, B4 (b, c, d, x))
  | B5 (a, b, c, d, e) -> Suffix_rot (a, B5 (b, c, d, e, x))


type ('a0, 'a1, 'h) prefix_decompose =
  | Prefix_none      : ('a0, 'a0, 'h) prefix_decompose
  | Prefix_underflow : ('a0, 'a1, 'h) pair -> ('a0, 'a1, 'h) prefix_decompose
  | Prefix_ok        : ('a0, 'a1, 'h, [`green]) buffer -> ('a0, 'a1, 'h) prefix_decompose
  | Prefix_overflow  : ('a0, 'a1, 'h, [`green]) buffer * ('a1, 'a2, 'h succ) pair -> ('a0, 'a2, 'h) prefix_decompose

let prefix_decompose : type a0 a1 h c. (a0, a1, h, c) buffer -> (a0, a1, h) prefix_decompose
= function
  | B0   -> Prefix_none
  | B1 x -> Prefix_underflow x
  | B2 (a, b) -> Prefix_ok (B2 (a, b))
  | B3 (a, b, c) -> Prefix_ok (B3 (a, b, c))
  | B4 (a, b, c, d) -> Prefix_overflow (B2 (a, b), Pair (c, d))
  | B5 (a, b, c, d, e) -> Prefix_overflow (B3 (a, b, c), Pair (d, e))

type ('a0, 'a1, 'h) suffix_decompose =
  | Suffix_none : ('a0, 'a0, 'h) suffix_decompose
  | Suffix_underflow : ('a0, 'a1, 'h) pair -> ('a0, 'a1, 'h) suffix_decompose
  | Suffix_ok        : ('a0, 'a1, 'h, [`green]) buffer -> ('a0, 'a1, 'h) suffix_decompose
  | Suffix_overflow  : ('a0, 'a1, 'h succ) pair * ('a1, 'a2, 'h, [`green]) buffer -> ('a0, 'a2, 'h) suffix_decompose

let suffix_decompose : type a0 a1 h c. (a0, a1, h, c) buffer -> (a0, a1, h) suffix_decompose
= function
  | B0   -> Suffix_none
  | B1 x -> Suffix_underflow x
  | B2 (a, b) -> Suffix_ok (B2 (a, b))
  | B3 (a, b, c) -> Suffix_ok (B3 (a, b, c))
  | B4 (a, b, c, d) -> Suffix_overflow (Pair (a, b), B2 (c, d))
  | B5 (a, b, c, d, e) -> Suffix_overflow (Pair (a, b), B3 (c, d, e))

type ('a0, 'a2, 'h) green_prefix_concat =
  Green_prefix_concat : ('a0, 'a1, 'h, [`green]) buffer * ('a1, 'a2, 'h succ) yellow_buffer -> ('a0, 'a2, 'h) green_prefix_concat

let green_prefix_concat
: type a0 a1 a2 h c.
     (a0, a1, h, c) buffer
  -> (a1, a2, h succ, [`green]) buffer
  -> (a0, a2, h) green_prefix_concat
= fun buf1 buf2 ->
  match prefix_decompose buf1 with
  | Prefix_ok buf1 -> Green_prefix_concat (buf1, Yellowish buf2)
  | Prefix_none ->
      let Green_uncons (Pair (a, b), buf2) = green_uncons buf2 in
      Green_prefix_concat (B2 (a, b), buf2)
  | Prefix_underflow a ->
      let Green_uncons (Pair (b, c), buf2) = green_uncons buf2 in
      Green_prefix_concat (B3 (a, b, c), buf2)
  | Prefix_overflow (buf1, ab) ->
      Green_prefix_concat (buf1, Yellowish (green_prefix_cons ab buf2))

type ('a0, 'a2, 'h) green_suffix_concat =
  Green_suffix_concat : ('a0, 'a1, 'h succ) yellow_buffer * ('a1, 'a2, 'h, [`green]) buffer -> ('a0, 'a2, 'h) green_suffix_concat

let green_suffix_concat
: type a0 a1 a2 h c.
     (a0, a1, h succ, [`green]) buffer
  -> (a1, a2, h, c) buffer
  -> (a0, a2, h) green_suffix_concat
= fun buf1 buf2 ->
  match suffix_decompose buf2 with
  | Suffix_ok buf2 -> Green_suffix_concat (Yellowish buf1, buf2)
  | Suffix_none ->
      let Green_unsnoc (buf1, Pair (a, b)) = green_unsnoc buf1 in
      Green_suffix_concat (buf1, B2 (a, b))
  | Suffix_underflow c ->
      let Green_unsnoc (buf1, Pair (a, b)) = green_unsnoc buf1 in
      Green_suffix_concat (buf1, B3 (a, b, c))
  | Suffix_overflow (ab, buf2) ->
      Green_suffix_concat (Yellowish (green_suffix_snoc buf1 ab), buf2)

type ('a0, 'a2, 'h) prefix_concat = Prefix_concat : ('a0, 'a1, 'h, [`green]) buffer * ('a1, 'a2, 'h succ) any_buffer -> ('a0, 'a2, 'h) prefix_concat

let prefix_concat
: type a0 a1 a2 h c. (a0, a1, h, c) buffer -> (a1, a2, h succ) yellow_buffer -> (a0, a2, h) prefix_concat
= fun buf1 buf2 ->
  match prefix_decompose buf1 with
  | Prefix_ok buf1 ->
      let Yellowish buf2 = buf2 in
      Prefix_concat (buf1, Any buf2)
  | Prefix_none ->
      let Yellow_uncons (Pair (a, b), buf2) = yellow_uncons buf2 in
      Prefix_concat (B2 (a, b), buf2)
  | Prefix_underflow a ->
      let Yellow_uncons (Pair (b, c), buf2) = yellow_uncons buf2 in
      Prefix_concat (B3 (a, b, c), buf2)
  | Prefix_overflow (buf1, ab) ->
      Prefix_concat (buf1, yellow_prefix_cons ab buf2)

type ('a0, 'a2, 'h) suffix_concat = Suffix_concat : ('a0, 'a1, 'h succ) any_buffer * ('a1, 'a2, 'h, [`green]) buffer -> ('a0, 'a2, 'h) suffix_concat

let suffix_concat
: type a0 a1 a2 h c. (a0, a1, h succ) yellow_buffer -> (a1, a2, h, c) buffer -> (a0, a2, h) suffix_concat
= fun buf1 buf2 ->
  match suffix_decompose buf2 with
  | Suffix_ok buf2 ->
      let Yellowish buf1 = buf1 in
      Suffix_concat (Any buf1, buf2)
  | Suffix_none ->
      let Yellow_unsnoc (buf1, Pair (a, b)) = yellow_unsnoc buf1 in
      Suffix_concat (buf1, B2 (a, b))
  | Suffix_underflow c ->
      let Yellow_unsnoc (buf1, Pair (a, b)) = yellow_unsnoc buf1 in
      Suffix_concat (buf1, B3 (a, b, c))
  | Suffix_overflow (ab, buf2) ->
      Suffix_concat (yellow_suffix_snoc buf1 ab, buf2)


type ('a0, 'a1, 'h) sandwich =
  | Sandwich_empty : ('a0, 'a0, 'h) sandwich
  | Sandwich_alone : ('a0, 'a1, 'h) pair -> ('a0, 'a1, 'h) sandwich
  | Sandwich : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h, _) buffer * ('a2, 'a3, 'h) pair -> ('a0, 'a3, 'h) sandwich

let buffer_unsandwich : type a0 a1 h c. (a0, a1, h, c) buffer -> (a0, a1, h) sandwich
= function
  | B0 -> Sandwich_empty
  | B1 a -> Sandwich_alone a
  | B2 (a, b) -> Sandwich (a, B0, b)
  | B3 (a, b, c) -> Sandwich (a, B1 b, c)
  | B4 (a, b, c, d) -> Sandwich (a, B2 (b, c), d)
  | B5 (a, b, c, d, e) -> Sandwich (a, B3 (b, c, d), e)

type ('a0, 'a1, 'h) buffer_halve =
  | Buffer_halve_even : ('a0, 'a1, 'h succ) any_buffer -> ('a0, 'a1, 'h) buffer_halve
  | Buffer_halve_odd : ('a0, 'a1, 'h) pair * ('a1, 'a2, 'h succ) any_buffer -> ('a0, 'a2, 'h) buffer_halve

let buffer_halve : type a0 a1 h c. (a0, a1, h, c) buffer -> (a0, a1, h) buffer_halve
= function
  | B0                 -> Buffer_halve_even (Any B0)
  | B1 a               -> Buffer_halve_odd (a, Any B0)
  | B2 (a, b)          -> Buffer_halve_even (Any (B1 (Pair (a, b))))
  | B3 (a, b, c)       -> Buffer_halve_odd (a, Any (B1 (Pair (b, c))))
  | B4 (a, b, c, d)    -> Buffer_halve_even (Any (B2 (Pair (a, b), Pair (c, d))))
  | B5 (a, b, c, d, e) -> Buffer_halve_odd (a, Any (B2 (Pair (b, c), Pair (d, e))))

let make_small
: type a0 a1 a2 a3 h c0 c1 c2.
     (a0, a1, h, c0) buffer
  -> (a1, a2, h succ, c1) buffer
  -> (a2, a3, h, c2) buffer
  -> (a0, a3, h, [`green]) kont
= fun prefix1 buf suffix1 ->
  match prefix_decompose prefix1, suffix_decompose suffix1 with
  | Prefix_ok p1, Suffix_ok s1 ->
      G (Green (p1, HOLE, s1), Small buf)

  | Prefix_ok p1, Suffix_none ->
      begin match buffer_unsnoc buf with
      | Buffer_unsnoc_empty -> Small p1
      | Buffer_unsnoc (Yellow_unsnoc (Any rest, Pair (c, d))) ->
          G (Green (p1, HOLE, B2 (c, d)), Small rest)
      end

  | Prefix_ok p1, Suffix_underflow e ->
      begin match buffer_unsnoc buf with
      | Buffer_unsnoc_empty -> buffer_snoc p1 e
      | Buffer_unsnoc (Yellow_unsnoc (Any rest, Pair (c, d))) ->
          G (Green (p1, HOLE, B3 (c, d, e)), Small rest)
      end

  | Prefix_none, Suffix_ok s1 ->
      begin match buffer_uncons buf with
      | Buffer_uncons_empty -> Small s1
      | Buffer_uncons (Yellow_uncons (Pair (c, d), Any rest)) ->
          G (Green (B2 (c, d), HOLE, s1), Small rest)
      end

  | Prefix_underflow b, Suffix_ok s1 ->
      begin match buffer_uncons buf with
      | Buffer_uncons_empty -> buffer_cons b s1
      | Buffer_uncons (Yellow_uncons (Pair (c, d), Any rest)) ->
          G (Green (B3 (b, c, d), HOLE, s1), Small rest)
      end

  | Prefix_none, Suffix_none ->
      begin match buffer_unsandwich buf with
      | Sandwich (Pair (a, b), rest, Pair (c, d)) ->
          G (Green (B2 (a, b), HOLE, B2 (c, d)), Small rest)
      | Sandwich_alone (Pair (a, b)) -> Small (B2 (a, b))
      | Sandwich_empty -> Small B0
      end

  | Prefix_none, Suffix_underflow e ->
      begin match buffer_unsandwich buf with
      | Sandwich (Pair (a, b), rest, Pair (c, d)) ->
          G (Green (B2 (a, b), HOLE, B3 (c, d, e)), Small rest)
      | Sandwich_alone (Pair (c, d)) -> Small (B3 (c, d, e))
      | Sandwich_empty -> Small (B1 e)
      end

  | Prefix_underflow a, Suffix_none ->
      begin match buffer_unsandwich buf with
      | Sandwich (Pair (b, c), rest, Pair (d, e)) ->
          G (Green (B3 (a, b, c), HOLE, B2 (d, e)), Small rest)
      | Sandwich_alone (Pair (b, c)) -> Small (B3 (a, b, c))
      | Sandwich_empty -> Small (B1 a)
      end

  | Prefix_underflow a, Suffix_underflow f ->
      begin match buffer_unsandwich buf with
      | Sandwich (Pair (b, c), rest, Pair (d, e)) ->
          G (Green (B3 (a, b, c), HOLE, B3 (d, e, f)), Small rest)
      | Sandwich_alone (Pair (b, c)) -> Small (B4 (a, b, c, f))
      | Sandwich_empty -> Small (B2 (a, f))
      end

  | Prefix_overflow (p1, ab), Suffix_ok s1 ->
      let buf = buffer_cons ab buf in
      G (Green (p1, HOLE, s1), buf)

  | Prefix_ok p1, Suffix_overflow (ab, s1) ->
      let buf = buffer_snoc buf ab in
      G (Green (p1, HOLE, s1), buf)

  | Prefix_none, Suffix_overflow (ab, s1) ->
      let Suffix_rot (Pair (c, d), center) = suffix_rot buf ab in
      G (Green (B2 (c, d), HOLE, s1), Small center)

  | Prefix_underflow a, Suffix_overflow (bc, s1) ->
      let Suffix_rot (Pair (b, c), center) = suffix_rot buf bc in
      G (Green (B3 (a, b, c), HOLE, s1), Small center)

  | Prefix_overflow (p1, ab), Suffix_none ->
      let Prefix_rot (center, Pair (c, d)) = prefix_rot ab buf in
      G (Green (p1, HOLE, B2 (c, d)), Small center)

  | Prefix_overflow (p1, ab), Suffix_underflow e ->
      let Prefix_rot (center, Pair (c, d)) = prefix_rot ab buf in
      G (Green (p1, HOLE, B3 (c, d, e)), Small center)

  | Prefix_overflow (p1, ab), Suffix_overflow (cd, s1) ->
      begin match buffer_halve buf with
      | Buffer_halve_even (Any rest) ->
          G (Green (p1, Yellow (B1 ab, HOLE, B1 cd), s1), Small rest)
      | Buffer_halve_odd (x, Any rest) ->
          G (Green (p1, Yellow (B2 (ab, x), HOLE, B1 cd), s1), Small rest)
      end

let green_of_red
: type a0 a1 h. (a0, a1, h, [`red]) kont -> (a0, a1, h, [`green]) kont
= function
  | R (Red (p1, HOLE, s1), Small buf) ->
      make_small p1 buf s1
  | R (Red (p1, Yellow (p2, child, s2), s1), kont) ->
      let Prefix_concat (p1, Any p2) = prefix_concat p1 (Yellowish p2) in
      let Suffix_concat (Any s2, s1) = suffix_concat (Yellowish s2) s1 in
      G (Green (p1, HOLE, s1), R (Red (p2, child, s2), kont))
  | R (Red (p1, HOLE, s1), G (Green (p2, child, s2), kont)) ->
      let Green_prefix_concat (p1, Yellowish p2) = green_prefix_concat p1 p2 in
      let Green_suffix_concat (Yellowish s2, s1) = green_suffix_concat s2 s1 in
      G (Green (p1, Yellow (p2, child, s2), s1), kont)

type _ not_yellow = Not_yellow: [< `green | `red] not_yellow

let ensure_green
: type a0 a1 h c. c not_yellow -> (a0, a1, h, c) kont -> (a0, a1, h, [`green]) kont
= fun Not_yellow t ->
  match t with
  | Small buf -> Small buf
  | G (x, k) -> G (x, k)
  | R (x, k) -> green_of_red (R (x, k))

let yellow p1 child s1 kont =
  T (Y (Yellow (p1, child, s1), ensure_green Not_yellow kont))

let red p1 child s1 kont =
  T (green_of_red (R (Red (p1, child, s1), kont)))


let cons x (T t) = match t with
  | Small buf -> T (buffer_cons x buf)
  | G (Green (p1, child, s1), kont) ->
      let p1 = green_prefix_cons x p1 in
      yellow p1 child s1 kont
  | Y (Yellow (p1, child, s1), kont) ->
      let Any p1 = yellow_prefix_cons x (Yellowish p1) in
      red p1 child s1 kont

let cons x t = cons (Elt x) t

let snoc (T t) x = match t with
  | Small buf -> T (buffer_snoc buf x)
  | G (Green (p1, child, s1), kont) ->
      let s1 = green_suffix_snoc s1 x in
      yellow p1 child s1 kont
  | Y (Yellow (p1, child, s1), kont) ->
      let Any s1 = yellow_suffix_snoc (Yellowish s1) x in
      red p1 child s1 kont

let snoc t x = snoc t (Elt x)

type ('a0, 'a2) uncons =
  | Uncons_empty : ('a0, 'a0) uncons
  | Uncons : ('a0, 'a1) elt * ('a1, 'a2) t -> ('a0, 'a2) uncons

let uncons : type a0 a1. (a0, a1) t -> (a0, a1) uncons
= fun (T t) -> match t with
  | Small buf ->
      begin match buffer_uncons buf with
      | Buffer_uncons_empty -> Uncons_empty
      | Buffer_uncons (Yellow_uncons (Elt x, Any buf)) -> Uncons (x, T (Small buf))
      end
  | G (Green (p1, child, s1), kont) ->
      let Green_uncons (Elt x, Yellowish p1) = green_uncons p1 in
      Uncons (x, yellow p1 child s1 kont)
  | Y (Yellow (p1, child, s1), kont) ->
      let Yellow_uncons (Elt x, Any p1) = yellow_uncons (Yellowish p1) in
      Uncons (x, red p1 child s1 kont)

type ('a0, 'a2) unsnoc =
  | Unsnoc_empty : ('a0, 'a0) unsnoc
  | Unsnoc : ('a0, 'a1) t * ('a1, 'a2) elt -> ('a0, 'a2) unsnoc

let unsnoc : type a0 a1. (a0, a1) t -> (a0, a1) unsnoc
= fun (T t) -> match t with
  | Small buf ->
      begin match buffer_unsnoc buf with
      | Buffer_unsnoc_empty -> Unsnoc_empty
      | Buffer_unsnoc (Yellow_unsnoc (Any buf, Elt x)) -> Unsnoc (T (Small buf), x)
      end
  | G (Green (p1, child, s1), kont) ->
      let Green_unsnoc (Yellowish s1, Elt x) = green_unsnoc s1 in
      Unsnoc (yellow p1 child s1 kont, x)
  | Y (Yellow (p1, child, s1), kont) ->
      let Yellow_unsnoc (Any s1, Elt x) = yellow_unsnoc (Yellowish s1) in
      Unsnoc (red p1 child s1 kont, x)

end
