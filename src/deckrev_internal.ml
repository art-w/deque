module Deq = Dequeue_internal

open Deq.ColorsGYRO

type 'a pr = out_channel -> 'a -> unit

type nh_ = NOT_HOLE
type nh = nh_ * nh_
type hole = HOLE
type has_hole = HAS_HOLE

type r0 = R0
type r1 = R1
type not_rev = r0 * r1
type is_rev = r1 * r0

type only_  = ONLY
type left_  = LEFT
type right_ = RIGHT

type only   = only_ * only_
type left   = left_ * right_
type right  = right_ * left_

type preferred_left  = PREFERRED_LEFT
type preferred_right = PREFERRED_RIGHT

type    z = ZERO
type 'a s = SUCC

type 'a ge1 = 'a s
type 'a ge2 = 'a s s
type 'a ge3 = 'a s s s
type 'a ge4 = 'a s s s s
type 'a ge5 = 'a s s s s s
type 'a ge6 = 'a s s s s s s
type 'a ge7 = 'a s s s s s s s
type 'a ge8 = 'a s s s s s s s s

type eq1  = z s
type eq2  = z ge2
type eq6  = z ge6

type 'a four  = 'a * 'a * 'a * 'a
type 'a five  = 'a * 'a * 'a * 'a * 'a
type 'a six   = 'a * 'a * 'a * 'a * 'a * 'a
type 'a eight = 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a

type ('a, 'upperbound) vector =
  | V0 : ('a, 'n) vector
  | V1 : 'a -> ('a, 'n ge1) vector
  | V2 : 'a * 'a -> ('a, 'n ge2) vector
  | V3 : 'a * 'a * 'a -> ('a, 'n ge3) vector
  | V4 : 'a * 'a * 'a * 'a -> ('a, 'n ge4) vector
  | V5 : 'a * 'a * 'a * 'a * 'a -> ('a, 'n ge5) vector
  | V6 : 'a * 'a * 'a * 'a * 'a * 'a -> ('a, 'n ge6) vector

let vector_fold_right
: type z a n. (a -> z -> z) -> (a, n) vector -> z -> z
= fun fn v z -> match v with
  | V0 -> z
  | V1 a -> fn a z
  | V2 (a, b) -> fn a (fn b z)
  | V3 (a, b, c) -> fn a (fn b (fn c z))
  | V4 (a, b, c, d) -> fn a (fn b (fn c (fn d z)))
  | V5 (a, b, c, d, e) -> fn a (fn b (fn c (fn d (fn e z))))
  | V6 (a, b, c, d, e, f) -> fn a (fn b (fn c (fn d (fn e (fn f z)))))

let vector_fold_left
: type z a n. (z -> a -> z) -> z -> (a, n) vector -> z
= fun fn z v -> match v with
  | V0 -> z
  | V1 a -> fn z a
  | V2 (a, b) -> fn (fn z a) b
  | V3 (a, b, c) -> fn (fn (fn z a) b) c
  | V4 (a, b, c, d) -> fn (fn (fn (fn z a) b) c) d
  | V5 (a, b, c, d, e) -> fn (fn (fn (fn (fn z a) b) c) d) e
  | V6 (a, b, c, d, e, f) -> fn (fn (fn (fn (fn (fn z a) b) c) d) e) f

module Buffer : sig
  type 'a elt = L2R of 'a | R2L of 'a
  type ('a, 'n) t

  val empty : ('a, z) t

  val rev : ('a, 'n) t -> ('a, 'n) t
  val is_rev : ('a, 'n) t -> bool

  val elt_out : 'a elt -> ('a, 'n) t -> 'a elt

  val cons : 'a elt -> ('a, 'n) t -> ('a, 'n s) t
  val snoc : ('a, 'n) t -> 'a elt -> ('a, 'n s) t

  val uncons : ('a, 'n s) t -> 'a elt * ('a, 'n) t
  val unsnoc : ('a, 'n s) t -> ('a, 'n) t * 'a elt

  val uncons2 : ('a, 'n s s) t -> 'a elt * 'a elt * ('a, 'n) t
  val unsnoc2 : ('a, 'n s s) t -> ('a, 'n) t * 'a elt * 'a elt

  val single_elt : 'a elt -> ('a, z s) t
  val triple : 'a elt -> 'a elt -> 'a elt -> ('a, z s s s) t

  val pair_elt : 'a elt -> 'a elt -> ('a, z s s) t

  val two : ('a, eq2) t -> 'a elt * 'a elt
  val cons2 : 'a elt * 'a elt -> ('a, 'n) t -> ('a, 'n s s) t
  val snoc2 : ('a, 'n) t -> 'a elt * 'a elt -> ('a, 'n s s) t

  val cons_5_vector : 'a elt five * ('a elt, _) vector
                   -> ('a, 'n) t
                   -> ('a, 'n ge5) t

  val snoc_5_vector : ('a, 'n) t
                   -> 'a elt five * ('a elt, _) vector
                   -> ('a, 'n ge5) t

  val cons6 : 'a elt six -> ('a, 'n) t -> ('a, 'n ge6) t
  val snoc6 : ('a, 'n) t -> 'a elt six -> ('a, 'n ge6) t

  val snoc8 : ('a, 'n) t -> 'a elt eight -> ('a, 'n ge8) t

  val cons_vector : ('a elt, _) vector -> ('a, 'n) t -> ('a, 'n) t
  val snoc_vector : ('a, 'n) t -> ('a elt, _) vector -> ('a, 'n) t

  type _ has1 =
    | Exact_0 : 'a has1
    | Lte1 : ('a, _ ge1) t -> 'a has1
  val has1 : ('a, 'n) t -> 'a has1

  val to_dequeue : ('a, _) t -> 'a elt Deq.t
  val of_dequeue : 'a elt Deq.t -> 'a has1

  type 'a has5 =
    | Exact_4 : 'a elt four -> 'a has5
    | At_least_5 : ('a, _ ge5) t -> 'a has5
  val has5   : ('a, _ ge4) t -> 'a has5

  type 'a has5p2 =
    | Less_than_5p2 : ('a elt, eq6) vector -> 'a has5p2
    | At_least_5p2 : ('a, _ ge5) t * 'a elt * 'a elt -> 'a has5p2
  val has5p2 : ('a, _ ge1) t -> 'a has5p2

  type 'a has2p5 =
    | Less_than_2p5 : ('a elt, eq6) vector -> 'a has2p5
    | At_least_2p5 : 'a elt * 'a elt * ('a, _ ge5) t -> 'a has2p5
  val has2p5 : ('a, _ ge1) t -> 'a has2p5

  type 'a has8 =
    | Less_than_8 : ('a elt five * ('a elt, eq2) vector) -> 'a has8
    | At_least_8 : ('a, _ ge8) t -> 'a has8
  val has8   : ('a, _ ge5) t -> 'a has8

  type 'a has3p8 =
    | Less_than_11 : 'a elt eight * ('a elt, eq2) vector -> 'a has3p8
    | At_least_11 : 'a elt * 'a elt * 'a elt * ('a, _ ge8) t -> 'a has3p8
  val has3p8 : ('a, _ ge8) t -> 'a has3p8

end = struct
  type 'a elt = L2R of 'a | R2L of 'a
  type ('a, 'quantity) t = 'a elt Deq.t

  let elt_rev = function
    | L2R x -> R2L x
    | R2L x -> L2R x

  let elt_out x t =
    if Deq.is_rev t
    then elt_rev x
    else x

  let empty = Deq.empty
  let rev t = Deq.rev t
  let is_rev t = Deq.is_rev t

  let cons x t = Deq.cons (elt_out x t) t
  let snoc t x = Deq.snoc t (elt_out x t)

  let opt_uncons t =
    match Deq.uncons t with
    | None -> None
    | Some (x, t') -> Some (elt_out x t, t')

  let opt_unsnoc t =
    match Deq.unsnoc t with
    | None -> None
    | Some (t', x) -> Some (t', elt_out x t)

  let uncons t =
    match opt_uncons t with
    | None -> assert false
    | Some (x, t) -> x, t

  let unsnoc t =
    match opt_unsnoc t with
    | None -> failwith "unsnoc unsafe?"
    | Some (t, x) -> t, x

  let single_elt x = Deq.cons x empty
  let triple x y z = cons x (cons y (single_elt z))

  let pair_elt x y = Deq.cons x (Deq.cons y Deq.empty)

  let uncons2 t =
    let x, t = uncons t in
    let y, t = uncons t in
    x, y, t

  let unsnoc2 t =
    let t, x = unsnoc t in
    let t, y = unsnoc t in
    t, y, x

  let two t =
    let x, y, t = uncons2 t in
    assert (Deq.is_empty t) ;
    (x, y)

  type _ has1 =
    | Exact_0 : 'a has1
    | Lte1 : ('a, _ ge1) t -> 'a has1

  let has1 t =
    if Deq.is_empty t
    then Exact_0
    else Lte1 t

  let to_dequeue t = t
  let of_dequeue d =
    if Deq.is_empty d
    then Exact_0
    else Lte1 d


  let snoc2 t (a, b) = snoc (snoc t a) b
  let cons2 (a, b) t = cons a (cons b t)

  let cons6 (a, b, c, d, e, f) t =
    cons a (cons b (cons c (cons d (cons e (cons f t)))))
  let snoc6 t (a, b, c, d, e, f) =
    snoc (snoc (snoc (snoc (snoc (snoc t a) b) c) d) e) f

  type (_, _) pop =
    | Not_enough : ('a, 'n) vector -> ('a, 'n s) pop
    | Enough : ('a, 'n) vector * 'a Deq.t -> ('a, 'n) pop

  let uncons3 t =
    match opt_uncons t with
    | None -> Not_enough V0
    | Some (x, t) ->
    match opt_uncons t with
    | None -> Not_enough (V1 x)
    | Some (y, t) ->
    match opt_uncons t with
    | None -> Not_enough (V2 (x, y))
    | Some (z, t) -> Enough (V3 (x, y, z), t)

  type 'a has8 =
    | Less_than_8 : ('a elt five * ('a elt, eq2) vector) -> 'a has8
    | At_least_8 : ('a, _ ge8) t -> 'a has8

  let uncons5 t =
    let a, b, t = uncons2 t in
    let c, d, t = uncons2 t in
    let e, t = uncons t in
    (a, b, c, d, e), t

  let has8 buffer =
    let five, t = uncons5 buffer in
    match uncons3 t with
    | Not_enough vec -> Less_than_8 (five, vec)
    | Enough _ -> At_least_8 buffer

  type 'a has5 =
    | Exact_4 : 'a elt four -> 'a has5
    | At_least_5 : ('a, _ ge5) t -> 'a has5

  let has5 buffer =
    let a, b, t = uncons2 buffer in
    let c, d, t = uncons2 t in
    match has1 t with
    | Exact_0 -> Exact_4 (a, b, c, d)
    | Lte1 _ -> At_least_5 buffer

  let cons_vector v t = vector_fold_right cons v t
  let snoc_vector t v = vector_fold_left  snoc t v

  type 'a has5p2 =
    | Less_than_5p2 : ('a elt, eq6) vector -> 'a has5p2
    | At_least_5p2 : ('a, _ ge5) t * 'a elt * 'a elt -> 'a has5p2

  let has5p2 t =
      let t, a = unsnoc t in
      match opt_unsnoc t with
      | None -> Less_than_5p2 (V1 a)
      | Some ((t as t5), b) ->
      match opt_unsnoc t with
      | None -> Less_than_5p2 (V2 (b, a))
      | Some (t, c) ->
      match opt_unsnoc t with
      | None -> Less_than_5p2 (V3 (c, b, a))
      | Some (t, d) ->
      match opt_unsnoc t with
      | None -> Less_than_5p2 (V4 (d, c, b, a))
      | Some (t, e) ->
      match opt_unsnoc t with
      | None -> Less_than_5p2 (V5 (e, d, c, b, a))
      | Some (t, f) ->
      match opt_unsnoc t with
      | None -> Less_than_5p2 (V6 (f, e, d, c, b, a))
      | Some _ -> At_least_5p2 (t5, b, a)

  type 'a has2p5 =
    | Less_than_2p5 : ('a elt, eq6) vector -> 'a has2p5
    | At_least_2p5 : 'a elt * 'a elt * ('a, _ ge5) t -> 'a has2p5

  let has2p5 t =
      let a, t = uncons t in
      match opt_uncons t with
      | None -> Less_than_2p5 (V1 a)
      | Some (b, (t as t5)) ->
      match opt_uncons t with
      | None -> Less_than_2p5 (V2 (a, b))
      | Some (c, t) ->
      match opt_uncons t with
      | None -> Less_than_2p5 (V3 (a, b, c))
      | Some (d, t) ->
      match opt_uncons t with
      | None -> Less_than_2p5 (V4 (a, b, c, d))
      | Some (e, t) ->
      match opt_uncons t with
      | None -> Less_than_2p5 (V5 (a, b, c, d, e))
      | Some (f, t) ->
      match opt_uncons t with
      | None -> Less_than_2p5 (V6 (a, b, c, d, e, f))
      | Some _ -> At_least_2p5 (a, b, t5)

  type 'a has3p8 =
    | Less_than_11 : 'a elt eight * ('a elt, eq2) vector -> 'a has3p8
    | At_least_11 : 'a elt * 'a elt * 'a elt * ('a, _ ge8) t -> 'a has3p8

  let has3p8 t =
    let a, b, t = uncons2 t in
    let c, (t as t8) = uncons t in
    let d, e, t = uncons2 t in
    let f, g, t = uncons2 t in
    let h, t = uncons t in
    match uncons3 t with
    | Not_enough vec -> Less_than_11 ((a, b, c, d, e, f, g, h), vec)
    | Enough _ -> At_least_11 (a, b, c, t8)

  let cons5 (a, b, c, d, e) t =
    cons a (cons b (cons c (cons d (cons e t))))

  let snoc5 t (a, b, c, d, e) =
    snoc (snoc (snoc (snoc (snoc t a) b) c) d) e

  let cons_5_vector (five, v) t = cons5 five (cons_vector v t)
  let snoc_5_vector t (five, v) = snoc_vector (snoc5 t five) v

  let snoc8 t (a, b, c, d, e, f, g, h) =
    snoc (snoc (snoc (snoc (snoc (snoc (snoc (snoc t a) b) c) d) e) f) g) h
end

type ('a, 'n) prefix = ('a, 'n) Buffer.t
type ('a, 'n) suffix = ('a, 'n) Buffer.t

type is_hole = IS_HOLE

and 'a stored_triple =
  | Stored_prefix : ('a, _ ge3) prefix -> 'a stored_triple
  | Stored : ('a, _ ge3) prefix
           * ('a stored_triple, _) st
           * ('a, _ ge3) suffix
          -> 'a stored_triple

and ('a, 'b, 'color, 'hole_loc, 'has_hole, 'opt_rev) only_triple =
  ('a, 'b, 'color, only, 'hole_loc, nh, 'has_hole, 'opt_rev) triple

and ('a, 'b, 'color, 'hole_loc, 'has_hole, 'opt_rev) left_triple =
  ('a, 'b, 'color, left, 'hole_loc, nh, 'has_hole, 'opt_rev) triple

and ('a, 'b, 'color, 'hole_loc, 'has_hole, 'opt_rev) right_triple =
  ('a, 'b, 'color, right, 'hole_loc, nh, 'has_hole, 'opt_rev) triple

and ('a, 'b, 'color, 'kind, 'hole_loc, 'is_hole, 'has_hole, 'is_rev) triple =

  | Triple_rev : ('a, 'b, 'c, 'k1 * 'k2, 'hl1 * 'hl2, 'ih, 'hh, not_rev) triple
              -> ('a, 'b, 'c, 'k2 * 'k1, 'hl2 * 'hl1, 'ih, 'hh, is_rev) triple

  | HOLE : ('a, 'a,
            ((notgreen * _ * notred) * _),
            (_*_) as 'k, 'k,
            is_hole, has_hole,
            _*_)
           triple

  | Only_prefix : ('a, _ ge1) prefix
               -> ('a, 'a, is_green, only, nh, nh, nh, not_rev) triple
  | Only_green :
           ('a, _ ge8) prefix
         * ('a stored_triple, (_ * notyellow * _) * notorange) deque
         * ('a, _ ge8) suffix
        -> ('a, 'a, is_green, only, nh, nh, nh, not_rev) triple
  | Only_yellow :
           ('a, _ ge7) prefix
         * ('a stored_triple, 'b, preferred_left, 'hole) not_empty
         * ('a, _ ge7) suffix
        -> ('a, 'b, is_yellow, only, 'hole, nh, has_hole, not_rev) triple
  | Only_orange :
           ('a, _ ge6) prefix
         * ('a stored_triple, 'b, preferred_right, 'hole) not_empty
         * ('a, _ ge6) suffix
        -> ('a, 'b, is_orange, only, 'hole, nh, has_hole, not_rev) triple
  | Only_red :
           ('a, _ ge5) prefix
         * ('a stored_triple, is_green) deque
         * ('a, _ ge5) suffix
        -> ('a, 'a, is_red, only, nh, nh, nh, not_rev) triple

  | Left_small :
           ('a, _ ge5) prefix
         * ('a, eq2) suffix
        -> ('a, 'b, is_green, left, nh, nh, nh, not_rev) triple
  | Left_green :
           ('a, _ ge8) prefix
         * ('a stored_triple, (_ * notyellow * _) * notorange) deque
         * ('a, eq2) suffix
        -> ('a, 'a, is_green, left, nh, nh, nh, not_rev) triple
  | Left_yellow :
           ('a, _ ge7) prefix
         * ('a stored_triple, 'b, preferred_left, 'hole) not_empty
         * ('a, eq2) suffix
        -> ('a, 'b, is_yellow, left, 'hole, nh, has_hole, not_rev) triple
  | Left_orange :
           ('a, _ ge6) prefix
         * ('a stored_triple, 'b, preferred_right, 'hole) not_empty
         * ('a, eq2) suffix
        -> ('a, 'b, is_orange, left, 'hole, nh, has_hole, not_rev) triple
  | Left_red :
           ('a, _ ge5) prefix
         * ('a stored_triple, is_green) deque
         * ('a, eq2) suffix
        -> ('a, 'a, is_red, left, nh, nh, nh, not_rev) triple

  | Right_small :
           ('a, eq2) prefix
         * ('a, _ ge5) suffix
        -> ('a, 'b, is_green, right, nh, nh, nh, not_rev) triple
  | Right_green :
           ('a, eq2) prefix
         * ('a stored_triple, (_ * notyellow * _) * notorange) deque
         * ('a, _ ge8) suffix
        -> ('a, 'a, is_green, right, nh, nh, nh, not_rev) triple
  | Right_yellow :
           ('a, eq2) prefix
         * ('a stored_triple, 'b, preferred_left, 'hole) not_empty
         * ('a, _ ge7) suffix
        -> ('a, 'b, is_yellow, right, 'hole, nh, has_hole, not_rev) triple
  | Right_orange :
           ('a, eq2) prefix
         * ('a stored_triple, 'b, preferred_right, 'hole) not_empty
         * ('a, _ ge6) suffix
        -> ('a, 'b, is_orange, right, 'hole, nh, has_hole, not_rev) triple
  | Right_red :
           ('a, eq2) prefix
         * ('a stored_triple, is_green) deque
         * ('a, _ ge5) suffix
        -> ('a, 'a, is_red, right, nh, nh, nh, not_rev) triple

and ('a, 'b, 'preference, 'hole_loc) not_empty =
  | Only_of :
       ('a, 'b, (notgreen * _ * notred) * _, only,
        (_*_) as 'hole_loc, _, has_hole, _*_) triple
    -> ('a, 'b, _, 'hole_loc) not_empty

  | Pair_left :
       ('a, 'b, (notgreen * _ * notred) * _, left,
        (_*_) as 'hole_loc, _, has_hole, _*_) triple
     * ('a, _, right) path
    -> ('a, 'b, preferred_left, 'hole_loc) not_empty

  | Pair_right :
       ('a, is_green, left) path
     * ('a, 'b, (notgreen * _ * notred) * _, right,
        (_*_) as 'hole_loc, _, has_hole, _*_) triple
    -> ('a, 'b, preferred_right, 'hole_loc) not_empty

  | Pair_left_sym :
       ('a, _, left) path
     * ('a, 'b, (notgreen * _ * notred) * _, right,
        'hole_loc, _, has_hole, _*_) triple
    -> ('a, 'b, preferred_left, 'hole_loc) not_empty

  | Pair_right_sym :
       ('a, 'b, (notgreen * _ * notred) * _, left,
        'hole_loc, _, has_hole, _*_) triple
     * ('a, is_green, right) path
    -> ('a, 'b, preferred_right, 'hole_loc) not_empty

and ('a, 'color, 'kind) path =
  | Path :
       ('a, 'b, (notgreen * _ * notred) * _,
        (_*_) as 'kind, (_*_) as 'hole_loc, _, has_hole, _*_) triple
     * ('b, 'b, ((_ * notyellow * _) * notorange) as 'color, 'hole_loc, nh, nh, nh, _*_) triple
    -> ('a, 'color, 'kind) path

and ('a, 'parent_color) deque =
  | Only_path : ('a, ((_ * notyellow * _) * notorange) as 'c, only) path -> ('a, 'c) deque
  | Pair_green :
        ('a, _, left) path
      * ('a, _, right) path
     -> ('a, is_red) deque
  | Pair_red :
        ('a, is_green, left) path
      * ('a, is_green, right) path
     -> ('a, is_green) deque

and 'a green_deque = ('a, is_green) deque
and 'a red_deque = ('a, is_red) deque

and ('a, 'c) st =
  | Void : ('a, is_green) st
  | T    : ('a, ((_ * notyellow * _) * notorange) as 'c) deque -> ('a, 'c) st
  | Rev  : ('a, ((_ * notyellow * _) * notorange) as 'c) deque -> ('a, 'c) st

type 'a semi = S : ('a, _) st -> 'a semi

type 'a t = Regular : ('a, is_green) st -> 'a t

let empty = Regular Void

let is_empty = function
  | Regular Void -> true
  | _ -> false

let triple_rev
: type a b c k1 k2 hl1 hl2 h hh rev0 rev1.
     (a, b, c, k1 * k2, hl1 * hl2, h, hh, rev0 * rev1) triple
  -> (a, b, c, k2 * k1, hl2 * hl1, h, hh, rev1 * rev0) triple
= function
  | Triple_rev t -> t
  | HOLE -> HOLE
  | Only_prefix _ as t -> Triple_rev t
  | Only_green _ as t -> Triple_rev t
  | Only_yellow _ as t -> Triple_rev t
  | Only_orange _ as t -> Triple_rev t
  | Only_red _ as t -> Triple_rev t
  | Left_small _ as t -> Triple_rev t
  | Left_green _ as t -> Triple_rev t
  | Left_yellow _ as t -> Triple_rev t
  | Left_orange _ as t -> Triple_rev t
  | Left_red _ as t -> Triple_rev t
  | Right_small _ as t -> Triple_rev t
  | Right_green _ as t -> Triple_rev t
  | Right_yellow _ as t -> Triple_rev t
  | Right_orange _ as t -> Triple_rev t
  | Right_red _ as t -> Triple_rev t

let path_rev
: type a c k1 k2. (a, c, k1 * k2) path -> (a, c, k2 * k1) path
= fun (Path (t, kont)) ->
  Path (triple_rev t, triple_rev kont)

let deque_rev
: type a c. (a, c) deque -> (a, c) deque
= function
  | Only_path p -> Only_path (path_rev p)
  | Pair_green (left, right) -> Pair_green (path_rev right, path_rev left)
  | Pair_red (left, right) -> Pair_red (path_rev right, path_rev left)

let not_empty_rev
: type a b p h1 h2.
     (a, b, p, h1 * h2) not_empty
  -> (a, b, p, h2 * h1) not_empty
= function
  | Only_of t -> Only_of (triple_rev t)
  | Pair_left (t, p) -> Pair_left_sym (path_rev p, triple_rev t)
  | Pair_right (p, t) -> Pair_right_sym (triple_rev t, path_rev p)
  | Pair_left_sym (p, t) -> Pair_left (triple_rev t, path_rev p)
  | Pair_right_sym (t, p) -> Pair_right (path_rev p, triple_rev t)

let st_rev
: type a c. (a, c) st -> (a, c) st
= function
  | Void -> Void
  | T deq -> Rev deq
  | Rev deq -> T deq

let triple_force_rev
: type a b c k1 k2 h1 h2 h hh.
     (a, b, c, k1*k2, h1*h2, h, hh, not_rev) triple
  -> (a, b, c, k2*k1, h2*h1, h, hh, not_rev) triple
= function
  | HOLE ->
      HOLE
  | Only_prefix buf ->
      Only_prefix (Buffer.rev buf)
  | Only_green (p, c, s) ->
      Only_green (Buffer.rev s, deque_rev c, Buffer.rev p)
  | Only_yellow (p, c, s) ->
      Only_yellow (Buffer.rev s, not_empty_rev c, Buffer.rev p)
  | Only_orange (p, c, s) ->
      Only_orange (Buffer.rev s, not_empty_rev c, Buffer.rev p)
  | Only_red (p, c, s) ->
      Only_red (Buffer.rev s, deque_rev c, Buffer.rev p)
  | Left_small (p, s) ->
      Right_small (Buffer.rev s, Buffer.rev p)
  | Left_green (p, c, s) ->
      Right_green (Buffer.rev s, deque_rev c, Buffer.rev p)
  | Left_yellow (p, c, s) ->
      Right_yellow (Buffer.rev s, not_empty_rev c, Buffer.rev p)
  | Left_orange (p, c, s) ->
      Right_orange (Buffer.rev s, not_empty_rev c, Buffer.rev p)
  | Left_red (p, c, s) ->
      Right_red (Buffer.rev s, deque_rev c, Buffer.rev p)
  | Right_small (p, s) ->
      Left_small (Buffer.rev s, Buffer.rev p)
  | Right_green (p, c, s) ->
      Left_green (Buffer.rev s, deque_rev c, Buffer.rev p)
  | Right_yellow (p, c, s) ->
      Left_yellow (Buffer.rev s, not_empty_rev c, Buffer.rev p)
  | Right_orange (p, c, s) ->
      Left_orange (Buffer.rev s, not_empty_rev c, Buffer.rev p)
  | Right_red (p, c, s) ->
      Left_red (Buffer.rev s, deque_rev c, Buffer.rev p)

let triple_not_rev
: type a b c k hl h hh r.
  (a, b, c, k, hl, h, hh, r) triple -> (a, b, c, k, hl, h, hh, not_rev) triple
= function
  | Triple_rev t -> triple_force_rev t
  | HOLE as t -> t
  | Only_prefix _ as t -> t
  | Only_green _ as t -> t
  | Only_yellow _ as t -> t
  | Only_orange _ as t -> t
  | Only_red _ as t -> t
  | Left_small _ as t -> t
  | Left_green _ as t -> t
  | Left_yellow _ as t -> t
  | Left_orange _ as t -> t
  | Left_red _ as t -> t
  | Right_small _ as t -> t
  | Right_green _ as t -> t
  | Right_yellow _ as t -> t
  | Right_orange _ as t -> t
  | Right_red _ as t -> t

let cons_only_triple
: type a b c h hh.
     a Buffer.elt
  -> (a, b, c, h, hh, not_rev) only_triple
  -> (a, b, c, h, hh, not_rev) only_triple
= fun x triple ->
  match triple with
  | Only_prefix buf       -> Only_prefix (Buffer.cons x buf)
  | Only_green  (p, c, s) -> Only_green  (Buffer.cons x p, c, s)
  | Only_yellow (p, c, s) -> Only_yellow (Buffer.cons x p, c, s)
  | Only_orange (p, c, s) -> Only_orange (Buffer.cons x p, c, s)
  | Only_red    (p, c, s) -> Only_red    (Buffer.cons x p, c, s)

let cons_only_triple
: type a b c h hh r.
     a Buffer.elt
  -> (a, b, c, h, hh, r) only_triple
  -> (a, b, c, h, hh, not_rev) only_triple
= fun x triple ->
  cons_only_triple x (triple_not_rev triple)

let snoc_only_triple
: type a b c h hh.
     (a, b, c, h, hh, not_rev) only_triple
  -> a Buffer.elt
  -> (a, b, c, h, hh, not_rev) only_triple
= fun triple x ->
  match triple with
  | Only_prefix buf       -> Only_prefix (Buffer.snoc buf x)
  | Only_green  (p, c, s) -> Only_green  (p, c, Buffer.snoc s x)
  | Only_yellow (p, c, s) -> Only_yellow (p, c, Buffer.snoc s x)
  | Only_orange (p, c, s) -> Only_orange (p, c, Buffer.snoc s x)
  | Only_red    (p, c, s) -> Only_red    (p, c, Buffer.snoc s x)

let snoc_only_triple
: type a b c h hh r.
     (a, b, c, h, hh, r) only_triple
  -> a Buffer.elt
  -> (a, b, c, h, hh, not_rev) only_triple
= fun triple x ->
  snoc_only_triple (triple_not_rev triple) x

type _ hole_test =
  | Is_hole  : is_hole hole_test
  | Not_hole : nh hole_test

let is_hole
: type a b c k hl h r. (a, b, c, k, hl, h, has_hole, r) triple -> h hole_test
= function
  | HOLE -> Is_hole
  | Triple_rev HOLE -> Is_hole
  | Only_yellow _ -> Not_hole
  | Only_orange _ -> Not_hole
  | Left_yellow _ -> Not_hole
  | Left_orange _ -> Not_hole
  | Right_yellow _ -> Not_hole
  | Right_orange _ -> Not_hole
  | Triple_rev (Only_yellow _) -> Not_hole
  | Triple_rev (Only_orange _) -> Not_hole
  | Triple_rev (Left_yellow _) -> Not_hole
  | Triple_rev (Left_orange _) -> Not_hole
  | Triple_rev (Right_yellow _) -> Not_hole
  | Triple_rev (Right_orange _) -> Not_hole

let cons_only_path
: type a c. a Buffer.elt -> (a, c, only) path -> (a, c, only) path
= fun x (Path (only, kont)) ->
  match is_hole only, only with
  | Is_hole, HOLE -> Path (only, cons_only_triple x kont)
  | Is_hole, Triple_rev HOLE -> Path (only, cons_only_triple x kont)
  | Not_hole, _   -> Path (cons_only_triple x only, kont)

let snoc_only_path
: type a c. (a, c, only) path -> a Buffer.elt -> (a, c, only) path
= fun (Path (only, kont)) x ->
  match is_hole only, only with
  | Is_hole, HOLE -> Path (only, snoc_only_triple kont x)
  | Is_hole, Triple_rev HOLE -> Path (only, snoc_only_triple kont x)
  | Not_hole, _   -> Path (snoc_only_triple only x, kont)

let cons_left_triple
: type a b c hl hh.
     a Buffer.elt
  -> (a, b, c, left, hl, nh, hh, not_rev) triple
  -> (a, b, c, left, hl, nh, hh, not_rev) triple
= fun x triple ->
  match triple with
  | Left_small  (p,    s) -> Left_small  (Buffer.cons x p, s)
  | Left_green  (p, c, s) -> Left_green  (Buffer.cons x p, c, s)
  | Left_yellow (p, c, s) -> Left_yellow (Buffer.cons x p, c, s)
  | Left_orange (p, c, s) -> Left_orange (Buffer.cons x p, c, s)
  | Left_red    (p, c, s) -> Left_red    (Buffer.cons x p, c, s)

let cons_left_triple
: type a b c hl hh r.
     a Buffer.elt
  -> (a, b, c, left, hl, nh, hh, r) triple
  -> (a, b, c, left, hl, nh, hh, not_rev) triple
= fun x triple ->
  cons_left_triple x (triple_not_rev triple)

let snoc_right_triple
: type a b c hl hh.
     (a, b, c, right, hl, nh, hh, not_rev) triple
  -> a Buffer.elt
  -> (a, b, c, right, hl, nh, hh, not_rev) triple
= fun triple x ->
  match triple with
  | Right_small  (p,    s) -> Right_small  (p,    Buffer.snoc s x)
  | Right_green  (p, c, s) -> Right_green  (p, c, Buffer.snoc s x)
  | Right_yellow (p, c, s) -> Right_yellow (p, c, Buffer.snoc s x)
  | Right_orange (p, c, s) -> Right_orange (p, c, Buffer.snoc s x)
  | Right_red    (p, c, s) -> Right_red    (p, c, Buffer.snoc s x)

let snoc_right_triple
: type a b c hl hh r.
     (a, b, c, right, hl, nh, hh, r) triple
  -> a Buffer.elt
  -> (a, b, c, right, hl, nh, hh, not_rev) triple
= fun triple x ->
  snoc_right_triple (triple_not_rev triple) x

let cons_left_path
: type a c. a Buffer.elt -> (a, c, left) path -> (a, c, left) path
= fun x (Path (left, kont)) ->
  match is_hole left, left with
  | Is_hole, HOLE -> Path (left, cons_left_triple x kont)
  | Is_hole, Triple_rev HOLE -> Path (left, cons_left_triple x kont)
  | Not_hole, _   -> Path (cons_left_triple x left, kont)

let snoc_right_path
: type a c. (a, c, right) path -> a Buffer.elt -> (a, c, right) path
= fun (Path (right, kont)) x ->
  match is_hole right, right with
  | Is_hole, HOLE -> Path (right, snoc_right_triple kont x)
  | Is_hole, Triple_rev HOLE -> Path (right, snoc_right_triple kont x)
  | Not_hole, _   -> Path (snoc_right_triple right x, kont)

let cons_deque
: type a c. a Buffer.elt -> (a, c) deque -> (a, c) deque
= fun x deq ->
  match deq with
  | Only_path p -> Only_path (cons_only_path x p)
  | Pair_green (prefix, suffix) -> Pair_green (cons_left_path x prefix, suffix)
  | Pair_red   (prefix, suffix) -> Pair_red   (cons_left_path x prefix, suffix)

let snoc_deque
: type a c. (a, c) deque -> a Buffer.elt -> (a, c) deque
= fun deq x ->
  match deq with
  | Only_path p -> Only_path (snoc_only_path p x)
  | Pair_green (prefix, suffix) ->
      Pair_green (prefix, snoc_right_path suffix x)
  | Pair_red (prefix, suffix) ->
      Pair_red (prefix, snoc_right_path suffix x)

let single_triple x = Only_prefix (Buffer.single_elt x)
let only_single x = Only_path (Path (HOLE, single_triple x))
let single x = T (only_single x)

let cons_t
: type a c. a Buffer.elt -> (a, c) st -> (a, c) st
= fun x t ->
  match t with
  | Void  -> single x
  | T deq -> T (cons_deque x deq)
  | Rev deq -> Rev (snoc_deque deq x)

let snoc_t
: type a c. (a, c) st -> a Buffer.elt -> (a, c) st
= fun t x ->
  match t with
  | Void  -> single x
  | T deq -> T (snoc_deque deq x)
  | Rev deq -> Rev (cons_deque x deq)

let cons x (Regular t) = Regular (cons_t x t)
let snoc (Regular t) x = Regular (snoc_t t x)

let cons_semi x (S t) = S (cons_t x t)
let snoc_semi (S t) x = S (snoc_t t x)

let cons_vector v t = vector_fold_right (fun x t -> cons x t) v t
let snoc_vector t v = vector_fold_left (fun t x -> snoc t x) t v

let cons_semi_vector v t = vector_fold_right (fun x t -> cons_semi x t) v t
let snoc_semi_vector t v = vector_fold_left (fun t x -> snoc_semi t x) t v


type _ green_or_red =
  | Is_green : is_green green_or_red
  | Is_red   : is_red green_or_red

let color
: type a c hl r. (a, a, c, hl, nh, nh, nh, r) triple -> c green_or_red
= function
  | Only_red    _ -> Is_red
  | Left_red    _ -> Is_red
  | Right_red   _ -> Is_red
  | Only_green  _ -> Is_green
  | Only_prefix _ -> Is_green
  | Left_small  _ -> Is_green
  | Left_green  _ -> Is_green
  | Right_small _ -> Is_green
  | Right_green _ -> Is_green
  | Triple_rev (Only_red    _) -> Is_red
  | Triple_rev (Left_red    _) -> Is_red
  | Triple_rev (Right_red   _) -> Is_red
  | Triple_rev (Only_green  _) -> Is_green
  | Triple_rev (Only_prefix _) -> Is_green
  | Triple_rev (Left_small  _) -> Is_green
  | Triple_rev (Left_green  _) -> Is_green
  | Triple_rev (Right_small _) -> Is_green
  | Triple_rev (Right_green _) -> Is_green

let color_path
: type a c k. (a, c, k) path -> c green_or_red
= fun (Path (_, t)) -> color t

let color_deque
: type a c. (a, c) deque -> c green_or_red
= function
  | Only_path t -> color_path t
  | Pair_red _ -> Is_green
  | Pair_green _ -> Is_red

let color_st
: type a c. (a, c) st -> c green_or_red
= function
  | Void -> Is_green
  | T t -> color_deque t
  | Rev t -> color_deque t


type 'a pref_left =
  | Pref_left : ('a, 'b, preferred_left, (_*_) as 'hole2) not_empty
              * ('b, 'b, (_ * notyellow * _) * notorange, 'hole2, nh, nh, nh, _*_) triple
             -> 'a pref_left

type 'a pref_right =
  | Pref_right : ('a, 'b, preferred_right, (_*_) as 'hole2) not_empty
               * ('b, 'b, (_ * notyellow * _) * notorange, 'hole2, nh, nh, nh, _*_) triple
              -> 'a pref_right

let pref_left
: type a c.  (a, c) deque -> a pref_left
= function
  | Only_path  (Path (d1, k))         -> Pref_left (Only_of d1, k)
  | Pair_green (Path (le, ft), right) -> Pref_left (Pair_left (le, right), ft)
  | Pair_red   (Path (le, ft), right) -> Pref_left (Pair_left (le, right), ft)

let pref_right
: type a b h1 h2 r1 r2.
     (a, b, preferred_left, h1 * h2) not_empty
  -> (b, b, (_ * notyellow * _) * notorange, h1 * h2, nh, nh, nh, r1 * r2) triple
  -> a pref_right
= fun deq ft ->
  match deq with
  | Only_of le -> Pref_right (Only_of le, ft)
  | Pair_left (le, Path (ri, ght)) ->
      Pref_right (Pair_right (Path (le, ft), ri), ght)
  | Pair_left_sym (Path (ri, ght), le) ->
      Pref_right (Pair_right_sym (ri, Path (le, ft)), ght)

let no_pref
: type a b h1 h2 r1 r2.
     (a, b, preferred_right, h1 * h2) not_empty
  -> (b, b, is_green, h1 * h2, nh, nh, nh, r1 * r2) triple
  -> (a, is_green) deque
= fun d1 ght ->
  match d1 with
  | Only_of ri ->
      Only_path (Path (ri, ght))
  | Pair_right (left, ri) ->
      Pair_red (left, Path (ri, ght))
  | Pair_right_sym (ri, left) ->
      Pair_red (Path (ri, ght), left)


let make_child
: type a b c p h1 h2 r1 r2.
     (a, b, p, h1 * h2) not_empty
  -> (b, b, c, h1 * h2, nh, nh, nh, r1 * r2) triple
  -> a semi
= fun ne_deq trip ->
  match color trip, ne_deq, trip with
  | Is_green, Only_of y, g -> S (T (Only_path (Path (y, g))))
  | Is_red,   Only_of y, g -> S (T (Only_path (Path (y, g))))
  | Is_green, Pair_left (le, right), ft ->
      S (T (Pair_green (Path (le, ft), right)))
  | Is_red, Pair_left (le, right), ft ->
      S (T (Pair_green (Path (le, ft), right)))
  | Is_green, Pair_right (left, ri), ght ->
      S (T (Pair_red (left, Path (ri, ght))))
  | Is_red, Pair_right (left, ri), ght ->
      S (T (Pair_green (left, Path (ri, ght))))

  | Is_green, Pair_left_sym (left, ri), ght ->
      S (T (Pair_green (left, Path (ri, ght))))
  | Is_red, Pair_left_sym (left, ri), ght ->
      S (T (Pair_green (left, Path (ri, ght))))
  | Is_green, Pair_right_sym (le, right), ft ->
      S (T (Pair_green (Path (le, ft), right)))
  | Is_red, Pair_right_sym (le, right), ft ->
      S (T (Pair_green (Path (le, ft), right)))

let cons_child
: type a b c p hl rev1 rev2.
     a Buffer.elt
  -> (a, b, p, hl) not_empty
  -> (b, b, c, hl, nh, nh, nh, rev1 * rev2) triple
  -> (a, b, p, hl) not_empty
   * (b, b, c, hl, nh, nh, nh, not_rev) triple
= fun x ne_deq trip ->
  match ne_deq, trip with
  | Only_of only, g ->
      begin match is_hole only, only with
      | Is_hole, HOLE -> Only_of only, cons_only_triple x g
      | Is_hole, Triple_rev HOLE -> Only_of only, cons_only_triple x g
      | Not_hole, only -> Only_of (cons_only_triple x only), triple_not_rev g
      end
  | Pair_left (left, right), g ->
      begin match is_hole left, left with
      | Is_hole, HOLE ->
          Pair_left (left, right), cons_left_triple x g
      | Is_hole, Triple_rev HOLE ->
          Pair_left (left, right), cons_left_triple x g
      | Not_hole, left ->
          Pair_left (cons_left_triple x left, right), triple_not_rev g
      end
  | Pair_right (left, right), g ->
      Pair_right (cons_left_path x left, right), triple_not_rev g

  | Pair_left_sym (left, right), g ->
      Pair_left_sym (cons_left_path x left, right), triple_not_rev g

  | Pair_right_sym (left, right), g ->
      begin match is_hole left, left with
      | Is_hole, HOLE ->
          Pair_right_sym (HOLE, right), cons_left_triple x g
      | Is_hole, Triple_rev HOLE ->
          Pair_right_sym (left, right), cons_left_triple x g
      | Not_hole, left ->
          Pair_right_sym (cons_left_triple x left, right), triple_not_rev g
      end

let snoc_child
: type a b c p hl rev1 rev2.
     (a, b, p, hl) not_empty
  -> (b, b, c, hl, nh, nh, nh, rev1 * rev2) triple
  -> a Buffer.elt
  -> (a, b, p, hl) not_empty
   * (b, b, c, hl, nh, nh, nh, not_rev) triple
= fun ne_deq trip x ->
  match ne_deq, trip with
  | Only_of only, g ->
      begin match is_hole only, only with
      | Is_hole,  HOLE -> Only_of only, snoc_only_triple g x
      | Is_hole,  Triple_rev HOLE -> Only_of only, snoc_only_triple g x
      | Not_hole, only -> Only_of (snoc_only_triple only x), triple_not_rev g
      end
  | Pair_right (left, right), g ->
      begin match is_hole right, right with
      | Is_hole, HOLE  ->
          Pair_right (left, right), snoc_right_triple g x
      | Is_hole, Triple_rev HOLE  ->
          Pair_right (left, right), snoc_right_triple g x
      | Not_hole, right ->
          Pair_right (left, snoc_right_triple right x), triple_not_rev g
      end
  | Pair_left (left, right), g ->
      Pair_left (left, snoc_right_path right x), triple_not_rev g

  | Pair_left_sym (left, right), g ->
      begin match is_hole right, right with
      | Is_hole, HOLE  ->
          Pair_left_sym (left, right), snoc_right_triple g x
      | Is_hole, Triple_rev HOLE  ->
          Pair_left_sym (left, right), snoc_right_triple g x
      | Not_hole, right ->
          Pair_left_sym (left, snoc_right_triple right x), triple_not_rev g
      end
  | Pair_right_sym (left, right), g ->
      Pair_right_sym (left, snoc_right_path right x), triple_not_rev g


type 'a buffer_12 = 'a Buffer.elt * ('a Buffer.elt, eq1) vector

let stored_left
: type a c.
     (a, _ ge5) prefix
  -> (a stored_triple, c) st
  -> (a, eq2) suffix
  -> a buffer_12
  -> (a, eq2) prefix * a stored_triple
= fun p2 d2 s2 s1 ->
  let s2 =
    let a, v1 = s1 in
    Buffer.snoc_vector (Buffer.snoc s2 a) v1
  in
  let x, p2 = Buffer.uncons p2 in
  let y, p2 = Buffer.uncons p2 in
  let s3 = Buffer.pair_elt x y in
  s3, Stored (p2, d2, s2)

let stored_right
: type a any_c.
     a buffer_12
  -> (a, eq2) prefix
  -> (a stored_triple, any_c) st
  -> (a, _ ge5) suffix
  -> a stored_triple * (a, eq2) suffix
= fun s1 p2 d2 s2 ->
  let p2 =
    let a, v1 = s1 in
    Buffer.cons a (Buffer.cons_vector v1 p2)
  in
  let s2, y, x = Buffer.unsnoc2 s2 in
  let s3 = Buffer.pair_elt y x in
  Stored (p2, d2, s2), s3

let extract_stored_left
: type a c.
     (a, c, left) path
  -> a buffer_12
  -> (a, eq2) suffix * a stored_triple
= fun (Path (t, kont)) s1 ->
  let t = triple_not_rev t in
  let kont = triple_not_rev kont in
  match t, kont with
  | (Left_orange (p2, d2, s2), d2_kont) ->
      let S d2 = make_child d2 d2_kont in
      stored_left p2 d2 s2 s1
  | (Left_yellow (p2, d2, s2), d2_kont) ->
      let S d2 = make_child d2 d2_kont in
      stored_left p2 d2 s2 s1
  | (HOLE, Left_small (p2,     s2)) -> stored_left p2 Void   s2 s1
  | (HOLE, Left_green (p2, d2, s2)) -> stored_left p2 (T d2) s2 s1
  | (HOLE, Left_red   (p2, d2, s2)) -> stored_left p2 (T d2) s2 s1

let extract_stored_right
: type a c.
     a buffer_12
  -> (a, c, right) path
  -> a stored_triple * (a, eq2) suffix
= fun s1 (Path (t, kont)) ->
  let t = triple_not_rev t in
  let kont = triple_not_rev kont in
  match t, kont with
  | Right_orange (p2, d2, s2), d2_kont ->
      let S d2 = make_child d2 d2_kont in
      stored_right s1 p2 d2 s2
  | Right_yellow (p2, d2, s2), d2_kont ->
      let S d2 = make_child d2 d2_kont in
      stored_right s1 p2 d2 s2
  | HOLE, Right_small (p2,     s2) -> stored_right s1 p2 Void   s2
  | HOLE, Right_green (p2, d2, s2) -> stored_right s1 p2 (T d2) s2
  | HOLE, Right_red   (p2, d2, s2) -> stored_right s1 p2 (T d2) s2

type ('a, 'color, 'kind) path_not_rev =
  Path_nr :
       ('a, 'b, (notgreen * _ * notred) * _,
        (_*_) as 'kind, (_*_) as 'hole_loc, _, has_hole, not_rev) triple
     * ('b, 'b, ((_ * notyellow * _) * notorange) as 'color,
        'hole_loc, nh, nh, nh, not_rev) triple
    -> ('a, 'color, 'kind) path_not_rev

let path_not_rev
: type a c k.  (a, c, k) path -> (a, c, k) path_not_rev
= function
  | Path (x, y) -> Path_nr (triple_not_rev x, triple_not_rev y)

let left_of_pair
: type a c1 c2.
     (a, c1, left) path
  -> (a, c2, right) path
  -> (a, c1, left) path
= fun left right ->
  match color_path left, path_not_rev left with
  | _, Path_nr (Left_yellow (p1, d1, s1), kont) ->
      let a, b = Buffer.two s1 in
      let s1 = (a, V1 b) in
      let stored, s3 = extract_stored_right s1 right in
      let d1, kont = snoc_child d1 kont (Buffer.L2R stored) in
      Path (Left_yellow (p1, d1, s3), kont)
  | _, Path_nr (Left_orange (p1, d1, s1), kont) ->
      let a, b = Buffer.two s1 in
      let s1 = (a, V1 b) in
      let stored, s3 = extract_stored_right s1 right in
      let d1, kont = snoc_child d1 kont (Buffer.L2R stored) in
      Path (Left_orange (p1, d1, s3), kont)
  | Is_green, Path_nr (HOLE, Left_green (p1, d1, s1)) ->
      let a, b = Buffer.two s1 in
      let s1 = (a, V1 b) in
      let stored, s3 = extract_stored_right s1 right in
      let d1 = snoc_deque d1 (Buffer.L2R stored) in
      Path (HOLE, Left_green (p1, d1, s3))
  | Is_red, Path_nr (HOLE, Left_red (p1, d1, s1)) ->
      let a, b = Buffer.two s1 in
      let s1 = (a, V1 b) in
      let stored, s3 = extract_stored_right s1 right in
      let d1 = snoc_deque d1 (Buffer.L2R stored) in
      Path (HOLE, Left_red (p1, d1, s3))
  | Is_green, Path_nr (HOLE, Left_small (p1, s1)) ->
      let a, b = Buffer.two s1 in
      let p1 = Buffer.snoc p1 a in
      let s1 = (b, V0) in
      let stored, s3 = extract_stored_right s1 right in
      let kont = single_triple (Buffer.L2R stored) in
      Path (Left_orange (p1, Only_of HOLE, s3), kont)

let right_of_pair
: type a c1 c2.
   (a, c1, left) path -> (a, c2, right) path -> (a, c2, right) path
= fun left right ->
  match color_path right, path_not_rev right with
  | _, Path_nr (Right_yellow (p1, d1, s1), kont) ->
      let a, b = Buffer.two p1 in
      let p1 = (a, V1 b) in
      let p3, stored = extract_stored_left left p1 in
      let d1, kont = cons_child (Buffer.L2R stored) d1 kont in
      Path (Right_yellow (p3, d1, s1), kont)
  | _, Path_nr (Right_orange (p1, d1, s1), kont) ->
      let a, b = Buffer.two p1 in
      let p1 = (a, V1 b) in
      let p3, stored = extract_stored_left left p1 in
      let d1, kont = cons_child (Buffer.L2R stored) d1 kont in
      Path (Right_orange (p3, d1, s1), kont)
  | Is_green, Path_nr (HOLE, Right_green (p1, d1, s1)) ->
      let a, b = Buffer.two p1 in
      let p1 = (a, V1 b) in
      let p3, stored = extract_stored_left left p1 in
      let d1 = cons_deque (Buffer.L2R stored) d1 in
      Path (HOLE, Right_green (p3, d1, s1))
  | Is_red, Path_nr (HOLE, Right_red (p1, d1, s1)) ->
      let a, b = Buffer.two p1 in
      let p1 = (a, V1 b) in
      let p3, stored = extract_stored_left left p1 in
      let d1 = cons_deque (Buffer.L2R stored) d1 in
      Path (HOLE, Right_red (p3, d1, s1))
  | Is_green, Path_nr (HOLE, Right_small (p1, s1)) ->
      let a, b = Buffer.two p1 in
      let p1 = (a, V0) in
      let s1 = Buffer.cons b s1 in
      let p3, stored = extract_stored_left left p1 in
      let kont = single_triple (Buffer.L2R stored) in
      Path (Right_orange (p3, Only_of HOLE, s1), kont)

type (_, _, _) path_attempt =
  | Small : ('a Buffer.elt, eq6) vector -> ('a, _, _) path_attempt
  | Ok : ('a, 'c, 'k) path -> ('a, 'c, 'k) path_attempt
  | Any : ('a, _, 'k) path -> ('a, is_red, 'k) path_attempt

let left_of_only
: type a c. (a, c, only) path -> (a, c, left) path_attempt
= fun path ->
  match color_path path, path_not_rev path with
  | _, Path_nr (Only_yellow (p1, d1, s1), kont) ->
      let s1, b, a = Buffer.unsnoc2 s1 in
      let s1' = Buffer.pair_elt b a in
      let d1, kont = snoc_child d1 kont (Buffer.L2R (Stored_prefix s1)) in
      Ok (Path (Left_yellow (p1, d1, s1'), kont))
  | _, Path_nr (Only_orange (p1, d1, s1), kont) ->
      let s1, b, a = Buffer.unsnoc2 s1 in
      let s1' = Buffer.pair_elt b a in
      let d1, kont = snoc_child d1 kont (Buffer.L2R (Stored_prefix s1)) in
      Ok (Path (Left_orange (p1, d1, s1'), kont))
  | Is_green, Path_nr (HOLE, Only_green (p1, d1, s1)) ->
      let s1, b, a = Buffer.unsnoc2 s1 in
      let s1' = Buffer.pair_elt b a in
      let d1 = snoc_deque d1 (Buffer.L2R (Stored_prefix s1)) in
      Ok (Path (HOLE, Left_green (p1, d1, s1')))
  | Is_red, Path_nr (HOLE, Only_red (p1, d1, s1)) ->
      let s1, b, a = Buffer.unsnoc2 s1 in
      let s1' = Buffer.pair_elt b a in
      let d1 = snoc_deque d1 (Buffer.L2R (Stored_prefix s1)) in
      Ok (Path (HOLE, Left_red (p1, d1, s1')))
  | Is_green, Path_nr (HOLE, Only_prefix p1) ->
      begin match Buffer.has5p2 p1 with
      | Buffer.Less_than_5p2 vec -> Small vec
      | Buffer.At_least_5p2 (p1, x, y) ->
          let s1 = Buffer.pair_elt x y in
          Ok (Path (HOLE, Left_small (p1, s1)))
      end

let right_of_only
: type a c.  (a, c, only) path -> (a, c, right) path_attempt
= fun path ->
  match color_path path, path_not_rev path with
  | _, Path_nr (Only_yellow (p1, d1, s1), kont) ->
      let a, b, p1 = Buffer.uncons2 p1 in
      let d1, kont = cons_child (Buffer.L2R (Stored_prefix p1)) d1 kont in
      let p1' = Buffer.pair_elt a b in
      Ok (Path (Right_yellow (p1', d1, s1), kont))
  | _, Path_nr (Only_orange (p1, d1, s1), kont) ->
      let a, b, p1 = Buffer.uncons2 p1 in
      let d1, kont = cons_child (Buffer.L2R (Stored_prefix p1)) d1 kont in
      let p1' = Buffer.pair_elt a b in
      Ok (Path (Right_orange (p1', d1, s1), kont))
  | Is_green, Path_nr (HOLE, Only_green (p1, d1, s1)) ->
      let a, b, p1 = Buffer.uncons2 p1 in
      let d1 = cons_deque (Buffer.L2R (Stored_prefix p1)) d1 in
      let p1' = Buffer.pair_elt a b in
      Ok (Path (HOLE, Right_green (p1', d1, s1)))
  | Is_red, Path_nr (HOLE, Only_red (p1, d1, s1)) ->
      let a, b, p1 = Buffer.uncons2 p1 in
      let d1 = cons_deque (Buffer.L2R (Stored_prefix p1)) d1 in
      let p1' = Buffer.pair_elt a b in
      Ok (Path (HOLE, Right_red (p1', d1, s1)))
  | Is_green, Path_nr (HOLE, Only_prefix p1) ->
      begin match Buffer.has2p5 p1 with
      | Buffer.Less_than_2p5 vec -> Small vec
      | Buffer.At_least_2p5 (x, y, s1) ->
          let p1 = Buffer.pair_elt x y in
          Ok (Path (HOLE, Right_small (p1, s1)))
      end

let make_left
: type a c. (a, c) st -> (a, c, left) path_attempt
= fun st ->
  match color_st st, st with
  | _, Void -> Small V0
  | _, T (Only_path only) -> left_of_only only
  | _, Rev (Only_path only) -> left_of_only (path_rev only)
  | Is_green, T (Pair_red (a, b)) -> Ok  (left_of_pair a b)
  | Is_red, T (Pair_green (a, b)) -> Any (left_of_pair a b)
  | Is_green, Rev (Pair_red (a, b)) ->
      Ok (left_of_pair (path_rev b) (path_rev a))
  | Is_red, Rev (Pair_green (a, b)) ->
      Any (left_of_pair (path_rev b) (path_rev a))

let make_right
: type a c. (a, c) st -> (a, c, right) path_attempt
= fun st ->
  match color_st st, st with
  | _, Void -> Small V0
  | _, T (Only_path only) -> right_of_only only
  | _, Rev (Only_path only) -> right_of_only (path_rev only)
  | Is_green, T (Pair_red (a, b)) -> Ok  (right_of_pair a b)
  | Is_red, T (Pair_green (a, b)) -> Any (right_of_pair a b)
  | Is_green, Rev (Pair_red (a, b)) ->
      Ok (right_of_pair (path_rev b) (path_rev a))
  | Is_red, Rev (Pair_green (a, b)) ->
      Any (right_of_pair (path_rev b) (path_rev a))

let concat_semi
: type a. a semi -> a semi -> a semi
= fun ((S dl) as deq_left) ((S dr) as deq_right) ->
  match make_left dl with
  | Small vec -> cons_semi_vector vec deq_right
  | Ok left ->
      begin match make_right dr with
      | Small vec -> snoc_semi_vector deq_left vec
      | Ok  right -> S (T (Pair_green (left, right)))
      | Any right -> S (T (Pair_green (left, right)))
      end
  | Any left ->
      begin match make_right dr with
      | Small vec -> snoc_semi_vector deq_left vec
      | Ok  right -> S (T (Pair_green (left, right)))
      | Any right -> S (T (Pair_green (left, right)))
      end

let concat
: type a. a t -> a t -> a t
= fun ((Regular dl) as deq_left) ((Regular dr) as deq_right) ->
  match make_left dl with
  | Small vec -> cons_vector vec deq_right
  | Ok left ->
      begin match make_right dr with
      | Small vec -> snoc_vector deq_left vec
      | Ok right -> Regular (T (Pair_red (left, right)))
      | _ -> .
      end
  | _ -> .


type ('a, 'k) color_path =
  | Exact_6 : 'a Buffer.elt six -> ('a, _) color_path
  | Any : ('a, _, 'k) path -> ('a, 'k) color_path

let semi_of_left
: type a c.  (a, c, left) path -> a Buffer.elt six -> a semi
= fun left six ->
  match color_path left, path_not_rev left with
  | Is_green, Path_nr (HOLE, Left_small (p2, s2)) ->
      let c, d = Buffer.two s2 in
      let p2 = Buffer.snoc2 p2 (c, d) in
      let p2 = Buffer.snoc6 p2 six in
      S (T (Only_path (Path (HOLE, Only_prefix p2))))
  | Is_green, Path_nr (HOLE, Left_green (p2, d2, s2)) ->
      let s2 = Buffer.snoc6 s2 six in
      S (T (Only_path (Path (HOLE, Only_green (p2, d2, s2)))))
  | _, Path_nr (Left_yellow (p2, d2, s2), kont) ->
      let s2 = Buffer.snoc6 s2 six in
      S (T (Only_path (Path (Only_yellow (p2, d2, s2), kont))))
  | _, Path_nr (Left_orange (p2, d2, s2), kont) ->
      let s2 = Buffer.snoc6 s2 six in
      S (T (Only_path (Path (Only_orange (p2, d2, s2), kont))))
  | Is_red, Path_nr (HOLE, Left_red (p2, d2, s2)) ->
      let s2 = Buffer.snoc6 s2 six in
      S (T (Only_path (Path (HOLE, Only_red (p2, d2, s2)))))

let semi_of_right
: type a c.  a Buffer.elt six -> (a, c, right) path -> a semi
= fun six right ->
  match color_path right, path_not_rev right with
  | Is_green, Path_nr (HOLE, Right_small (p2, s2)) ->
      let c, d = Buffer.two p2 in
      let s2 = Buffer.cons2 (c, d) s2 in
      let s2 = Buffer.cons6 six s2 in
      S (T (Only_path (Path (HOLE, Only_prefix s2))))
  | Is_green, Path_nr (HOLE, Right_green (p2, d2, s2)) ->
      let p2 = Buffer.cons6 six p2 in
      S (T (Only_path (Path (HOLE, Only_green (p2, d2, s2)))))
  | _, Path_nr (Right_yellow (p2, d2, s2), kont) ->
      let p2 = Buffer.cons6 six p2 in
      S (T (Only_path (Path (Only_yellow (p2, d2, s2), kont))))
  | _, Path_nr (Right_orange (p2, d2, s2), kont) ->
      let p2 = Buffer.cons6 six p2 in
      S (T (Only_path (Path (Only_orange (p2, d2, s2), kont))))
  | Is_red, Path_nr (HOLE, Right_red (p2, d2, s2)) ->
      let p2 = Buffer.cons6 six p2 in
      S (T (Only_path (Path (HOLE, Only_red (p2, d2, s2)))))

let uncons_green_left
: type a. (a, is_green, left) path -> a Buffer.elt * (a, left) color_path
= fun left ->
  match path_not_rev left with
  | Path_nr (HOLE, Left_small (p1, s1)) ->
      let x, p1 = Buffer.uncons p1 in
      let result = match Buffer.has5 p1 with
         | Buffer.At_least_5 p1 ->
             Any (Path (HOLE, Left_small (p1, s1)))
         | Buffer.Exact_4 (a, b, c, d) ->
             let e, f = Buffer.two s1 in
             Exact_6 (a, b, c, d, e, f) in
      x, result
  | Path_nr (HOLE, Left_green (p1, d1, s1)) ->
      let x, p1 = Buffer.uncons p1 in
      let Pref_left (d1, k) = pref_left d1 in
      let path = Path (Left_yellow (p1, d1, s1), k) in
      x, Any path
  | Path_nr (Left_yellow (p1, d1, s1), kont) ->
      let x, p1 = Buffer.uncons p1 in
      let Pref_right (d1, kont) = pref_right d1 kont in
      let path = Path (Left_orange (p1, d1, s1), kont) in
      x, Any path
  | Path_nr (Left_orange (p1, d1, s1), kont) ->
      let x, p1 = Buffer.uncons p1 in
      let d1 = no_pref d1 kont in
      let path = Path (HOLE, Left_red (p1, d1, s1)) in
      x, Any path

let unsnoc_green_right
: type a. (a, is_green, right) path -> (a, right) color_path * a Buffer.elt
= fun right ->
  match path_not_rev right with
  | Path_nr (HOLE, Right_small (p1, s1)) ->
      let s1, x = Buffer.unsnoc s1 in
      let result = match Buffer.has5 s1 with
        | Buffer.At_least_5 s1 ->
            Any (Path (HOLE, Right_small (p1, s1)))
        | Buffer.Exact_4 (c, d, e, f) ->
            let a, b = Buffer.two p1 in
            Exact_6 (a, b, c, d, e, f) in
      result, x
  | Path_nr (HOLE, Right_green (p1, d1, s1)) ->
      let s1, x = Buffer.unsnoc s1 in
      let Pref_left (d1, k) = pref_left d1 in
      let path = Path (Right_yellow (p1, d1, s1), k) in
      Any path, x
  | Path_nr (Right_yellow (p1, d1, s1), kont) ->
      let s1, x = Buffer.unsnoc s1 in
      let Pref_right (d1, kont) = pref_right d1 kont in
      let path = Path (Right_orange (p1, d1, s1), kont) in
      Any path, x
  | Path_nr (Right_orange (p1, d1, s1), kont) ->
      let s1, x = Buffer.unsnoc s1 in
      let d1 = no_pref d1 kont in
      let path = Path (HOLE, Right_red (p1, d1, s1)) in
      Any path, x

let uncons_green
: type a. (a, is_green) deque -> a Buffer.elt * a semi
= function
  | Only_path path ->
      begin match path_not_rev path with
      | Path_nr (HOLE, Only_prefix p1) ->
          let x, p1 = Buffer.uncons p1 in
          let s = match Buffer.has1 p1 with
             | Buffer.Exact_0 -> S Void
             | Buffer.Lte1 p1 ->
                 S (T (Only_path (Path (HOLE, Only_prefix p1))))
          in
          x, s
      | Path_nr (HOLE, Only_green (p1, d1, s1)) ->
          let x, p1 = Buffer.uncons p1 in
          let Pref_left (d1, k) = pref_left d1 in
          let only = Path (Only_yellow (p1, d1, s1), k) in
          x, S (T (Only_path only))
      | Path_nr (Only_yellow (p1, d1, s1), kont) ->
          let x, p1 = Buffer.uncons p1 in
          let Pref_right (d1, kont) = pref_right d1 kont in
          let only = (Path (Only_orange (p1, d1, s1), kont)) in
          x, S (T (Only_path only))
      | Path_nr (Only_orange (p1, d1, s1), kont) ->
          let x, p1 = Buffer.uncons p1 in
          let d1 = no_pref d1 kont in
          let only = Path (HOLE, Only_red (p1, d1, s1)) in
          x, S (T (Only_path only))
      end
  | Pair_red (left, right) ->
      let x, result = uncons_green_left left in
      x, begin match result with
         | Any left ->
             S (T (Pair_green (left, right)))
         | Exact_6 six ->
             semi_of_right six right
         end

let unsnoc_green
: type a. (a, is_green) deque -> a semi * a Buffer.elt
= function
  | Only_path path ->
      begin match path_not_rev path with
      | Path_nr (HOLE, Only_prefix p1) ->
          let p1, x = Buffer.unsnoc p1 in
          let s = match Buffer.has1 p1 with
             | Buffer.Exact_0 -> S Void
             | Buffer.Lte1 p1 ->
                 S (T (Only_path (Path (HOLE, Only_prefix p1))))
          in
          s, x
      | Path_nr (HOLE, Only_green (p1, d1, s1)) ->
          let s1, x = Buffer.unsnoc s1 in
          let Pref_left (d1, k) = pref_left d1 in
          let only = Path (Only_yellow (p1, d1, s1), k) in
          S (T (Only_path only)), x
      | Path_nr (Only_yellow (p1, d1, s1), kont) ->
          let s1, x = Buffer.unsnoc s1 in
          let Pref_right (d1, kont) = pref_right d1 kont in
          let only = (Path (Only_orange (p1, d1, s1), kont)) in
          S (T (Only_path only)), x
      | Path_nr (Only_orange (p1, d1, s1), kont) ->
          let s1, x = Buffer.unsnoc s1 in
          let d1 = no_pref d1 kont in
          let only = Path (HOLE, Only_red (p1, d1, s1)) in
          S (T (Only_path only)), x
      end
  | Pair_red (left, right) ->
      let result, x = unsnoc_green_right right in
      let result = match result with
         | Any right -> S (T (Pair_green (left, right)))
         | Exact_6 six -> semi_of_left left six
      in
      result, x

type _ unstored =
  Unstored : ('a, _ ge3) prefix * 'a stored_triple semi -> 'a unstored

let stored_elt = function
  | Buffer.L2R x -> x
  | Buffer.R2L (Stored_prefix p) ->
      Stored_prefix (Buffer.rev p)
  | Buffer.R2L (Stored (p, d, s)) ->
      Stored (Buffer.rev s, st_rev d, Buffer.rev p)

let uncons_stored
= fun d1 ->
    let stored, d1 = uncons_green d1 in
    match stored_elt stored with
    | Stored_prefix p2 -> Unstored (p2, d1)
    | Stored (p2, d2, s2) ->
        let s2 = Stored_prefix s2 in
        let d1 = cons_semi (Buffer.L2R s2) d1 in
        let d1 = concat_semi (S d2) d1 in
        Unstored (p2, d1)

let unsnoc_stored
= fun d1 ->
    let d1, stored = unsnoc_green d1 in
    match stored_elt stored with
    | Stored_prefix p2 -> Unstored (p2, d1)
    | Stored (p2, d2, s2) ->
        let p2 = Stored_prefix p2 in
        let d1 = snoc_semi d1 (Buffer.L2R p2) in
        let d1 = concat_semi d1 (S d2) in
        Unstored (s2, d1)

type _ sandwich =
  | Alone : 'a Buffer.elt -> 'a sandwich
  | Sandwich : 'a Buffer.elt * 'a semi * 'a Buffer.elt -> 'a sandwich

let unsandwich_green
: type a. (a, is_green) deque -> a sandwich
= function
  | Only_path path ->
      begin match path_not_rev path with
      | Path_nr (HOLE, Only_prefix p1) ->
          let x, p1 = Buffer.uncons p1 in
          begin match Buffer.has1 p1 with
          | Buffer.Exact_0 -> Alone x
          | Buffer.Lte1 p1 ->
              let p1, y = Buffer.unsnoc p1 in
              let d1 = match Buffer.has1 p1 with
                | Buffer.Exact_0 -> S Void
                | Buffer.Lte1 p1 ->
                    S (T (Only_path (Path (HOLE, Only_prefix p1))))
              in
              Sandwich (x, d1, y)
          end
      | Path_nr (HOLE, Only_green (p1, d1, s1)) ->
          let x, p1 = Buffer.uncons p1 in
          let s1, y = Buffer.unsnoc s1 in
          let Pref_left (d1, k) = pref_left d1 in
          let only = Path (Only_yellow (p1, d1, s1), k) in
          Sandwich (x, S (T (Only_path only)), y)
      | Path_nr (Only_yellow (p1, d1, s1), kont) ->
          let x, p1 = Buffer.uncons p1 in
          let s1, y = Buffer.unsnoc s1 in
          let Pref_right (d1, kont) = pref_right d1 kont in
          let only = (Path (Only_orange (p1, d1, s1), kont)) in
          Sandwich (x, S (T (Only_path only)), y)
      | Path_nr (Only_orange (p1, d1, s1), kont) ->
          let x, p1 = Buffer.uncons p1 in
          let s1, y = Buffer.unsnoc s1 in
          let d1 = no_pref d1 kont in
          let only = Path (HOLE, Only_red (p1, d1, s1)) in
          Sandwich (x, S (T (Only_path only)), y)
      end

  | Pair_red (left, right) ->
      let x, left = uncons_green_left left in
      let right, y = unsnoc_green_right right in
      let d1 = match left, right with
        | Any left, Any right -> S (T (Pair_green (left, right)))
        | Exact_6 lst, Any right -> semi_of_right lst right
        | Any left, Exact_6 lst  -> semi_of_left left lst
        | Exact_6 left_lst, Exact_6 right_lst ->
            let buf = Buffer.empty in
            let buf = Buffer.cons6 right_lst buf in
            let buf = Buffer.cons6 left_lst  buf in
            S (T (Only_path (Path (HOLE, Only_prefix buf))))
      in
      Sandwich (x, d1, y)

type _ unstored_sandwich =
  | Unstored_alone    : ('a, _ ge3) prefix -> 'a unstored_sandwich
  | Unstored_sandwich : ('a, _ ge3) prefix
                      * 'a stored_triple semi
                      * ('a, _ ge3) suffix
                     -> 'a unstored_sandwich

let unsandwich_stored
= fun d1 ->
  match unsandwich_green d1 with
  | Alone stored ->
      begin match stored_elt stored with
      | Stored_prefix x -> Unstored_alone x
      | Stored (p1, d1, s1) -> Unstored_sandwich (p1, S d1, s1)
      end
  | Sandwich (left, d1, right) ->
      begin match stored_elt left, d1, stored_elt right with
      | (Stored_prefix p2, d1, Stored_prefix s3) ->
          Unstored_sandwich (p2, d1, s3)
      | (Stored (p2, d2, s2), d1, Stored_prefix s3) ->
            let d1 = cons_semi (Buffer.L2R (Stored_prefix s2)) d1 in
            let d1 = concat_semi (S d2) d1 in
            Unstored_sandwich (p2, d1, s3)
      | (Stored_prefix p2, d1, Stored (p3, d3, s3)) ->
            let d1 = snoc_semi d1 (Buffer.L2R (Stored_prefix p3)) in
            let d1 = concat_semi d1 (S d3) in
            Unstored_sandwich (p2, d1, s3)
      | (Stored (p2, d2, s2), d1, Stored (p3, d3, s3)) ->
            let d1 = cons_semi (Buffer.L2R (Stored_prefix s2)) d1 in
            let d1 = snoc_semi d1 (Buffer.L2R (Stored_prefix p3)) in
            let d1 = concat_semi (S d2) d1 in
            let d1 = concat_semi d1 (S d3) in
            Unstored_sandwich (p2, d1, s3)
      end

let only_small
: type a.
     (a, _ ge8) prefix
  -> (a, _ ge8) suffix
  -> (a, a, is_green, only, nh, nh, nh, not_rev) triple
= fun p2 s2 ->
  match Buffer.has3p8 s2 with
  | Buffer.Less_than_11 (eight, vec) ->
      let p2 = Buffer.snoc_vector (Buffer.snoc8 p2 eight) vec in
      Only_prefix p2
  | Buffer.At_least_11 (a, b, c, s2) ->
      let stored = Stored_prefix (Buffer.triple a b c) in
      let d1 = only_single (Buffer.L2R stored) in
      Only_green (p2, d1, s2)

let only_green p2 d2 s2 = match d2 with
  | S (T d2) -> Only_green (p2, d2, s2)
  | S (Rev d2) -> Only_green (p2, deque_rev d2, s2)
  | S Void -> only_small p2 s2

let ensure_green
: type a ho c r.
     (a, a, c, ho, nh, nh, nh, r) triple
  -> (a, a, is_green, ho, nh, nh, nh, not_rev) triple
= fun t ->
  let t = triple_not_rev t in
  match color t, t with
  | Is_green, t ->
      t
  | Is_red, Only_red (p1, d1, s1) ->
      begin match Buffer.has8 p1, Buffer.has8 s1 with
      | Buffer.At_least_8 p1, Buffer.At_least_8 s1 ->
          Only_green (p1, d1, s1)
      | Buffer.Less_than_8 p1_lst, Buffer.Less_than_8 s1_lst ->
          begin match unsandwich_stored d1 with
          | Unstored_alone center ->
              let center = Buffer.cons_5_vector p1_lst center in
              let center = Buffer.snoc_5_vector center s1_lst in
              Only_prefix center
          | Unstored_sandwich (p2, d2, s2) ->
              let p2 = Buffer.cons_5_vector p1_lst p2 in
              let s2 = Buffer.snoc_5_vector s2 s1_lst in
              only_green p2 d2 s2
          end
      | Buffer.Less_than_8 p1_lst, Buffer.At_least_8 s1 ->
          let Unstored (p2, d1) = uncons_stored d1 in
          let p2 = Buffer.cons_5_vector p1_lst p2 in
          let result = only_green p2 d1 s1 in
          result
      | Buffer.At_least_8 p1, Buffer.Less_than_8 s1_lst ->
          let Unstored (s2, d1) = unsnoc_stored d1 in
          let s2 = Buffer.snoc_5_vector s2 s1_lst in
          only_green p1 d1 s2
      end
  | Is_red, Left_red (p1, d1, s1) ->
      begin match Buffer.has8 p1 with
      | Buffer.At_least_8 p1 ->
          Left_green (p1, d1, s1)
      | Buffer.Less_than_8 p1_lst ->
          let Unstored (p2, S d1) = uncons_stored d1 in
          let p2 = Buffer.cons_5_vector p1_lst p2 in
          begin match d1 with
          | Void -> Left_small (p2, s1)
          | T d1 -> Left_green (p2, d1, s1)
          | Rev d1 -> Left_green (p2, deque_rev d1, s1)
          end
      end
  | Is_red, Right_red (p1, d1, s1) ->
      begin match Buffer.has8 s1 with
      | Buffer.At_least_8 s1 ->
          Right_green (p1, d1, s1)
      | Buffer.Less_than_8 s1_lst ->
          let Unstored (s2, S d1) = unsnoc_stored d1 in
          let s2 = Buffer.snoc_5_vector s2 s1_lst in
          begin match d1 with
          | Void -> Right_small (p1, s2)
          | T d1 -> Right_green (p1, d1, s2)
          | Rev d1 -> Right_green (p1, deque_rev d1, s2)
          end
      end

let ensure_green_path
: type a c k. (a, c, k) path -> (a, is_green, k) path
= fun (Path (y, g)) -> Path (y, ensure_green g)

let ensure_green_deque
: type a c. (a, c) deque -> (a, is_green) deque
= function
  | Only_path p -> Only_path (ensure_green_path p)
  | Pair_red (a, b) -> Pair_red (a, b)
  | Pair_green (a, b) ->
      Pair_red (ensure_green_path a, ensure_green_path b)

let regular_of_semi
: type a. a semi -> a t
= function
  | S Void -> Regular Void
  | S (T deq) -> Regular (T (ensure_green_deque deq))
  | S (Rev deq) -> Regular (Rev (ensure_green_deque deq))

let rev_regular_of_semi
: type a. a semi -> a t
= function
  | S Void -> Regular Void
  | S (T deq) -> Regular (Rev (ensure_green_deque deq))
  | S (Rev deq) -> Regular (T (ensure_green_deque deq))

let unelt = function
  | Buffer.L2R x -> x
  | Buffer.R2L x -> x

let uncons
: type a. a t -> (a * a t) option
= fun (Regular t) ->
  match t with
  | Void -> None
  | T deq ->
      let x, semi_deq = uncons_green deq in
      let reg = regular_of_semi semi_deq in
      Some (unelt x, reg)
  | Rev deq ->
      let semi_deq, x = unsnoc_green deq in
      let reg = rev_regular_of_semi semi_deq in
      Some (unelt x, reg)

let unsnoc
: type a. a t -> (a t * a) option
= fun (Regular t) ->
  match t with
  | Void -> None
  | T deq ->
      let semi_deq, x = unsnoc_green deq in
      Some (regular_of_semi semi_deq, unelt x)
  | Rev deq ->
      let x, semi_deq = uncons_green deq in
      Some (rev_regular_of_semi semi_deq, unelt x)

let rev
: type a. a t -> a t
= fun (Regular t) ->
  match t with
  | Void -> Regular Void
  | T deq -> Regular (Rev deq)
  | Rev deq -> Regular (T deq)

let cons x t = cons (Buffer.L2R x) t
let snoc t x = snoc t (Buffer.L2R x)
