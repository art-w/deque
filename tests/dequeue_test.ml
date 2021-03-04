module type DEQUE = sig
  type 'a t
  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a t -> 'a -> 'a t
  val uncons : 'a t -> ('a * 'a t) option
  val unsnoc : 'a t -> ('a t * 'a) option
  val rev : 'a t -> 'a t
  val to_list : 'a t -> 'a list
  val length : 'a t -> int
end

module Naive = struct
  type 'a t = 'a list

  let empty = []

  let cons x t = x :: t

  let uncons = function
    | []   -> None
    | x::t -> Some (x, t)

  let unsnoc t =
    let rec go acc = function
      | []   -> None
      | [x]  -> Some (List.rev acc, x)
      | x::t -> go (x::acc) t
    in
    go [] t

  let concat a b = List.rev_append (List.rev a) b

  let snoc t x = concat t [x]

  let rev = List.rev

  let to_list t = t

  let length = List.length
end

module Bi (A : DEQUE) (B : DEQUE) = struct
  type 'a t = 'a A.t * 'a B.t

  let rec uncons_to_list t = match B.uncons t with
    | None -> []
    | Some (x, t) -> x :: uncons_to_list t

  let rec unsnoc_to_list acc t = match B.unsnoc t with
    | None -> acc
    | Some (t, x) -> unsnoc_to_list (x::acc) t
  let unsnoc_to_list t = unsnoc_to_list [] t

  let make a b =
    let xs = A.to_list a in
    let ys = B.to_list b in
    assert (xs = ys) ;
    assert (xs = uncons_to_list b) ;
    assert (xs = unsnoc_to_list b) ;
    assert (List.length xs = B.length b) ;
    (a, b)

  let empty = make A.empty B.empty

  let cons x (a, b) =
    make (A.cons x a) (B.cons x b)

  let snoc (a, b) x =
    make (A.snoc a x) (B.snoc b x)

  let uncons (a, b) =
    match A.uncons a, B.uncons b with
    | None, None -> None
    | Some (x, a), Some (x', b) ->
        assert (x = x') ;
        Some (x, make a b)
    | _ -> assert false

  let unsnoc (a, b) =
    match A.unsnoc a, B.unsnoc b with
    | None, None -> None
    | Some (a, x), Some (b, x') ->
        assert (x = x') ;
        Some (make a b, x)
    | _ -> assert false

  let rev (a, b) = make (A.rev a) (B.rev b)
end

module Test (X : DEQUE) = struct

  module D2 = Bi (Naive) (X)

  let elt () = Random.int 10000

  let some_fst t = function
    | None -> t
    | Some (t, _) -> t

  let some_snd t = function
    | None -> t
    | Some (_, t) -> t

  let test t =
    match Random.int 5 with
    | 0 -> D2.cons (elt ()) t
    | 1 -> D2.snoc t (elt ())
    | 2 -> some_snd t (D2.uncons t)
    | 3 -> some_fst t (D2.unsnoc t)
    | 4 -> D2.rev t
    | _ -> assert false

  let rec test_repeatedly n t =
    if n <= 0
    then ()
    else test_repeatedly (n - 1) (test t)

  let () =
    test_repeatedly 100000 D2.empty

end

module A = Test (Deque.Dequeue)
module B = Test (Deque.Deck)
