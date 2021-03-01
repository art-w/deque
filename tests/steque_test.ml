module type STEQUE = sig
  type 'a t
  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a t -> 'a -> 'a t
  val uncons : 'a t -> ('a * 'a t) option
  val append : 'a t -> 'a t -> 'a t
  val length : 'a t -> int
end

module Bi (A : STEQUE) (B : STEQUE) = struct
  type 'a t = 'a A.t * 'a B.t

  let rec compare_prefix n (a, b) =
    if n <= 0
    then a, b
    else
    match A.uncons a, B.uncons b with
    | None, None -> raise Not_found
    | Some (x, a), Some (y, b) ->
        assert (x = y) ;
        compare_prefix (n - 1) (a, b)
    | _ ->
        assert false

  let make a b =
    try compare_prefix 500 (a, b)
    with Not_found -> (a, b)

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

  let append (a0, b0) (a1, b1) =
    make (A.append a0 a1) (B.append b0 b1)

  let length (a, _) = A.length a
end

module D2 = Bi (Deque.Steque) (Deque.Deck)

let counter = ref 0
let elt () = incr counter ; !counter

let some_snd t = function
  | None -> t
  | Some (_, t) -> t

let () = Random.init 9

let rec test n t =
  match Random.int 5 with
  | 0 -> n, D2.cons (elt ()) t
  | 1 -> n, D2.snoc t (elt ())
  | 2 -> n, some_snd t (D2.uncons t)
  | 3 -> n, D2.append t t
  | 4 ->
      let fuel = Random.int n in
      let t' = test_repeatedly fuel D2.empty in
      n - fuel, D2.append t t'
  | _ -> assert false

and test_repeatedly n t =
  if n <= 0
  then t
  else let n, t = test n t in
       test_repeatedly (n - 1) t

let () =
  let s, d = test_repeatedly 1000 D2.empty in
  let xs = Deque.Steque.to_list s in
  let ys = Deque.Deck.to_list d in
  assert (xs = ys) ;
  assert (List.length xs = Deque.Steque.length s)
