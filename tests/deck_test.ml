type 'a m =
  | Done : 'a m
  | Yield : 'a * (unit -> 'a m) -> 'a m
  | Interleave : 'a m * 'a m -> 'a m
  | Nest : (unit -> 'a m) -> 'a m
  | Bind : 'a m * ('a -> 'b m) -> 'b m
  | Delay : (unit -> 'a m) -> 'a m

let ( @ ) a b = Interleave (a, b)
let return x = Yield (x, fun () -> Done)
let ( >>= ) m f = Bind (m, f)
let map f m = Bind (m, fun x -> return (f x))
let ( <$> ) f x = map f x
let ( <*> ) f x = f >>= fun f -> map f x

let rec of_list lst = match lst with
  | [] -> Done
  | x::xs -> Yield (x, fun () -> of_list xs)

let rec force1
: type a. a m -> a option
= function
  | Done -> None
  | Yield (x, _) -> Some x
  | Delay m -> force1 (m ())
  | Interleave (a, b) ->
      begin match force1 a with
      | None -> force1 b
      | Some x -> Some x
      end
  | Bind (m, f) ->
      begin match force1 m with
      | None -> None
      | Some x -> force1 (f x)
      end
  | Nest m -> force1 (m ())

let rec iteri
: type a. int -> (int -> a -> int) -> int -> a m -> int
= fun count f d xs ->
  match xs with
  | Done -> count
  | Yield (x, xs) ->
      let count = f count x in
      iteri (count + 1) f d (xs ())
  | Delay m ->
      iteri count f d (m ())
  | Interleave (a, b) ->
      let a, b = if Random.bool () then a, b else b, a in
      let count = iteri count f d a in
      iteri count f d b
  | Bind (m, g) ->
      iteri count (fun count x -> iteri count f d (g x)) d m
  | Nest m ->
      if d <= 0
      then match force1 (m ()) with
           | None -> count
           | Some x -> f count x
      else iteri count f (d - 1) (m ())

let iteri f d xs = iteri 0 f d xs

module D = Deck_internal
open D

let rec list_gen size m =
  if size <= 0
  then return []
  else let rest = list_gen (size - 1) m in
       m >>= fun x ->
       rest >>= fun xs ->
       return (Buffer.L2R x :: xs)

let buffer_make e size : ('a, 's) Buffer.t m =
  Obj.magic @@ map Deque.Dequeue.of_list @@ list_gen size e

let sample = function
  | [] -> assert false
  | [x] -> [x]
  | x::xs ->
      if Random.bool ()
      then x :: List.nth xs (Random.int (List.length xs)) :: []
      else x :: []

let buffer_ge8 e : ('a, z ge8) Buffer.t m = buffer_make e (8 + Random.int 6)
let buffer_ge7 e : ('a, z ge7) Buffer.t m = buffer_make e (7 + Random.int 6)
let buffer_ge6 e : ('a, z ge6) Buffer.t m = buffer_make e (6 + Random.int 6)
let buffer_ge5 e : ('a, z ge5) Buffer.t m = buffer_make e (5 + Random.int 6)
let buffer_ge3 e : ('a, z ge3) Buffer.t m = buffer_make e (3 + Random.int 6)
let buffer_eq2 e : ('a, z ge2) Buffer.t m = buffer_make e 2
let buffer_ge1 e : ('a, z ge1) Buffer.t m = buffer_make e (1 + Random.int 6)

type ('a, 'preference, 'color) holy =
  | Holy : ('a, 'b, 'preference, (_*_) as 'hole_loc) not_empty
         * ('b, 'b, [< `green | `red] as 'color,
            'hole_loc, nh, nh, nh, _*_) triple
        -> ('a, 'preference, 'color) holy

type ('a, 'preference) unholy =
  | Unholy : ('a, 'a, 'preference, nh) not_empty
          -> ('a, 'preference) unholy

type 'a gr_deque = GR_deq : ('a, [< `green | `red]) deque -> 'a gr_deque
type 'a g_deque = G_deq : ('a, [`green]) deque -> 'a g_deque

type ('a, 'k) gr_path =
  GR_path : ('a, [< `green | `red], 'k) path -> ('a, 'k) gr_path
type ('a, 'k) g_path = G_path : ('a, [`green], 'k) path -> ('a, 'k) g_path

let rec stored_triple
: type a. a m -> a stored_triple m
= fun e ->
  if Random.bool ()
  then
  Nest
  (fun () ->
  ( buffer_ge3 e >>= fun p ->
    semiregular (stored_triple e) >>= fun (S c) ->
    buffer_ge3 e >>= fun s ->
    return (Stored (p, c, s))
    )
  @
  ( buffer_ge3 e >>= fun p -> return (Stored_prefix p) )
  )
  else
  Nest
  (fun () ->
  ( buffer_ge3 e >>= fun p -> return (Stored_prefix p) )
  @
  ( buffer_ge3 e >>= fun p ->
    semiregular (stored_triple e) >>= fun (S c) ->
    buffer_ge3 e >>= fun s ->
    return (Stored (p, c, s))
    )
  )

and only_green
: type a. a m -> (a, a, [`green], only, nh, nh, nh, not_rev) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_ge1 e >>= fun p -> return (Only_prefix p) )
  @
  ( buffer_ge8 e >>= fun p ->
    gr_deque (stored_triple e) >>= fun (GR_deq c) ->
    buffer_ge8 e >>= fun s ->
    return (Only_green (p, c, s))
  ))

and only_red
: type a. a m -> (a, a, [`red], only, nh, nh, nh, not_rev) triple m
= fun e ->
  Nest
  (fun () ->
    buffer_ge5 e >>= fun p ->
    g_deque (stored_triple e) >>= fun (G_deq c) ->
    buffer_ge5 e >>= fun s ->
    return (Only_red (p, c, s))
  )

and only_yellow_green
: type a. a m -> (a, [`green], only) path m
= fun e ->
  Nest
  (fun () ->
  (
    only_green e >>= fun k ->
    return (Path (HOLE, k))
  )
  @
  ( buffer_ge7 e >>= fun p ->
    not_empty_left_green (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_ge7 e >>= fun s ->
    return (Path (Only_yellow (p, c, s), k))
  )
  @
  ( buffer_ge6 e >>= fun p ->
    not_empty_right_green (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_ge6 e >>= fun s ->
    return (Path (Only_orange (p, c, s), k))
  )
  )

and only_yellow_red
: type a. a m -> (a, [`red], only) path m
= fun e ->
  Nest
  (fun () ->
  (
    only_red e >>= fun k ->
    return (Path (HOLE, k))
  )
  @
  ( buffer_ge7 e >>= fun p ->
    not_empty_left_red (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_ge7 e >>= fun s ->
    return (Path (Only_yellow (p, c, s), k))
  )
  @
  ( buffer_ge6 e >>= fun p ->
    not_empty_right_red (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_ge6 e >>= fun s ->
    return (Path (Only_orange (p, c, s), k))
  )
  )

and left_yellow_green
: type a. a m -> (a, [`green], left) path m
= fun e ->
  Nest
  (fun () ->
  (
    left_green e >>= fun k ->
    return (Path (HOLE, k))
  )
  @
  ( buffer_ge7 e >>= fun p ->
    not_empty_left_green (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_eq2 e >>= fun s ->
    return (Path (Left_yellow (p, c, s), k))
  )
  @
  ( buffer_ge6 e >>= fun p ->
    not_empty_right_green (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_eq2 e >>= fun s ->
    return (Path (Left_orange (p, c, s), k))
  )
  )

and right_yellow_green
: type a. a m -> (a, [`green], right) path m
= fun e ->
  Nest
  (fun () ->
  ( right_green e >>= fun k -> return (Path (HOLE, k)) )
  @
  ( buffer_eq2 e >>= fun p ->
    not_empty_left_green (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_ge7 e >>= fun s ->
    return (Path (Right_yellow (p, c, s), k))
  )
  @
  ( buffer_eq2 e >>= fun p ->
    not_empty_right_green (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_ge6 e >>= fun s ->
    return (Path (Right_orange (p, c, s), k))
  )
  )

and left_yellow_red
: type a. a m -> (a, [`red], left) path m
= fun e ->
  Nest
  (fun () ->
  ( left_red e >>= fun k -> return (Path (HOLE, k)) )
  @
  ( right_red e >>= fun k -> return (Path (HOLE, Triple_rev k)) )
  @
  ( buffer_ge7 e >>= fun p ->
    not_empty_left_red (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_eq2 e >>= fun s ->
    return (Path (Left_yellow (p, c, s), k))
  )
  @
  ( buffer_ge6 e >>= fun p ->
    not_empty_right_red (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_eq2 e >>= fun s ->
    return (Path (Left_orange (p, c, s), k))
  )
  )

and right_yellow_red
: type a. a m -> (a, [`red], right) path m
= fun e ->
  Nest
  (fun () ->
  ( right_red e >>= fun k -> return (Path (HOLE, k)) )
  @
  ( buffer_eq2 e >>= fun p ->
    not_empty_left_red (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_ge7 e >>= fun s ->
    return (Path (Right_yellow (p, c, s), k))
  )
  @
  ( buffer_eq2 e >>= fun p ->
    not_empty_right_red (stored_triple e) >>= fun (Holy (c, k)) ->
    buffer_ge6 e >>= fun s ->
    return (Path (Right_orange (p, c, s), k))
  )
  )

and not_empty_left_red
: type a. a m -> (a, preferred_left, [`red]) holy m
= fun e ->
  Nest
  (fun () ->
  ( left_yellow_red e >>= fun (Path (y, k)) ->
    gr_path_right e >>= fun (GR_path right) ->
    return (Holy (Pair_left (y, right), k))
  )
  @
  ( g_path_left e >>= fun (G_path left) ->
    right_yellow_red e >>= fun (Path (y, k)) ->
    return (Holy (Pair_left_sym (left, y), k))
  )
  @
  ( only_yellow_red e >>= fun (Path (y, k)) ->
    return (Holy (Only_of y, k))
  )
  )

and not_empty_right_red
: type a. a m -> (a, preferred_right, [`red]) holy m
= fun e ->
  Nest
  (fun () ->
  ( g_path_left e >>= fun (G_path left) ->
    right_yellow_red e >>= fun (Path (y, k)) ->
    return (Holy (Pair_right (left, y), k))
  )
  @
  ( left_yellow_red e >>= fun (Path (y, k)) ->
    g_path_right e >>= fun (G_path right) ->
    return (Holy (Pair_right_sym (y, right), k))
  )
  @
  ( only_yellow_red e >>= fun (Path (y, k)) ->
    return (Holy (Only_of y, k))
  ))

and not_empty_left_green
: type a. a m -> (a, preferred_left, [`green]) holy m
= fun e ->
  Nest
  (fun () ->
  ( only_yellow_green e >>= fun (Path (y, k)) ->
    return (Holy (Only_of y, k))
  )
  @
  ( left_yellow_green e >>= fun (Path (y, k)) ->
    gr_path_right e >>= fun (GR_path right) ->
    return (Holy (Pair_left (y, right), k))
  ))

and not_empty_right_green
: type a. a m -> (a, preferred_right, [`green]) holy m
= fun e ->
  Nest
  (fun () ->
  ( only_yellow_green e >>= fun (Path (y, k)) ->
    return (Holy (Only_of y, k))
  )
  @
  (
    g_path_left e >>= fun (G_path left) ->
    right_yellow_green e >>= fun (Path (y, k)) ->
    return (Holy (Pair_right (left, y), k))
  ))

and left_green
: type a. a m -> (a, a, [`green], left, nh, nh, nh, not_rev) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_ge5 e >>= fun p ->
    buffer_eq2 e >>= fun s ->
    return (Left_small (p, s))
  )
  @
  ( buffer_ge8 e >>= fun p ->
    gr_deque (stored_triple e) >>= fun (GR_deq c) ->
    buffer_eq2 e >>= fun s ->
    return (Left_green (p, c, s))
  )
  )

and left_red
: type a. a m -> (a, a, [`red], left, nh, nh, nh, not_rev) triple m
= fun e ->
  Nest
  (fun () ->
    buffer_ge5 e >>= fun p ->
    g_deque (stored_triple e) >>= fun (G_deq c) ->
    buffer_eq2 e >>= fun s ->
    return (Left_red (p, c, s))
  )

and right_green
: type a. a m -> (a, a, [`green], right, nh, nh, nh, not_rev) triple m
= fun e ->
  Nest
  (fun () ->
  ( buffer_eq2 e >>= fun p ->
    buffer_ge5 e >>= fun s ->
    return (Right_small (p, s))
  )
  @
  ( buffer_eq2 e >>= fun p ->
    gr_deque (stored_triple e) >>= fun (GR_deq c) ->
    buffer_ge8 e >>= fun s ->
    return (Right_green (p, c, s))
  )
  )

and right_red
: type a. a m -> (a, a, [`red], right, nh, nh, nh, not_rev) triple m
= fun e ->
  Nest
  (fun () ->
    buffer_eq2 e >>= fun p ->
    g_deque (stored_triple e) >>= fun (G_deq c) ->
    buffer_ge5 e >>= fun s ->
    return (Right_red (p, c, s))
  )

and g_deque
: type a. a m -> a g_deque m
= fun e ->
  Nest
  (fun () ->
  ( g_path_only e >>= fun (G_path p) ->
    return (G_deq (Only_path p))
  )
  @
  ( g_path_left  e >>= fun (G_path left) ->
    g_path_right e >>= fun (G_path right) ->
    return (G_deq (Pair_red (left, right)))
  ))

and gr_deque
: type a. a m -> a gr_deque m
= fun e ->
  Nest
  (fun () ->
  ( gr_path_left  e >>= fun (GR_path left) ->
    gr_path_right e >>= fun (GR_path right) ->
    return (GR_deq (Pair_green (left, right)))
  )
  @
  ( g_path_left  e >>= fun (G_path left) ->
    g_path_right e >>= fun (G_path right) ->
    return (GR_deq (Pair_red (left, right)))
  )
  @
  ( gr_path_only e >>= fun (GR_path p) ->
    return (GR_deq (Only_path p))
  ))

and gr_path_only
: type a. a m -> (a, only) gr_path m
= fun e ->
  Nest
  (fun () ->
  ( only_yellow_red e >>= fun (Path (y, k)) ->
    return (GR_path (Path (y, k)))
  )
  @
  ( only_yellow_green e >>= fun (Path (y, k)) ->
    return (GR_path (Path (y, k))) )
  )

and gr_path_left
: type a. a m -> (a, left) gr_path m
= fun e ->
  Nest
  (fun () ->
  ( left_yellow_red e >>= fun p -> return (GR_path p) )
  @
  ( left_yellow_green e >>= fun p -> return (GR_path p) )
  )

and gr_path_right
: type a. a m -> (a, right) gr_path m
= fun e ->
  Nest
  (fun () ->
  ( right_yellow_red e >>= fun y -> return (GR_path y) )
  @
  ( right_yellow_green e >>= fun y -> return (GR_path y) )
  )

and g_path_only
: type a. a m -> (a, only) g_path m
= fun e ->
  Nest (fun () -> only_yellow_green e >>= fun y -> return (G_path y))

and g_path_left
: type a. a m -> (a, left) g_path m
= fun e ->
  Nest (fun () -> left_yellow_green e >>= fun p -> return (G_path p))

and g_path_right
: type a. a m -> (a, right) g_path m
= fun e ->
  Nest (fun () -> right_yellow_green e >>= fun y -> return (G_path y))

and semiregular
: type a. a m -> a semi m
= fun e ->
  Nest
  (fun () ->
   return (S Void)
   @ ( gr_deque e >>= fun (GR_deq d) -> return (S (T d)) )
   @ ( gr_deque e >>= fun (GR_deq d) -> return (S (Rev d)) )
  )

and regular
: type a. a m -> a t m
= fun e ->
  return (Regular Void)
  @ ( g_deque e >>= fun (G_deq d) -> return (Regular (T d)) )



let counter = ref 0
let elt () =
  let v = !counter in
  counter := v + 1 ;
  v

let gen_elt = Delay (fun () -> return (elt ()))

let all_semi = semiregular gen_elt
let all = regular gen_elt

let list_uncons = function
  | [] -> None
  | x::xs -> Some (x, xs)

let rec list_unsnoc acc = function
  | [] -> None
  | [x] -> Some (List.rev acc, x)
  | x::xs -> list_unsnoc (x :: acc) xs
let list_unsnoc xs = list_unsnoc [] xs

let option_eq eq x y = match x, y with
  | None, None -> true
  | Some x, Some y -> eq x y
  | _ -> false

let max_depth = 6

let rec uncons_to_list acc deq =
  match D.uncons deq with
  | None -> List.rev acc
  | Some (x, d) -> uncons_to_list (x::acc) d
let uncons_to_list deq = uncons_to_list [] deq

let to_list = uncons_to_list

let rec unsnoc_to_list acc deq =
  match D.unsnoc deq with
  | None -> acc
  | Some (d, x) -> unsnoc_to_list (x::acc) d
let unsnoc_to_list deq = unsnoc_to_list [] deq

let rec un_to_list left right deq =
  if Random.bool ()
  then match D.uncons deq with
    | None -> List.concat [List.rev left ; right]
    | Some (x, d) -> un_to_list (x :: left) right d
  else match D.unsnoc deq with
    | None -> List.concat [List.rev left ; right]
    | Some (d, x) -> un_to_list left (x :: right) d

let un_to_list d = un_to_list [] [] d

let fold_left_to_list t =
  List.rev (Deque.Deck.fold_left (fun xs x -> x::xs) [] (Obj.magic t))

let test _ deq =
  let real_deq = D.regular_of_semi deq in
  let lst = to_list real_deq in
  assert (lst = Deque.Deck.to_list (Obj.magic real_deq)) ;
  assert (lst = fold_left_to_list real_deq) ;
  let x = elt () in
  assert (to_list (D.cons x real_deq) = x :: lst) ;
  assert (to_list (D.snoc real_deq x) = List.concat [lst; [x]] ) ;
  assert (lst = un_to_list real_deq) ;
  iteri
    (fun j deq' ->
      let real_deq' = D.regular_of_semi deq' in
      let lst' = to_list real_deq' in
      assert (to_list (D.concat real_deq real_deq')
              = List.concat [lst; lst']) ;
      let d1 = D.concat_semi deq deq' in
      let d2 = D.concat_semi deq' deq in
      let real_deq = D.regular_of_semi d1 in
      let dlst = to_list real_deq in
      assert (dlst = un_to_list real_deq) ;
      let real_deq = D.regular_of_semi d2 in
      let dlst = to_list real_deq in
      assert (dlst = un_to_list real_deq) ;
      j
    )
    (max_depth - 4)
    all_semi

let () = ignore (iteri test max_depth all_semi)
