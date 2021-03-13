module Base = struct
  include Dequeue_internal

  let fold_left
  : type a z. (z -> a -> z) -> z -> a s -> z
  = fun f z (T t) ->

    let list_of_buffer
    : type b c. (z -> b -> z) -> z -> (b, c) buffer -> z
    = fun f z buf ->
      match buf with
      | B0 -> z
      | B1 a -> f z a
      | B2 (a, b) -> f (f z a) b
      | B3 (a, b, c) -> f (f (f z a) b) c
      | B4 (a, b, c, d) -> f (f (f (f z a) b) c) d
      | B5 (a, b, c, d, e) -> f (f (f (f (f z a) b) c) d) e
    in

    let rec go
    : type b1 b2 c1 c2.
      (z -> b1 -> z) -> z -> (b1, b2, c1) deque -> (b2, c2) kont -> z
    = fun f z deq kont ->
      match deq with
      | HOLE -> go_kont f z kont
      | Yellow (prefix, child, suffix) ->
          let z = list_of_buffer f z prefix in
          let z = go (go_pair f) z child kont in
          list_of_buffer f z suffix
      | Green (prefix, child, suffix) ->
          let z = list_of_buffer f z prefix in
          let z = go (go_pair f) z child kont in
          list_of_buffer f z suffix
      | Red (prefix, child, suffix) ->
          let z = list_of_buffer f z prefix in
          let z = go (go_pair f) z child kont in
          list_of_buffer f z suffix

    and go_pair
    : type b. (z -> b -> z) -> z -> (b * b) -> z
    = fun f z (x, y) -> f (f z x) y

    and go_kont
    : type b c. (z -> b -> z) -> z -> (b, c) kont -> z
    = fun f z kont ->
      match kont with
      | Small buf -> list_of_buffer f z buf
      | Y (child, kont) -> go f z child kont
      | R (child, kont) -> go f z child kont
      | G (child, kont) -> go f z child kont
    in

    go_kont f z t

  let fold_right
  : type a z. (a -> z -> z) -> a s -> z -> z
  = fun f (T t) z ->

    let list_of_buffer
    : type b c. (b -> z -> z) -> (b, c) buffer -> z -> z
    = fun f buf z ->
      match buf with
      | B0 -> z
      | B1 a -> f a z
      | B2 (a, b) -> f a (f b z)
      | B3 (a, b, c) -> f a (f b (f c z))
      | B4 (a, b, c, d) -> f a (f b (f c (f d z)))
      | B5 (a, b, c, d, e) -> f a (f b (f c (f d (f e z))))
    in

    let rec go
    : type b1 b2 c1 c2.
      (b1 -> z -> z) -> (b1, b2, c1) deque -> z -> (b2, c2) kont -> z
    = fun f deq z kont ->
      match deq with
      | HOLE -> go_kont f kont z
      | Yellow (prefix, child, suffix) ->
          let z = list_of_buffer f suffix z in
          let z = go (go_pair f) child z kont in
          list_of_buffer f prefix z
      | Green (prefix, child, suffix) ->
          let z = list_of_buffer f suffix z in
          let z = go (go_pair f) child z kont in
          list_of_buffer f prefix z
      | Red (prefix, child, suffix) ->
          let z = list_of_buffer f suffix z in
          let z = go (go_pair f) child z kont in
          list_of_buffer f prefix z

    and go_pair
    : type b. (b -> z -> z) -> (b * b) -> z -> z
    = fun f (x, y) z -> f x (f y z)

    and go_kont
    : type b c. (b -> z -> z) -> (b, c) kont -> z -> z
    = fun f kont z ->
      match kont with
      | Small buf -> list_of_buffer f buf z
      | Y (child, kont) -> go f child z kont
      | R (child, kont) -> go f child z kont
      | G (child, kont) -> go f child z kont
    in

    go_kont f t z


  let fold_left f z { length ; s } =
    if length >= 0
    then fold_left f z s
    else fold_right (fun x z -> f z x) s z

  and fold_right f { length ; s } z =
    if length >= 0
    then fold_right f s z
    else fold_left (fun x z -> f z x) z s


  let compare_lengths xs ys = compare (length xs) (length ys)

  let append xs ys =
    if compare_lengths xs ys <= 0
    then fold_right cons xs ys
    else fold_left  snoc xs ys

end

include List_like.Make (Base)
include Base

let nth
: type a. a s -> int -> int -> a
= fun (T t) i j ->

  let buffer_length
  : type b c. int -> (b, c) buffer -> int
  = fun s -> function
    | B0 -> 0
    | B1 _ -> s
    | B2 _ -> 2 * s
    | B3 _ -> 3 * s
    | B4 _ -> 4 * s
    | B5 _ -> 5 * s
  in

  let buffer
  : type b c. int -> int -> (int -> int -> b -> a) -> (b, c) buffer -> a
  = fun i s search -> function
    | B0 -> assert false
    | B1 a -> search i s a
    | B2 (a, b) ->
        if i < s
        then search i s a
        else search (i - s) s b
    | B3 (a, b, c) ->
        begin match i / s with
        | 0 -> search i s a
        | 1 -> search (i - s) s b
        | 2 -> search (i - 2 * s) s c
        | _ -> assert false
        end
    | B4 (a, b, c, d) ->
        begin match i / s with
        | 0 -> search i s a
        | 1 -> search (i - s) s b
        | 2 -> search (i - 2 * s) s c
        | 3 -> search (i - 3 * s) s d
        | _ -> assert false
        end
    | B5 (a, b, c, d, e) ->
        begin match i / s with
        | 0 -> search i s a
        | 1 -> search (i - s) s b
        | 2 -> search (i - 2 * s) s c
        | 3 -> search (i - 3 * s) s d
        | 4 -> search (i - 4 * s) s e
        | _ -> assert false
        end
  in

  let rec go
  : type b1 b2 c1 c2.
       int
    -> int
    -> int
    -> (int -> int -> b1 -> a)
    -> (b1, b2, c1) deque
    -> (b2, c2) kont
    -> a
  = fun i j s search deq kont ->
    match deq with
    | HOLE -> go_kont i j s search kont
    | Yellow (prefix, child, suffix) ->
        go_level i j s search prefix suffix child kont
    | Green (prefix, child, suffix) ->
        go_level i j s search prefix suffix child kont
    | Red (prefix, child, suffix) ->
        go_level i j s search prefix suffix child kont

  and go_level
  : type b1 c1 c2 c3 d3 d4.
       int
    -> int
    -> int
    -> (int -> int -> b1 -> a)
    -> (b1, c1) buffer
    -> (b1, c2) buffer
    -> (b1 * b1, c3, d3) deque
    -> (c3, d4) kont
    -> a
  = fun i j s search prefix suffix child kont ->
    let prefix_len = buffer_length s prefix in
    let suffix_len = buffer_length s suffix in
    if i < prefix_len
    then buffer i s search prefix
    else if j < suffix_len
    then buffer (suffix_len - j - 1) s search suffix
    else let i, j, s = i - prefix_len, j - suffix_len, 2 * s in
         go i j s (go_pair search) child kont

  and go_pair
  : type b. (int -> int -> b -> a) -> int -> int -> (b * b) -> a
  = fun f i s (x, y) ->
    let s2 = s / 2 in
    if i < s2
    then f i s2 x
    else f (i - s2) s2 y

  and go_kont
  : type b c. int -> int -> int -> (int -> int -> b -> a) -> (b, c) kont -> a
  = fun i j s search -> function
    | Small buf -> buffer i s search buf
    | Y (child, kont) -> go i j s search child kont
    | R (child, kont) -> go i j s search child kont
    | G (child, kont) -> go i j s search child kont
  in

  let search1 : int -> int -> a -> a
  = fun _ _ x -> x
  in

  go_kont i j 1 search1 t

let nth t i =
  if i < 0 then invalid_arg "Dequeue.nth" ;
  let j = length t - i - 1 in
  if j < 0 then failwith "Dequeue.nth" ;
  if t.length >= 0
  then nth t.s i j
  else nth t.s j i

let nth_opt t i =
  try Some (nth t i) with Failure _ -> None


let rec make
: type a. int -> a -> (a, [`green]) kont
= fun n x ->
  match n with
  | 0 -> Small B0
  | 1 -> Small (B1 x)
  | 2 -> Small (B2 (x, x))
  | 3 -> Small (B3 (x, x, x))
  | _ ->
      let n = n - 4 in
      begin match n mod 2 with
      | 0 ->
          let b = B2 (x, x) in
          let x2 = (x, x) in
          G (Green (b, HOLE, b), make (n / 2) x2)
      | 1 ->
          let p = B3 (x, x, x) in
          let s = B2 (x, x) in
          let x2 = (x, x) in
          G (Green (p, HOLE, s), make ((n - 1) / 2) x2)
      | _ ->
          assert false
      end

let make n x = { length = n ; s = T (make n x) }

let rec of_list
: type a. a list -> (a, [`green]) kont
= function
  | [] -> Small B0
  | [a] -> Small (B1 a)
  | [a ; b] -> Small (B2 (a, b))
  | [a ; b ; c] -> Small (B3 (a, b, c))
  | a :: b :: c :: d :: lst ->
      let p = B2 (a, b) in
      let rec go acc = function
        | c, d, [] -> acc, B2 (c, d)
        | c, d, e :: [] -> acc, B3 (c, d, e)
        | c, d, e :: f :: xs -> go ((c, d) :: acc) (e, f, xs)
      in
      let lst, s = go [] (c, d, lst) in
      G (Green (p, HOLE, s), of_list (List.rev lst))

let of_list lst =
  { length = List.length lst ; s = T (of_list lst) }
