include Dequeue_internal

let fold_left
: type a z. (z -> a -> z) -> z -> a t -> z
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
: type a z. (a -> z -> z) -> a t -> z -> z
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


let to_list
: type a. a t -> a list
= fun t -> fold_right (fun x xs -> x :: xs) t []

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

let of_list lst = T (of_list lst)

let length t = fold_left (fun z _ -> z + 1) 0 t
