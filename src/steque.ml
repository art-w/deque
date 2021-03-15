include Steque_internal

let append a b = concat a b

let fold_left
: type a z. (z -> a -> z) -> z -> a t -> z
= fun f z (T t) ->

  let fold_prefix
  : type b c. (z -> b -> z) -> z -> (b, c) prefix -> z
  = fun f z p ->
    match p with
  | P2 (a, b) -> f (f z a) b
  | P3 (a, b, c) -> f (f (f z a) b) c
  | P4 (a, b, c, d, deq) ->
      let z = f (f (f (f z a) b) c) d in
      Dequeue.fold_left f z deq
  in

  let list_of_suffix f z s = Dequeue.fold_left f z s in

  let rec go
  : type b1 b2 c1 c2 k.
    (z -> b1 -> z) -> z -> (b1, b2, c1, k) steque -> (b2, c2) kont -> z
  = fun f z steque kont ->
    match steque with
    | KONT -> go_kont f z kont
    | Triple (p, c, s) ->
        let z = fold_prefix f z p in
        let z = go (go_pair f) z c kont in
        list_of_suffix f z s

  and go_pair
  : type b. (z -> b -> z) -> z -> b pair -> z
  = fun f z (Pair (p, k)) ->
    let z = fold_prefix f z p in
    go_kont (go_pair f) z k

  and go_kont
  : type b c. (z -> b -> z) -> z -> (b, c) kont -> z
  = fun f z kont ->
    match kont with
    | Suffix s -> list_of_suffix f z s
    | Y (c, k) -> go f z c k
    | Yr (c, k) -> go f z c k
    | R (c, k) -> go f z c k
    | G (c, k) -> go f z c k
  in

  go_kont f z t

let fold_right
: type a z. (a -> z -> z) -> a t -> z -> z
= fun f (T t) z ->

  let fold_prefix
  : type b c. (b -> z -> z) -> (b, c) prefix -> z -> z
  = fun f p z ->
    match p with
  | P2 (a, b) -> f a (f b z)
  | P3 (a, b, c) -> f a (f b (f c z))
  | P4 (a, b, c, d, deq) ->
      let z = Dequeue.fold_right f deq z in
      f a (f b (f c (f d z)))
  in

  let list_of_suffix f s z = Dequeue.fold_right f s z in

  let rec go
  : type b1 b2 c1 c2 k.
    (b1 -> z -> z) -> (b1, b2, c1, k) steque -> z -> (b2, c2) kont -> z
  = fun f steque z kont ->
    match steque with
    | KONT -> go_kont f kont z
    | Triple (p, c, s) ->
        let z = list_of_suffix f s z in
        let z = go (go_pair f) c z kont in
        fold_prefix f p z

  and go_pair
  : type b. (b -> z -> z) -> b pair -> z -> z
  = fun f (Pair (p, k)) z ->
    let z = go_kont (go_pair f) k z in
    fold_prefix f p z

  and go_kont
  : type b c. (b -> z -> z) -> (b, c) kont -> z -> z
  = fun f kont z ->
    match kont with
    | Suffix s -> list_of_suffix f s z
    | Y (c, k) -> go f c z k
    | Yr (c, k) -> go f c z k
    | R (c, k) -> go f c z k
    | G (c, k) -> go f c z k
  in

  go_kont f t z

let rev t = fold_left (fun t x -> cons x t) empty t

let of_dequeue d = T (Suffix d)

let make n x = of_dequeue (Dequeue.make n x)

let singleton x = of_dequeue (Dequeue.singleton x)
