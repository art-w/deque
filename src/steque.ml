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

let to_list t = List.rev (fold_left (fun z x -> x :: z) [] t)

let length t = fold_left (fun z _ -> z + 1) 0 t
