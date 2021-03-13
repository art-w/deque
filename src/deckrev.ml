include Deckrev_internal

let append a b = concat a b

let rec fold_left
: type a acc. (acc -> a Buffer.elt -> acc) -> acc -> a t -> acc
= fun f acc (Regular t) -> fold_left_st f acc t

and fold_left_st
: type a acc c. (acc -> a Buffer.elt -> acc) -> acc -> (a, c) st -> acc
= fun f acc t ->
  match t with
  | Void -> acc
  | T deq -> fold_left_deque f acc deq
  | Rev deq -> fold_right_deque (fun x z -> f z x) deq acc

and fold_left_deque
: type a acc c. (acc -> a Buffer.elt -> acc) -> acc -> (a, c) deque -> acc
= fun f acc deq ->
  match deq with
  | Only_path p -> fold_left_path f acc p
  | Pair_red (left, right) ->
      let acc = fold_left_path f acc left in
      fold_left_path f acc right
  | Pair_green (left, right) ->
      let acc = fold_left_path f acc left in
      fold_left_path f acc right

and fold_left_nedeque
: type acc a b c p h1 h2 r1 r2.
     (acc -> a Buffer.elt -> acc)
  -> acc
  -> (a, b, p, h1*h2) not_empty
  -> (b, b, c, h1*h2, nh, nh, nh, r1 * r2) triple
  -> acc
= fun f acc deq kont ->
  match deq with
  | Only_of p ->
      fold_left_triple_h f acc p kont
  | Pair_left (left, right) ->
      let acc = fold_left_triple_h f acc left kont in
      fold_left_path f acc right
  | Pair_right (left, right) ->
      let acc = fold_left_path f acc left in
      fold_left_triple_h f acc right kont
  | Pair_right_sym (left, right) ->
      let acc = fold_left_triple_h f acc left kont in
      fold_left_path f acc right
  | Pair_left_sym (left, right) ->
      let acc = fold_left_path f acc left in
      fold_left_triple_h f acc right kont

and fold_left_path
: type acc a c k. (acc -> a Buffer.elt -> acc) -> acc -> (a, c, k) path -> acc
= fun f acc (Path (h, k)) ->
  fold_left_triple_h f acc h k

and fold_left_triple_h
: type acc a b c c2 k h1 h2 h r1 r2 r3 r4.
    (acc -> a Buffer.elt -> acc)
  -> acc
  -> (a, b, c, k, h1*h2, h, has_hole, r1*r2) triple
  -> (b, b, c2, h1*h2, nh, nh, nh, r3*r4) triple
  -> acc
= fun f acc triple kont ->
  match triple_not_rev triple with
  | HOLE -> fold_left_triple_nh f acc kont
  | Only_yellow (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_nedeque (fold_left_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Only_orange (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_nedeque (fold_left_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Left_yellow (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_nedeque (fold_left_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Left_orange (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_nedeque (fold_left_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Right_yellow (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_nedeque (fold_left_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Right_orange (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_nedeque (fold_left_stored_triple f) acc c kont in
      buffer_fold_left f acc s
      (*
  | Triple_rev triple ->
      fold_right_triple_h f triple kont acc
      *)

and buffer_fold_left
: type acc a s.
     (acc -> a Buffer.elt -> acc)
  -> acc
  -> (a, s) Buffer.t
  -> acc
= fun f acc t ->
  let deq = Buffer.to_dequeue t in
  Dequeue.fold_left (fun z x -> f z (Buffer.elt_out x t)) acc deq

and fold_left_triple_nh
: type acc a b c k r.
    (acc -> a Buffer.elt -> acc)
  -> acc
  -> (a, b, c, k, nh, nh, nh, r) triple
  -> acc
= fun f acc triple ->
  match triple_not_rev triple with
  | Only_prefix p ->
      buffer_fold_left f acc p
  | Only_green (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_deque (fold_left_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Only_red (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_deque (fold_left_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Left_small (p, s) ->
      let acc = buffer_fold_left f acc p in
      buffer_fold_left f acc s
  | Left_green (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_deque (fold_left_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Left_red (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_deque (fold_left_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Right_small (p, s) ->
      let acc = buffer_fold_left f acc p in
      buffer_fold_left f acc s
  | Right_green (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_deque (fold_left_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Right_red (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_deque (fold_left_stored_triple f) acc c in
      buffer_fold_left f acc s

and fold_left_stored_triple
: type acc a.
     (acc -> a Buffer.elt -> acc)
  -> acc
  -> a stored_triple Buffer.elt
  -> acc
= fun f acc stored ->
  match stored_elt stored with
  | Stored_prefix p -> buffer_fold_left f acc p
  | Stored (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_left_st (fold_left_stored_triple f) acc c in
      buffer_fold_left f acc s


and fold_right
: type a acc. (a Buffer.elt -> acc -> acc) -> a t -> acc -> acc
= fun f (Regular t) acc -> fold_right_st f t acc

and fold_right_st
: type a acc c. (a Buffer.elt -> acc -> acc) -> (a, c) st -> acc -> acc
= fun f t acc ->
  match t with
  | Void -> acc
  | T deq -> fold_right_deque f deq acc
  | Rev deq -> fold_left_deque (fun z x -> f x z) acc deq

and fold_right_deque
: type a acc c. (a Buffer.elt -> acc -> acc) -> (a, c) deque -> acc -> acc
= fun f deq acc ->
  match deq with
  | Only_path p -> fold_right_path f p acc
  | Pair_red (left, right) ->
      let acc = fold_right_path f right acc in
      fold_right_path f left acc
  | Pair_green (left, right) ->
      let acc = fold_right_path f right acc in
      fold_right_path f left acc

and fold_right_nedeque
: type acc a b c p h1 h2 r1 r2.
     (a Buffer.elt -> acc -> acc)
  -> (a, b, p, h1*h2) not_empty
  -> (b, b, c, h1*h2, nh, nh, nh, r1*r2) triple
  -> acc
  -> acc
= fun f deq kont acc ->
  match deq with
  | Only_of p -> fold_right_triple_h f p kont acc
  | Pair_left (left, right) ->
      let acc = fold_right_path f right acc in
      fold_right_triple_h f left kont acc
  | Pair_right (left, right) ->
      let acc = fold_right_triple_h f right kont acc in
      fold_right_path f left acc
  | Pair_right_sym (left, right) ->
      let acc = fold_right_path f right acc in
      fold_right_triple_h f left kont acc
  | Pair_left_sym (left, right) ->
      let acc = fold_right_triple_h f right kont acc in
      fold_right_path f left acc

and fold_right_path
: type acc a c k. (a Buffer.elt -> acc -> acc) -> (a, c, k) path -> acc -> acc
= fun f (Path (h, k)) acc ->
  fold_right_triple_h f h k acc

and fold_right_triple_h
: type acc a b c c2 k h1 h2 h r1 r2 r3 r4.
     (a Buffer.elt -> acc -> acc)
  -> (a, b, c, k, h1*h2, h, has_hole, r1*r2) triple
  -> (b, b, c2, h1*h2, nh, nh, nh, r3*r4) triple
  -> acc
  -> acc
= fun f triple kont acc ->
  match triple_not_rev triple with
  | HOLE ->
      fold_right_triple_nh f kont acc
  | Only_yellow (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_nedeque (fold_right_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Only_orange (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_nedeque (fold_right_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Left_yellow (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_nedeque (fold_right_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Left_orange (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_nedeque (fold_right_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Right_yellow (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_nedeque (fold_right_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Right_orange (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_nedeque (fold_right_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | _ -> .


and buffer_fold_right
: type acc a s.
     (a Buffer.elt -> acc -> acc)
  -> (a, s) Buffer.t
  -> acc
  -> acc
= fun f t acc ->
  let deq = Buffer.to_dequeue t in
  Dequeue.fold_right (fun x z -> f (Buffer.elt_out x t) z) deq acc

and fold_right_triple_nh
: type acc a b c k r1 r2.
     (a Buffer.elt -> acc -> acc)
  -> (a, b, c, k, nh, nh, nh, r1 * r2) triple
  -> acc
  -> acc
= fun f triple acc ->
  match triple_not_rev triple with
  | Only_prefix p ->
      buffer_fold_right f p acc
  | Only_green (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_deque (fold_right_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Only_red (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_deque (fold_right_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Left_small (p, s) ->
      let acc = buffer_fold_right f s acc in
      buffer_fold_right f p acc
  | Left_green (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_deque (fold_right_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Left_red (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_deque (fold_right_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Right_small (p, s) ->
      let acc = buffer_fold_right f s acc in
      buffer_fold_right f p acc
  | Right_green (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_deque (fold_right_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Right_red (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_deque (fold_right_stored_triple f) c acc in
      buffer_fold_right f p acc

and fold_right_stored_triple
: type acc a.
     (a Buffer.elt -> acc -> acc)
  -> a stored_triple Buffer.elt
  -> acc
  -> acc
= fun f stored acc ->
  match stored_elt stored with
  | Stored_prefix p -> buffer_fold_right f p acc
  | Stored (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_right_st (fold_right_stored_triple f) c acc in
      buffer_fold_right f p acc


let fold_left f z t =
  fold_left  (fun z x -> f z (unelt x)) z t

let fold_right f t z =
  fold_right (fun x z -> f (unelt x) z) t z


let of_buffer d =
  match Buffer.of_dequeue d with
  | Buffer.Exact_0 -> Regular Void
  | Buffer.Lte1 d -> Regular (T (Only_path (Path (HOLE, Only_prefix d))))

let of_dequeue d =
  let d = Dequeue.map (fun x -> Buffer.L2R x) d in
  of_buffer d

let make n x = of_buffer (Dequeue.make n (Buffer.L2R x))
