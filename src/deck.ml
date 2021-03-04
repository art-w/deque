include Deck_internal

let append a b = concat a b

let rec fold_left
: type a acc. (acc -> a -> acc) -> acc -> a t -> acc
= fun f acc (Regular t) -> fold_st f acc t

and fold_st
: type a acc c. (acc -> a -> acc) -> acc -> (a, c) st -> acc
= fun f acc t ->
  match t with
  | Void -> acc
  | T deq -> fold_deque f acc deq

and fold_deque
: type a acc c. (acc -> a -> acc) -> acc -> (a, c) deque -> acc
= fun f acc deq ->
  match deq with
  | Only_path p -> fold_path f acc p
  | Pair_red (left, right) ->
      let acc = fold_path f acc left in
      fold_path f acc right
  | Pair_green (left, right) ->
      let acc = fold_path f acc left in
      fold_path f acc right

and fold_nedeque
: type acc a b c p h.
     (acc -> a -> acc)
  -> acc
  -> (a, b, p, h) not_empty
  -> (b, b, c, h, nh, nh, nh) triple
  -> acc
= fun f acc deq kont ->
  match deq with
  | Only_of p -> fold_triple_h f acc p kont
  | Pair_left (left, right) ->
      let acc = fold_triple_h f acc left kont in
      fold_path f acc right
  | Pair_right (left, right) ->
      let acc = fold_path f acc left in
      fold_triple_h f acc right kont

and fold_path
: type acc a c k. (acc -> a -> acc) -> acc -> (a, c, k) path -> acc
= fun f acc (Path (h, k)) ->
  fold_triple_h f acc h k

and fold_triple_h
: type acc a b c c2 k hl h.
    (acc -> a -> acc)
  -> acc
  -> (a, b, c, k, hl, h, has_hole) triple
  -> (b, b, c2, hl, nh, nh, nh) triple
  -> acc
= fun f acc triple kont ->
  match triple with
  | HOLE -> fold_triple_nh f acc kont
  | Only_yellow (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_nedeque (fold_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Only_orange (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_nedeque (fold_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Left_yellow (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_nedeque (fold_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Left_orange (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_nedeque (fold_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Right_yellow (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_nedeque (fold_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | Right_orange (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_nedeque (fold_stored_triple f) acc c kont in
      buffer_fold_left f acc s
  | _ -> .

and buffer_fold_left
: type acc a s.
     (acc -> a -> acc)
  -> acc
  -> (a, s) Buffer.t
  -> acc
= fun f acc t ->
  Dequeue.fold_left f acc (Buffer.to_dequeue t)

and fold_triple_nh
: type acc a b c k.
    (acc -> a -> acc)
  -> acc
  -> (a, b, c, k, nh, nh, nh) triple
  -> acc
= fun f acc triple ->
  match triple with
  | Only_prefix p ->
      buffer_fold_left f acc p
  | Only_green (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_deque (fold_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Only_red (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_deque (fold_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Left_small (p, s) ->
      let acc = buffer_fold_left f acc p in
      buffer_fold_left f acc s
  | Left_green (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_deque (fold_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Left_red (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_deque (fold_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Right_small (p, s) ->
      let acc = buffer_fold_left f acc p in
      buffer_fold_left f acc s
  | Right_green (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_deque (fold_stored_triple f) acc c in
      buffer_fold_left f acc s
  | Right_red (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_deque (fold_stored_triple f) acc c in
      buffer_fold_left f acc s

and fold_stored_triple
: type acc a. (acc -> a -> acc) -> acc -> a stored_triple -> acc
= fun f acc stored ->
  match stored with
  | Stored_prefix p -> buffer_fold_left f acc p
  | Stored (p, c, s) ->
      let acc = buffer_fold_left f acc p in
      let acc = fold_st (fold_stored_triple f) acc c in
      buffer_fold_left f acc s


let rec fold_right
: type a acc. (a -> acc -> acc) -> a t -> acc -> acc
= fun f (Regular t) acc -> fold_st f t acc

and fold_st
: type a acc c. (a -> acc -> acc) -> (a, c) st -> acc -> acc
= fun f t acc ->
  match t with
  | Void -> acc
  | T deq -> fold_deque f deq acc

and fold_deque
: type a acc c. (a -> acc -> acc) -> (a, c) deque -> acc -> acc
= fun f deq acc ->
  match deq with
  | Only_path p -> fold_path f p acc
  | Pair_red (left, right) ->
      let acc = fold_path f right acc in
      fold_path f left acc
  | Pair_green (left, right) ->
      let acc = fold_path f right acc in
      fold_path f left acc

and fold_nedeque
: type acc a b c p h.
     (a -> acc -> acc)
  -> (a, b, p, h) not_empty
  -> (b, b, c, h, nh, nh, nh) triple
  -> acc
  -> acc
= fun f deq kont acc ->
  match deq with
  | Only_of p -> fold_triple_h f p kont acc
  | Pair_left (left, right) ->
      let acc = fold_path f right acc in
      fold_triple_h f left kont acc
  | Pair_right (left, right) ->
      let acc = fold_triple_h f right kont acc in
      fold_path f left acc

and fold_path
: type acc a c k. (a -> acc -> acc) -> (a, c, k) path -> acc -> acc
= fun f (Path (h, k)) acc ->
  fold_triple_h f h k acc

and fold_triple_h
: type acc a b c c2 k hl h.
    (a -> acc -> acc)
  -> (a, b, c, k, hl, h, has_hole) triple
  -> (b, b, c2, hl, nh, nh, nh) triple
  -> acc
  -> acc
= fun f triple kont acc ->
  match triple with
  | HOLE -> fold_triple_nh f kont acc
  | Only_yellow (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_nedeque (fold_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Only_orange (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_nedeque (fold_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Left_yellow (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_nedeque (fold_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Left_orange (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_nedeque (fold_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Right_yellow (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_nedeque (fold_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | Right_orange (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_nedeque (fold_stored_triple f) c kont acc in
      buffer_fold_right f p acc
  | _ -> .

and buffer_fold_right
: type acc a s.
     (a -> acc -> acc)
  -> (a, s) Buffer.t
  -> acc
  -> acc
= fun f t acc ->
  Dequeue.fold_right f (Buffer.to_dequeue t) acc

and fold_triple_nh
: type acc a b c k.
    (a -> acc -> acc)
  -> (a, b, c, k, nh, nh, nh) triple
  -> acc
  -> acc
= fun f triple acc ->
  match triple with
  | Only_prefix p ->
      buffer_fold_right f p acc
  | Only_green (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_deque (fold_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Only_red (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_deque (fold_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Left_small (p, s) ->
      let acc = buffer_fold_right f s acc in
      buffer_fold_right f p acc
  | Left_green (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_deque (fold_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Left_red (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_deque (fold_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Right_small (p, s) ->
      let acc = buffer_fold_right f s acc in
      buffer_fold_right f p acc
  | Right_green (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_deque (fold_stored_triple f) c acc in
      buffer_fold_right f p acc
  | Right_red (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_deque (fold_stored_triple f) c acc in
      buffer_fold_right f p acc

and fold_stored_triple
: type acc a. (a -> acc -> acc) -> a stored_triple -> acc -> acc
= fun f stored acc ->
  match stored with
  | Stored_prefix p -> buffer_fold_right f p acc
  | Stored (p, c, s) ->
      let acc = buffer_fold_right f s acc in
      let acc = fold_st (fold_stored_triple f) c acc in
      buffer_fold_right f p acc

let rev t = fold_left (fun t x -> cons x t) empty t
