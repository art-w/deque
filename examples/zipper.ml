type 'a t =
  { left : 'a Deque.t
  ; focus : 'a
  ; right : 'a Deque.t
  }

let of_deque deq =
  match Deque.uncons deq with
  | None -> None
  | Some (focus, right) -> Some { left = Deque.empty ; focus ; right }

let focus t = t.focus

let go_right t = match Deque.uncons t.right with
  | None -> None
  | Some (focus, right) ->
      Some { left = Deque.snoc t.left t.focus ; focus ; right }

let go_left t = match Deque.unsnoc t.left with
  | None -> None
  | Some (left, focus) ->
      Some { left ; focus ; right = Deque.cons t.focus t.right }

let to_deque t = Deque.(t.left @ cons t.focus t.right)
