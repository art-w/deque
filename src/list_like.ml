module type DEQUE = sig
  type 'a t
  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
  val uncons : 'a t -> ('a * 'a t) option
  val snoc : 'a t -> 'a -> 'a t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Make (D : DEQUE) = struct

  let length t = D.fold_left (fun s _ -> s + 1) 0 t

  let hd t = match D.uncons t with
    | None -> failwith "hd"
    | Some (x, _) -> x

  let tl t = match D.uncons t with
    | None -> failwith "tl"
    | Some (_, t) -> t

  let iter f t = D.fold_left (fun () x -> f x) () t

  let iteri f t =
    let _ = D.fold_left (fun i x -> f i x ; i + 1) 0 t in
    ()

  let map f t =
    D.fold_right (fun x ys -> D.cons (f x) ys) t D.empty

  let mapi f t =
    let _, t =
      D.fold_left
        (fun (i, ys) x -> i + 1, D.snoc ys (f i x))
        (0, D.empty)
        t
    in t

  let rev_map f t =
    D.fold_left (fun ys x -> D.cons (f x) ys) D.empty t

  let filter_map f t =
    D.fold_right
      (fun x ys ->
        match f x with
        | None -> ys
        | Some y -> D.cons y ys)
      t
      D.empty

  let fold_left_map f z t =
    D.fold_left
      (fun (z, ys) x ->
        let z, y = f z x in
        z, D.snoc ys y)
      (z, D.empty)
      t

  exception Abort

  let exists p t =
    try iter (fun x -> if p x then raise Abort) t ; true
    with Abort -> false

  let for_all p t = not (exists (fun x -> not (p x)) t)

  let mem x t = exists (( = ) x) t
  let memq x t = exists (( == ) x) t

  let find (type a) p t =
    let module M = struct exception Found of a end in
    try iter (fun x -> if p x then raise (M.Found x)) t ;
        raise Not_found
    with M.Found x -> x

  let find_opt (type a) p t =
    let module M = struct exception Found of a end in
    try iter (fun x -> if p x then raise (M.Found x)) t ;
        None
    with M.Found x -> Some x

  let find_map (type a) f t =
    let module M = struct exception Found of a end in
    let g x = match f x with None -> () | Some y -> raise (M.Found y) in
    try iter g t ;
        None
    with M.Found x -> Some x

  let filter f t =
    D.fold_right
      (fun x ys ->
        if f x then D.cons x ys else ys)
      t
      D.empty

  let find_all = filter

  let filteri f t =
    let _, t =
      D.fold_left
        (fun (i, ys) x ->
          i + 1, if f i x then D.snoc ys x else ys)
        (0, D.empty)
        t
    in t

  let partition f t =
    D.fold_left
      (fun (left, right) x ->
        if f x
        then D.snoc left x, right
        else left, D.snoc right x)
      (D.empty, D.empty)
      t

  let assoc k t =
    let _, y = find (fun (x, _) -> x = k) t in
    y

  let assoc_opt k t =
    find_map (fun (x, y) -> if x = k then Some y else None) t

  let assq k t =
    let _, y = find (fun (x, _) -> x == k) t in
    y

  let assq_opt k t =
    find_map (fun (x, y) -> if x == k then Some y else None) t

  let mem_assoc x t = exists (fun (x', _) -> x = x') t
  let mem_assq x t = exists (fun (x', _) -> x == x') t

  let split t =
    D.fold_left
      (fun (left, right) (x, y) -> D.snoc left x, D.snoc right y)
      (D.empty, D.empty)
      t

  let to_list t = D.fold_right (fun x xs -> x :: xs) t []

  let of_list xs = List.fold_left (fun t x -> D.snoc t x) D.empty xs
end
