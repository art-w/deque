module type DEQUE = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val cons : 'a -> 'a t -> 'a t
  val uncons : 'a t -> ('a * 'a t) option
  val snoc : 'a t -> 'a -> 'a t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val rev : 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t
end

module type DEQUE_CAT = sig
  include DEQUE
  val append : 'a t -> 'a t -> 'a t
  val rev : 'a t -> 'a t
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
    D.fold_left (fun ys x -> D.snoc ys (f x)) D.empty t

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
    D.fold_left
      (fun ys x ->
        match f x with
        | None -> ys
        | Some y -> D.snoc ys y)
      D.empty
      t

  let fold_left_map f z t =
    D.fold_left
      (fun (z, ys) x ->
        let z, y = f z x in
        z, D.snoc ys y)
      (z, D.empty)
      t

  exception Abort

  let exists p t =
    try iter (fun x -> if p x then raise Abort) t ; false
    with Abort -> true

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
    D.fold_left
      (fun ys x -> if f x then D.snoc ys x else ys)
      D.empty
      t

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

  let to_seq t = Seq.unfold D.uncons t

  let of_seq s = Seq.fold_left (fun xs x -> D.snoc xs x) D.empty s

  let init n f =
    let rec go acc i =
      if i >= n
      then acc
      else let x = f i in
           go (D.snoc acc x) (i + 1)
    in
    go D.empty 0

  let to_array t =
    match D.uncons t with
    | None -> [| |]
    | Some (x, t) ->
      let n = length t in
      let arr = Array.make (n + 1) x in
      iteri (fun i x -> arr.(i + 1) <- x) t ;
      arr

  let of_array t =
    init (Array.length t) (Array.get t)

  let sort cmp t =
    let t = to_array t in
    Array.sort cmp t ;
    of_array t

  let stable_sort cmp t =
    let t = to_array t in
    Array.stable_sort cmp t ;
    of_array t

  let fast_sort cmp t =
    let t = to_array t in
    Array.fast_sort cmp t ;
    of_array t

  let sort_uniq cmp t =
    of_list @@ List.sort_uniq cmp @@ to_list t


  let fold_left2 ~exn f z xs ys =
    let z, ys =
      D.fold_left
        (fun (z, ys) x ->
          match D.uncons ys with
          | None -> raise exn
          | Some (y, ys) ->
              f z x y, ys)
        (z, ys)
        xs
    in
    if D.is_empty ys
    then z
    else raise exn

  let iter2 f xs ys =
    fold_left2 ~exn:(Invalid_argument "Deque.iter2")
      (fun () x y -> f x y)
      ()
      xs
      ys

  let map2 f xs ys =
    fold_left2 ~exn:(Invalid_argument "Deque.map2")
      (fun t x y -> D.snoc t (f x y))
      D.empty
      xs
      ys

  let rev_map2 f xs ys =
    fold_left2 ~exn:(Invalid_argument "Deque.rev_map2")
      (fun t x y -> D.cons (f x y) t)
      D.empty
      xs
      ys

  let exists2 p xs ys =
    try fold_left2 ~exn:(Invalid_argument "Deque.exists2")
          (fun b x y -> if p x y then raise Abort else b)
          false
          xs
          ys
    with Abort -> true

  let for_all2 p xs ys =
    try fold_left2 ~exn:(Invalid_argument "Deque.for_all2")
          (fun b x y -> if p x y then b else raise Abort)
          true
          xs
          ys
    with Abort -> false

  let combine xs ys =
    fold_left2 ~exn:(Invalid_argument "Deque.combine")
      (fun t x y -> D.snoc t (x, y))
      D.empty
      xs
      ys

  let fold_left2 f z xs ys =
    fold_left2 ~exn:(Invalid_argument "Deque.fold_left2") f z xs ys


  exception Return of int

  let compare cmp xs ys =
    try let ys =
          D.fold_left
            (fun ys x ->
              match D.uncons ys with
              | None -> raise (Return (-1))
              | Some (y, ys) ->
                  match cmp x y with
                  | 0 -> ys
                  | n -> raise (Return n))
            ys
            xs
        in
        if D.is_empty ys
        then 0
        else 1
    with Return n -> n


  let ( @ ) = D.append

  let rev_append xs ys = D.rev xs @ ys

  let concat xss = D.fold_left (fun z x -> x @ z) D.empty xss

  let flatten = concat

  let concat_map f t =
    D.fold_left (fun ys x -> ys @ f x) D.empty t

end
