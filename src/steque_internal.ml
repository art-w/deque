module Deque = Dequeue_internal
type 'a suffix = 'a Deque.t

type (_, _) prefix =
  | P2 : 'a * 'a                       -> ('a, [`red]) prefix
  | P3 : 'a * 'a * 'a                  -> ('a, [`yellow]) prefix
  | P4 : 'a * 'a * 'a * 'a * 'a suffix -> ('a, [`green]) prefix

type is_kont = IS_KONT
type is_not_kont = IS_NOT_KONT

type (_, _, _, _) steque =
  | KONT   : ('a, 'a, [`yellow], is_kont) steque
  | Triple : ('a, 'c) prefix
           * ('a pair, 'b, [`yellow], _) steque
           * 'a suffix
          -> ('a, 'b, 'c, is_not_kont) steque

and 'a pair =
  Pair : ('a, _) prefix * ('a pair, _) kont -> 'a pair

and (_, _) kont =
  | Suffix : 'a Deque.t -> ('a, [`green]) kont
  | R  : ('a, 'b, [`red], is_not_kont) steque
       * ('b, [`green]) kont
      -> ('a, [`red]) kont
  | Y  : ('a, 'b, [`yellow], is_not_kont) steque
       * ('b, [`green]) kont
      -> ('a, [`yellow]) kont
  | G  : ('a, 'b, [`green ], is_not_kont) steque
       * ('b, [< `green | `red]) kont
      -> ('a, [`green]) kont
  | Yr : ('a, 'b, [`yellow], is_not_kont) steque
       * ('b, [`red]) kont
      -> ('a, [`orange]) kont

and _ t = T : ('a, [< `yellow | `green ]) kont -> 'a t

type _ any_kont = Any_kont : ('a, _) kont -> 'a any_kont

let empty_kont = Suffix Deque.empty

let empty = T empty_kont

let is_empty = function
  | T (Suffix d) -> Deque.is_empty d
  | _ -> false

let green
: type a c.
  (a, [`green]) prefix -> (a pair, c) kont -> a suffix -> (a, [`green]) kont
= fun p kont s ->
  match kont with
  | Suffix small -> G (Triple (p, KONT, s), Suffix small)
  | Y (triple, k) -> G (Triple (p, triple, s), k)
  | Yr (triple, k) -> G (Triple (p, triple, s), k)
  | G (triple, k) -> G (Triple (p, KONT, s), G (triple, k))
  | R (triple, k) -> G (Triple (p, KONT, s), R (triple, k))

let yellow
: type a.
  (a, [`yellow]) prefix -> (a pair) t -> a suffix -> (a, [`yellow]) kont
= fun p (T kont) s ->
  match kont with
  | Y (child, k)    -> Y (Triple (p, child, s), k)
  | (Suffix _) as k -> Y (Triple (p, KONT,  s), k)
  | G (g, k)        -> Y (Triple (p, KONT,  s), G (g, k))

let orange
: type a. (a, [`yellow]) prefix -> (a pair) any_kont -> a suffix -> a any_kont
= fun p (Any_kont kont) s ->
  match kont with
  | Y  (child, k)   -> Any_kont (Y  (Triple (p, child, s), k))
  | Yr (child, k)   -> Any_kont (Yr (Triple (p, child, s), k))
  | (G _)      as k -> Any_kont (Y  (Triple (p, KONT, s), k))
  | (Suffix _) as k -> Any_kont (Y  (Triple (p, KONT, s), k))
  | (R _)      as k -> Any_kont (Yr (Triple (p, KONT, s), k))

let red
: type a. (a, [`red]) prefix -> (a pair) t -> a suffix -> (a, [`red]) kont
= fun p (T kont) s ->
  match kont with
  | Y (child, k)    -> R (Triple (p, child, s), k)
  | (Suffix _) as k -> R (Triple (p, KONT,  s), k)
  | G (g, k)        -> R (Triple (p, KONT,  s), G (g, k))

let green_prefix_cons
: type a. a -> (a, [`green]) prefix -> (a, [`green]) prefix
= fun x -> function
  | P4 (a, b, c, d, deq) -> P4 (x, a, b, c, Deque.cons d deq)

let red_prefix_cons
: type a. a -> (a, [`red]) prefix -> (a, [`yellow]) prefix
= fun x -> function
  | P2 (a, b) -> P3 (x, a, b)

let yellow_prefix_cons
: type a. a -> (a, [`yellow]) prefix -> (a, [`green]) prefix
= fun x -> function
  | P3 (a, b, c) -> P4 (x, a, b, c, Deque.empty)

type _ yg_prefix = Any : ('a, [< `yellow | `green]) prefix -> 'a yg_prefix

let prefix_cons
: type a c. a -> (a, c) prefix -> a yg_prefix
= fun x p ->
  match p with
  | P2 (a, b) -> Any (P3 (x, a, b))
  | P3 (a, b, c) -> Any (P4 (x, a, b, c, Deque.empty))
  | P4 (a, b, c, d, deq) -> Any (P4 (x, a, b, c, Deque.cons d deq))

let cons'
: type a c. a -> (a, c) kont -> a t
= fun x t ->
  match t with
  | Suffix s ->
      T (Suffix (Deque.cons x s))
  | Yr (Triple (p, child, s), kont) ->
      let p = yellow_prefix_cons x p in
      T (G (Triple (p, child, s), kont))
  | Y (Triple (p, child, s), kont) ->
      let p = yellow_prefix_cons x p in
      T (G (Triple (p, child, s), kont))
  | G (Triple (p, child, s), kont) ->
      let p = green_prefix_cons x p in
      T (G (Triple (p, child, s), kont))
  | R (Triple (p, child, s), kont) ->
      let p = red_prefix_cons x p in
      T (Y (Triple (p, child, s), kont))

let cons_any
: type a. a -> a any_kont -> a any_kont
= fun x (Any_kont t) ->
  let T t = cons' x t in
  Any_kont t

let cons_any'
: type a. a -> a any_kont -> a t
= fun x (Any_kont t) -> cons' x t

let cons
: type a. a -> a t -> a t
= fun x (T t) -> cons' x t

let snoc_any
: type a. a -> a any_kont -> a any_kont
= fun x (Any_kont t) ->
  match t with
  | Suffix s ->
      Any_kont (Suffix (Deque.snoc s x))
  | Yr (Triple (p, child, s), kont) ->
      let s = Deque.snoc s x in
      Any_kont (Yr (Triple (p, child, s), kont))
  | Y (Triple (p, child, s), kont) ->
      let s = Deque.snoc s x in
      Any_kont (Y (Triple (p, child, s), kont))
  | G (Triple (p, child, s), kont) ->
      let s = Deque.snoc s x in
      Any_kont (G (Triple (p, child, s), kont))
  | R (Triple (p, child, s), kont) ->
      let s = Deque.snoc s x in
      Any_kont (R (Triple (p, child, s), kont))

let snoc
: type a. a t -> a -> a t
= fun (T t) x ->
  match t with
  | Suffix s ->
      T (Suffix (Deque.snoc s x))
  | Y (Triple (p, child, s), kont) ->
      let s = Deque.snoc s x in
      T (Y (Triple (p, child, s), kont))
  | G (Triple (p, child, s), kont) ->
      let s = Deque.snoc s x in
      T (G (Triple (p, child, s), kont))


let join_any
: type a c.
  (a pair, c) kont -> a suffix -> a any_kont -> a pair any_kont * a suffix
= fun child s ty ->
  let child, Any_kont y =
    match Deque.uncons s with
    | None -> Any_kont child, ty
    | Some (x, s) ->
        match Deque.uncons s with
        | None -> Any_kont child, cons_any x ty
        | Some (y, s) ->
            let prefix = match Deque.uncons s with
              | None ->
                  Pair (P2 (x, y), empty_kont)
              | Some (z, s) ->
                  match Deque.uncons s with
                  | None -> Pair (P3 (x, y, z), empty_kont)
                  | Some (w, s) ->
                      Pair (P4 (x, y, z, w, s), empty_kont)
            in
            snoc_any prefix (Any_kont child), ty
  in
  match y with
  | Suffix y_suffix ->
      child, y_suffix
  | Y (Triple (y_prefix, Triple (x, y, z), y_suffix), y_kont) ->
      let y_child = Triple (x, y, z) in
      let yp = Pair (y_prefix, Y (y_child, y_kont)) in
      snoc_any yp child, y_suffix
  | Y (Triple (y_prefix, KONT, y_suffix), y_kont) ->
      let yp = Pair (y_prefix, y_kont) in
      snoc_any yp child, y_suffix

  | Yr (Triple (y_prefix, Triple (x, y, z), y_suffix), y_kont) ->
      let y_child = Triple (x, y, z) in
      let yp = Pair (y_prefix, Yr (y_child, y_kont)) in
      snoc_any yp child, y_suffix
  | Yr (Triple (y_prefix, KONT, y_suffix), y_kont) ->
      let yp = Pair (y_prefix, y_kont) in
      snoc_any yp child, y_suffix

  | R (Triple (y_prefix, Triple (x, y, z), y_suffix), y_kont) ->
      let y_child = Triple (x, y, z) in
      let yp = Pair (y_prefix, Y (y_child, y_kont)) in
      snoc_any yp child, y_suffix
  | R (Triple (y_prefix, KONT, y_suffix), y_kont) ->
      let yp = Pair (y_prefix, y_kont) in
      snoc_any yp child, y_suffix

  | G (Triple (y_prefix, Triple (x, y, z), y_suffix), y_kont) ->
      let y_child = Triple (x, y, z) in
      let Any_kont y_child = match y_kont with
        | R (r, k) -> Any_kont (Yr (y_child, R (r, k)))
        | G (g, k) -> Any_kont (Y (y_child, G (g, k)))
        | Suffix small -> Any_kont (Y (y_child, Suffix small))
      in
      let yp = Pair (y_prefix, y_child) in
      snoc_any yp child, y_suffix
  | G (Triple (y_prefix, KONT, y_suffix), y_kont) ->
      let yp = Pair (y_prefix, y_kont) in
      snoc_any yp child, y_suffix

let join_t
: type a. (a pair) t -> a suffix -> a any_kont -> a pair t * a suffix
= fun child s ty ->
  let child, Any_kont y =
    match Deque.uncons s with
    | None -> child, ty
    | Some (x, s) ->
        match Deque.uncons s with
        | None -> child, cons_any x ty
        | Some (y, s) ->
            let prefix = match Deque.uncons s with
              | None -> Pair (P2 (x, y), empty_kont)
              | Some (z, s) ->
                  match Deque.uncons s with
                  | None -> Pair (P3 (x, y, z), empty_kont)
                  | Some (w, s) -> Pair (P4 (x, y, z, w, s), empty_kont)
            in
            snoc child prefix, ty
  in
  match y with
  | Suffix y_suffix -> child, y_suffix
  | Y (Triple (y_prefix, Triple (x, y, z), y_suffix), y_kont) ->
      let y_child = Triple (x, y, z) in
      let yp = Pair (y_prefix, Y (y_child, y_kont)) in
      snoc child yp, y_suffix
  | Y (Triple (y_prefix, KONT, y_suffix), y_kont) ->
      let yp = Pair (y_prefix, y_kont) in
      snoc child yp, y_suffix

  | G (Triple (y_prefix, Triple (x, y, z), y_suffix), y_kont) ->
      let y_child = Triple (x, y, z) in
      let Any_kont y_child = match y_kont with
        | R (r, k) -> Any_kont (Yr (y_child, R (r, k)))
        | G (g, k) -> Any_kont (Y (y_child, G (g, k)))
        | Suffix small -> Any_kont (Y (y_child, Suffix small))
      in
      let yp = Pair (y_prefix, y_child) in
      snoc child yp, y_suffix
  | G (Triple (y_prefix, KONT, y_suffix), y_kont) ->
      let yp = Pair (y_prefix, y_kont) in
      snoc child yp, y_suffix

  | Yr (Triple (y_prefix, Triple (x, y, z), y_suffix), y_kont) ->
      let y_child = Triple (x, y, z) in
      let yp = Pair (y_prefix, Yr (y_child, y_kont)) in
      snoc child yp, y_suffix
  | Yr (Triple (y_prefix, KONT, y_suffix), y_kont) ->
      let yp = Pair (y_prefix, y_kont) in
      snoc child yp, y_suffix

  | R (Triple (y_prefix, Triple (x, y, z), y_suffix), y_kont) ->
      let y_child = Triple (x, y, z) in
      let yp = Pair (y_prefix, Y (y_child, y_kont)) in
      snoc child yp, y_suffix
  | R (Triple (y_prefix, KONT, y_suffix), y_kont) ->
      let yp = Pair (y_prefix, y_kont) in
      snoc child yp, y_suffix

type _ green_or_red = Green_or_red : [< `green | `red] green_or_red

let split
: type a b c k.
  c green_or_red -> (a, b, [`yellow], k) steque -> (b, c) kont -> a any_kont
= fun green_or_red steque kont ->
  match steque with
  | KONT -> Any_kont kont
  | Triple (p, c, s) ->
      begin match green_or_red, kont with
      | _, Suffix t -> Any_kont (Y (Triple (p, c, s), Suffix t))
      | _, G (t, k) -> Any_kont (Y (Triple (p, c, s), G (t, k)))
      | _, R (t, k) -> Any_kont (Yr (Triple (p, c, s), R (t, k)))
      | Green_or_red, _ -> .
      end

let split_green
: type a b k. (a, b, [`yellow], k) steque -> (b, [`green]) kont -> a t
= fun steque kont ->
  match steque with
  | KONT -> T kont
  | Triple (p, c, s) ->
      begin match kont with
      | Suffix t -> T (Y (Triple (p, c, s), Suffix t))
      | G (t, k) -> T (Y (Triple (p, c, s), G (t, k)))
      end

let split_red
: type a b k. (a, b, [`yellow], k) steque -> (b, [`red]) kont -> a any_kont
= fun steque kont ->
  match steque with
  | KONT -> Any_kont kont
  | Triple (p, c, s) ->
      begin match kont with
      | R (t, k) -> Any_kont (Yr (Triple (p, c, s), R (t, k)))
      end

type _ partition =
  | Small : 'a suffix -> 'a partition
  | Parts : ('a, 'c) prefix * ('a pair, _) kont * 'a suffix -> 'a partition

let partition
: type a. a any_kont -> a partition
= fun (Any_kont kont) ->
  match kont with
  | Suffix suffix -> Small suffix
  | Y (Triple (p, KONT, s), k) -> Parts (p, k, s)
  | G (Triple (p, KONT, s), k) -> Parts (p, k, s)
  | R (Triple (p, KONT, s), k) -> Parts (p, k, s)
  | Yr (Triple (p, KONT, s), k) -> Parts (p, k, s)
  | R (Triple (p, Triple (x, y, z), s), k) ->
      Parts (p, Y (Triple (x, y, z), k), s)
  | Y (Triple (p, Triple (x, y, z), s), k) ->
      Parts (p, Y (Triple (x, y, z), k), s)
  | Yr (Triple (p, Triple (x, y, z), s), k) ->
      Parts (p, Yr (Triple (x, y, z), k), s)
  | G (Triple (p, Triple (x, y, z), s), Suffix k) ->
      Parts (p, Y (Triple (x, y, z), Suffix k), s)
  | G (Triple (p, Triple (x, y, z), s), G (g, k)) ->
      Parts (p, Y (Triple (x, y, z), G (g, k)), s)
  | G (Triple (p, Triple (x, y, z), s), R (r, k)) ->
      Parts (p, Yr (Triple (x, y, z), R (r, k)), s)

let concat_kont
: type a c. (a, c) kont -> a any_kont -> a any_kont
= fun x ty ->
  match x with
  | Suffix small ->
      begin match Deque.uncons small with
      | None -> ty
      | Some (x, small) ->
          match Deque.uncons small with
          | None -> cons_any x ty
          | Some (y, small) ->
              match Deque.uncons small with
              | None -> cons_any x (cons_any y ty)
              | Some (z, small) ->
                  match Deque.uncons small with
                  | None -> cons_any x (cons_any y (cons_any z ty))
                  | Some (w, small) ->
                      let prefix = P4 (x, y, z, w, small) in
                      match partition ty with
                      | Small c ->
                          Any_kont (G (Triple (prefix, KONT, c), empty_kont))
                      | Parts (p, child, suffix) ->
                          let T child =
                            cons_any' (Pair (p, empty_kont)) (Any_kont child)
                          in
                          Any_kont (green prefix child suffix)
      end

  | G (Triple (prefix, child, s), kont) ->
      let Any_kont child = split Green_or_red child kont in
      let Any_kont rest, suffix = join_any child s ty in
      Any_kont (green prefix rest suffix)

  | Y (Triple (prefix, child, s), kont) ->
      let child = split_green child kont in
      let child, suffix = join_t child s ty in
      Any_kont (yellow prefix child suffix)

  | Yr (Triple (prefix, child, s), kont) ->
      let Any_kont child = split_red child kont in
      let child, suffix = join_any child s ty in
      orange prefix child suffix

  | R (Triple (prefix, child, s), kont) ->
      let child = split_green child kont in
      let child, suffix = join_t child s ty in
      Any_kont (red prefix child suffix)

let any_kont : type a. a t -> a any_kont
= fun (T t) -> Any_kont t

let concat
: type a. a t -> a t -> a t
= fun (T x) ty ->
  match x with
  | Suffix small ->
      begin match Deque.uncons small with
      | None -> ty
      | Some (x, small) ->
          match Deque.uncons small with
          | None -> cons x ty
          | Some (y, small) ->
              match Deque.uncons small with
              | None -> cons x (cons y ty)
              | Some (z, small) ->
                  match Deque.uncons small with
                  | None -> cons x (cons y (cons z ty))
                  | Some (w, small) ->
                      let prefix = P4 (x, y, z, w, small) in
                      match partition (any_kont ty) with
                      | Small c ->
                          T (G (Triple (prefix, KONT, c), empty_kont))
                      | Parts (p, child, suffix) ->
                          let T child =
                            cons_any' (Pair (p, empty_kont)) (Any_kont child)
                          in
                          T (green prefix child suffix)
      end

  | G (Triple (prefix, child, s), kont) ->
      let Any_kont child = split Green_or_red child kont in
      let Any_kont child, suffix = join_any child s (any_kont ty) in
      T (green prefix child suffix)

  | Y (Triple (prefix, child, s), kont) ->
      let child = split_green child kont in
      let child, suffix = join_t child s (any_kont ty) in
      T (yellow prefix child suffix)


let green_pop
: type a. (a, [`green]) prefix -> a * a yg_prefix
= function
  | P4 (a, b, c, d, deq) ->
      match Deque.uncons deq with
      | None -> a, Any (P3 (b, c, d))
      | Some (e, deq) -> a, Any (P4 (b, c, d, e, deq))

let yellow_pop
: type a. (a, [`yellow]) prefix -> a * (a, [`red]) prefix
= function P3 (a, b, c) -> a, P2 (b, c)

let green_of_red
: type a. (a, [`red]) kont -> (a, [`green]) kont
= function
  | R (Triple (P2 (a, b), KONT, s), Suffix small) ->
    begin match Deque.uncons small with
    | None ->
        Suffix (Deque.cons a (Deque.cons b s))

    | Some (Pair (p, s2), small) ->
        let p = match p with
          | P2 (c, d) -> P4 (a, b, c, d, Deque.empty)
          | P3 (c, d, e) -> P4 (a, b, c, d, Deque.cons e Deque.empty)
          | P4 (c, d, e, f, deq) ->
              P4 (a, b, c, d, Deque.cons e (Deque.cons f deq))
        in
        let Any_kont child = concat_kont s2 (Any_kont (Suffix small)) in
        green p child s
    end

  | R (Triple (P2 (a, b), KONT, s), G (Triple (p, child, s2), k)) ->
      let Pair (p, r), Any yellow_p = green_pop p in
      let p = match p with
        | P2 (c, d) -> P4 (a, b, c, d, Deque.empty)
        | P3 (c, d, e) -> P4 (a, b, c, d, Deque.cons e Deque.empty)
        | P4 (c, d, e, f, deq) ->
            P4 (a, b, c, d, Deque.cons e (Deque.cons f deq))
      in
      let child = match yellow_p, k with
        | P4 (a, b, c, d, deq), k ->
            Any_kont (G (Triple (P4 (a, b, c, d, deq), child, s2), k))
        | P3 (a, b, c), R (x, k) ->
            Any_kont (Yr (Triple (P3 (a, b, c), child, s2), R (x, k)))
        | P3 (a, b, c), G (x, k) ->
            Any_kont (Y (Triple (P3 (a, b, c), child, s2), G (x, k)))
        | P3 (a, b, c), Suffix k ->
            Any_kont (Y (Triple (P3 (a, b, c), child, s2), Suffix k))
      in
      let Any_kont child = concat_kont r child in
      green p child s

  | R (Triple (P2 (a, b), Triple (p, child, s2), s), k) ->
      let Pair (p, r), red_p = yellow_pop p in
      let p = match p with
        | P2 (c, d) -> P4 (a, b, c, d, Deque.empty)
        | P3 (c, d, e) -> P4 (a, b, c, d, Deque.cons e Deque.empty)
        | P4 (c, d, e, f, deq) ->
            P4 (a, b, c, d, Deque.cons e (Deque.cons f deq))
      in
      let child = Any_kont (R (Triple (red_p, child, s2), k)) in
      let Any_kont child = concat_kont r child in
      green p child s

type _ not_yellow = Not_yellow : [< `red | `green ] not_yellow

let make_green
: type a c. c not_yellow -> (a, c) kont -> (a, [`green]) kont
= fun not_yellow kont ->
  match not_yellow, kont with
  | _, Suffix s -> Suffix s
  | _, G (g, k) -> G (g, k)
  | _, R (r, k) -> green_of_red (R (r, k))
  | Not_yellow, _ -> .

let uncons
: type a. a t -> (a * a t) option
= fun (T t) ->
  match t with
  | Suffix s ->
      begin match Deque.uncons s with
      | None -> None
      | Some (x, s) -> Some (x, T (Suffix s))
      end
  | G (Triple (p, c, s), k) ->
      let x, Any p = green_pop p in
      begin match p with
      | (P4 _) as p -> Some (x, T (G (Triple (p, c, s), k)))
      | (P3 _) as p ->
          Some (x, T (Y (Triple (p, c, s), make_green Not_yellow k)))
      end
  | Y (Triple (p, c, s), k) ->
      let x, p = yellow_pop p in
      Some (x, T (green_of_red (R (Triple (p, c, s), k))))


