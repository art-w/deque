module type S = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool

  val cons : 'a -> 'a t -> 'a t
  val uncons : 'a t -> ('a * 'a t) option
  val snoc : 'a t -> 'a -> 'a t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val length : 'a t -> int
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t

  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val rev_map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t

  val for_all : ('a -> bool) -> 'a t -> bool
  val exists : ('a -> bool) -> 'a t -> bool
  val mem : 'a -> 'a t -> bool
  val memq : 'a -> 'a t -> bool

  val find : ('a -> bool) -> 'a t -> 'a
  val find_opt : ('a -> bool) -> 'a t -> 'a option
  val find_map : ('a -> 'b option) -> 'a t -> 'b option
  val filter : ('a -> bool) -> 'a t -> 'a t
  val find_all : ('a -> bool) -> 'a t -> 'a t
  val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

  val assoc : 'a -> ('a * 'b) t -> 'b
  val assoc_opt : 'a -> ('a * 'b) t -> 'b option
  val assq : 'a -> ('a * 'b) t -> 'b
  val assq_opt : 'a -> ('a * 'b) t -> 'b option
  val mem_assoc : 'a -> ('a * 'b) t -> bool
  val mem_assq : 'a -> ('a * 'b) t -> bool

  val split : ('a * 'b) t -> 'a t * 'b t

  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
end
