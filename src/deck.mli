type 'a t

val empty : 'a t
val is_empty : 'a t -> bool

val cons : 'a -> 'a t -> 'a t
val uncons : 'a t -> ('a * 'a t) option

val snoc : 'a t -> 'a -> 'a t
val unsnoc : 'a t -> ('a t * 'a) option

val append : 'a t -> 'a t -> 'a t

val rev : 'a t -> 'a t

val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
