type 'a t
(** The type of a deque containing elements of type ['a]. *)

val empty : 'a t
(** The [empty] deque. *)

val is_empty : 'a t -> bool
(** [is_empty xs] returns [true] when the deque [xs] contains no elements, [false] if at least one. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] adds an element [x] to the front of the deque [xs]. {b O(1)} *)

val uncons : 'a t -> ('a * 'a t) option
(** [uncons xs] pops the left-most element of the deque [xs]. {b O(1)}
    @return [None] if the deque is empty.
*)

val snoc : 'a t -> 'a -> 'a t
(** [snoc xs x] adds an element [x] to the back of the deque [xs]. {b O(1)} *)

val unsnoc : 'a t -> ('a t * 'a) option
(** [unsnoc xs] pops the right-most element of the deque [xs]. {b O(1)}
    @return [None] if the deque is empty.
*)

val append : 'a t -> 'a t -> 'a t
(** [append xs ys] concatenates the two deques [xs] and [ys]. {b O(1)} *)

val rev : 'a t -> 'a t
(** [rev xs] reverses the order of the elements of [xs]. *)

val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_left f z xs] computes [f (... (f (f z x_0) x_1) ...) x_n] where
    [x_0...x_n] are the elements of the deque [xs] in left to right order.
*)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold_right f xs z] computes [f x_0 (f x_1 (... (f x_n z)))] where
    [x_0...x_n] are the elements of the deque [xs] in left to right order.
*)
