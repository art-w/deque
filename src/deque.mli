(** A double-ended queue (abbreviated {e deque}) is an ordered collection for
    which elements can be added and removed from the front and the back of the
    list. This library provides a purely functional, fully persistent
    implementation, such that the main operations are all worst-case constant
    time.

    The following datastructures were invented by Kaplan and Tarjan and are
    described in their brilliant paper
    {{: http://www.cs.tau.ac.il/~haimk/papers/jacm-deq.ps }
    "Purely Functional, Real-Time Deques with Catenation"}.
*)

(** A double-ended queue with {b O(1)} [cons]/[uncons], [snoc]/[unsnoc]
    and [rev]; missing a fast [append]. Most similar to a finger tree as [nth]
    is also {b O(log min(i, N - i))}.  *)
module Dequeue : sig

  type 'a t
  (** The type of a deque containing elements of type ['a]. *)

  val empty : 'a t
  (** The [empty] deque. *)

  val is_empty : 'a t -> bool
  (** [is_empty xs] returns [true] when the deque [xs] contains no elements, [false] if at least one. *)

  val cons : 'a -> 'a t -> 'a t
  (** [cons x xs] adds an element [x] to the front of the deque [xs]. {b O(1)} *)

  val singleton : 'a -> 'a t
  (** [singleton x] returns a deque containing only a single element [x]. *)

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
  (** [append xs ys] concatenates the two deques [xs] and [ys]. {b O(N)} *)

  val rev : 'a t -> 'a t
  (** [rev xs] reverses the order of the elements of [xs]. {b O(1)} *)

  val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold_left f z xs] computes [f (... (f (f z x_0) x_1) ...) x_n] where
      [x_0...x_n] are the elements of the deque [xs] in left to right order.
  *)

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_right f xs z] computes [f x_0 (f x_1 (... (f x_n z)))] where
      [x_0...x_n] are the elements of the deque [xs] in left to right order.
  *)

  include Deque_sig.S with type 'a t := 'a t (** @inline *)
end

(** A stack-ended queue with {b O(1)} [cons]/[uncons], [snoc] and [append]; missing [unsnoc] and [rev]. *)
module Steque : sig
  include module type of Steque (** @inline *)

  include Deque_sig.S with type 'a t := 'a t (** @inline *)
end

(** A double-ended queue with all operations in {b O(1)}: [cons]/[uncons], [snoc]/[unsnoc] and [append]. *)
module Deck : sig
  include module type of Deck (** @inline *)

  include Deque_sig.S with type 'a t := 'a t (** @inline *)
end

(** Same as {! Deck}, but [rev] is also {b O(1)}. *)
module Deckrev : sig
  include module type of Deckrev (** @inline *)

  include Deque_sig.S with type 'a t := 'a t (** @inline *)
end

(** For convenience, the module {! Deck} is also included here as the default: *)

include module type of Deck (** @inline *)
