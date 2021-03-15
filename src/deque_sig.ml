module type S = sig
  (**/**)
  type 'a t
  (**/**)

  (** {1 List} *)

  (** Since a double-ended queue is most similar to a list, the following
      functions are provided to match the signature and behaviour of the
      standard {! List} module.
  *)

  val hd : 'a t -> 'a
  (** [hd xs] returns the left-most element of [xs].
      @raise Failure if the deque is empty.
  *)

  val tl : 'a t -> 'a t
  (** [tl xs] removes the left-most element of [xs].
      @raise Failure if the deque is empty.
  *)

  val nth : 'a t -> int -> 'a
  (** [nth xs n] returns the [n]-th element of the deque [xs]. The left-most
      element is at position 0.
      @raise Failure if the deque is too short.
      @raise Invalid_argument if [n] is negative.
  *)

  val nth_opt : 'a t -> int -> 'a option
  (** [nth xs n] returns the [n]-th element of the deque [xs]. The left-most
      element is at position 0.
      @return None if the deque is too short.
      @raise Invalid_argument if [n] is negative.
  *)

  val make : int -> 'a -> 'a t
  (** [make len x] replicates the value [x] in a new deque of size [len].
      @raise Invalid_argument if [len] is negative.
  *)

  val init : int -> (int -> 'a) -> 'a t
  (** [init len f] creates a new deque of size [len] such that its [i]th
      element is [f i], evaluated left to right.
      @raise Invalid_argument if [len] is negative.
  *)

  (** {1 Comparisons} *)

  val ( = ) : 'a t -> 'a t -> bool
  (** [xs = ys] is satisfied when the elements of [xs] are in the same order,
      and are structurally equal to the elements of [ys]. *)

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** [equal eq xs ys] is [true] if the two deques have the same length
      and satisfy [eq x_i y_i] for each pair of elements of [combine xs ys]. *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** [compare cmp xs ys] compares the deque [xs] in lexical order with [ys]
      according to the comparison function [cmp]. *)

  (** {1 Catenation} *)

  val ( @ ) : 'a t -> 'a t -> 'a t
  (** An alias for [append]: [xs @ ys] concatenates the two deques together. *)

  val rev_append : 'a t -> 'a t -> 'a t
  (** [rev_append xs ys] computes [append (rev xs) ys]. *)

  val concat : 'a t t -> 'a t
  (** Concatenate a deque of deques. The elements of the argument are all
      appended together, in the same order, to give the result.
  *)

  val flatten : 'a t t -> 'a t
  (** [flatten] is an alias for [concat]. *)

  (** {1 Iterators} *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f xs] applies the function [f] in turn to each element of [xs] from
      left to right. *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** Same as [iter], but the function [f] also receives the index of each
      element as first argument (counting from 0). *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f xs] creates a new deque where each element [x] of [xs] has been
      replaced by [f x]. *)

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  (** Same as [map], but the function [f] also receives the index of each
      element as its first argument (counting from 0). *)

  val rev_map : ('a -> 'b) -> 'a t -> 'b t
  (** [rev_map f xs] gives the same result as [rev (map f xs)]. *)

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  (** [filter_map f xs] applies [f] to each element of [xs], filtering out the
      [None] results and returns a deque of all the [Some] values. *)

  val concat_map : ('a -> 'b t) -> 'a t -> 'b t
  (** [concat_map f xs] gives the same result as [concat (map f xs)]. *)

  val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
  (** [fold_left_map f z xs] is a combination of [fold_left] and [map] that
      threads an accumulator [z] through calls to [f].
  *)

  val fold_left  : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold_left f z xs] computes [f (... (f (f z x_0) x_1) ...) x_n] where
      [x_0...x_n] are the elements of the deque [xs] in left to right order.
  *)

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_right f xs z] computes [f x_0 (f x_1 (... (f x_n z)))] where
      [x_0...x_n] are the elements of the deque [xs] in left to right order.
  *)

  (** {1 Iterators on two deques} *)

  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  (** [iter2 f xs ys] calls [f x_i y_i] for each element of [xs] and [ys] at
      the same index [i], in left to right order.
      @raise Invalid_argument if the two deques have different lengths.
  *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 f xs ys] zips the two deques.
      @raise Invalid_argument if they have different lengths.
  *)

  val rev_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Same as [rev (map2 f xs ys)]. *)

  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
  (** [fold_left2 f z xs ys] computes
      [f (... (f (f z x_0 y_0) x_1 y_1) ...) x_n y_n)].
  *)

  (** {1 Scanning} *)

  val for_all : ('a -> bool) -> 'a t -> bool
  (** [for_all f xs] checks if all elements of the deque [xs] satisfy the
      predicate [f]. It computes the conjunction [f x_0 && ... && f x_n], or
      returns [true] if the deque was empty.
  *)

  val exists : ('a -> bool) -> 'a t -> bool
  (** [exists f xs] checks if at least one element of the deque [xs]
      satisfies the predicate [f]. It computes the disjunction [f x_0 || ... ||
      f x_n], or returns [false] if the deque was empty.
  *)

  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** Same as [for_all], but for a two-arguments predicate.
      @raise Invalid_argument if the two deques have different lenths.
  *)

  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** Same as [exists], but for a two-arguments predicate.
      @raise Invalid_argument if the two deques have different lenths.
  *)

  val mem : 'a -> 'a t -> bool
  (** [mem x xs] is true if and only if [x] is structurally equal [(=)] to an
      element of [xs]. *)

  val memq : 'a -> 'a t -> bool
  (** [memq x xs] is true if and only if [x] is physically equal [(==)] to an
      element of [xs]. *)

  (** {1 Searching} *)

  val find : ('a -> bool) -> 'a t -> 'a
  (** [find f xs] returns the left-most element of [xs] that satisfies the
      predicate [f].
      @raise Not_found otherwise.
  *)

  val find_opt : ('a -> bool) -> 'a t -> 'a option
  (** [find f xs] returns the left-most element of [xs] that satisfies the
      predicate [f].
      @return None otherwise.
  *)

  val find_map : ('a -> 'b option) -> 'a t -> 'b option
  (** [find_map f xs] applies [f] to the elements of [xs] from left to right,
      and returns the first result of the form [Some v], or [None] if none
      exists.
  *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter f xs] returns all the elements of the deque [xs] that satisfy the
      predicate [f]. The order of the elements in the deque is preserved. *)

  val find_all : ('a -> bool) -> 'a t -> 'a t
  (** Same as [filter]. *)

  val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
  (** Same as [filter], but the predicate [f] also receives the index of each
      element as its first argument. *)

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [partition f xs] returns a pair of deques [(trues, falses)], such that
      [trues] contains all the elements of [xs] that satisfy [f] and [falses]
      the rest. The order of elements is preserved.
  *)

  (** {1 Association} *)

  val assoc : 'a -> ('a * 'b) t -> 'b
  (** [assoc key xs] returns the left-most value associated with [key] in the
      deque of key-value pairs [xs].
      @raise Not_found if there is no value with such a [key].
  *)

  val assoc_opt : 'a -> ('a * 'b) t -> 'b option
  (** [assoc key xs] returns the left-most value associated with [key] in the
      deque of key-value pairs [xs].
      @return None if there is no value with such a [key].
  *)

  val assq : 'a -> ('a * 'b) t -> 'b
  (** Same as [assoc], but uses physical equality rather than structural
      equality for key comparison.
      @raise Not_found if there is no value associated with [key].
  *)

  val assq_opt : 'a -> ('a * 'b) t -> 'b option
  (** Same as [assoc_opt], but uses physical equality rather than structural
      equality for key comparison.
      @return None if there is no value associated with [key].
  *)

  val mem_assoc : 'a -> ('a * 'b) t -> bool
  (** [mem_assoc key xs] returns [true] when [xs] contains a pair with [key],
      and [false] otherwise. *)

  val mem_assq : 'a -> ('a * 'b) t -> bool
  (** Same as [mem_assoc], but uses physical equality rather than structural
      equality. *)

  (** {1 Pairs} *)

  val split : ('a * 'b) t -> 'a t * 'b t
  (** [split xys] returns two deques [xs, ys] such that [xs] contains all the
      [fst] values of [xys], and [ys] all the [snd]. The order in each deque is
      preserved from the input. *)

  val combine : 'a t -> 'b t -> ('a * 'b) t
  (** [combine xs ys] returns a single deque [xys] formed by the aligned pairs
      [(x_i, y_i)] of [xs] and [ys]. The order is preserved.
      @raise Invalid_argument if the two deques have different lengths.
  *)

  (** {1 Sorting} *)

  val sort : ('a -> 'a -> int) -> 'a t -> 'a t
  (** [sort cmp xs] sorts the deque [xs] in increasing order, according to the
      comparison function [cmp].  This comparison function [cmp] must return
      [0] when its argument are equal, a positive integer if the first is
      greater and a negative integer if the first is smaller.
      See {! Array.sort} for a complete specification.
  *)

  val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  (** Same as [sort], but guarantees that elements that compare equal are kept
      in their original order. *)

  val fast_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  (** Same as [sort]. *)

  val sort_uniq : ('a -> 'a -> int) -> 'a t -> 'a t
  (** Same as [sort], but also removes duplicates. *)

  val merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  (** Assuming that [xs] and [ys] are two deques already sorted in increasing
    order by the comparison function [cmp], then [merge cmp xs ys] returns a
    sorted deque containing all the elements of [xs] and [ys].  *)

  (** {1 Conversions} *)

  (** All conversions between collections preserve the left to right ordering
      of elements: *)

  val to_array : 'a t -> 'a array
  (** [to_array xs] is an array containing all the elements of the deque [xs].
  *)

  val of_array : 'a array -> 'a t
  (** [of_array arr] creates a deque from the elements of the array [arr]. *)

  val to_list : 'a t -> 'a list
  (** [to_list xs] returns a list of the elements of the deque [xs]. *)

  val of_list : 'a list -> 'a t
  (** [of_list lst] creates a deque from the elements of the list [lst]. *)

  val to_seq : 'a t -> 'a Seq.t
  (** Iterate on a deque. *)

  val of_seq : 'a Seq.t -> 'a t
  (** Create a deque from a sequence. *)
end
