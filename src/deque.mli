
module Dequeue : sig
  include Deque_sig.S
  val unsnoc : 'a t -> ('a t * 'a) option
end

module Steque : sig
  include Deque_sig.S
  val append : 'a t -> 'a t -> 'a t
  val ( @ ) : 'a t -> 'a t -> 'a t
end

module Deck : sig
  include Deque_sig.S
  val unsnoc : 'a t -> ('a t * 'a) option
  val append : 'a t -> 'a t -> 'a t
  val ( @ ) : 'a t -> 'a t -> 'a t
end

include module type of Deck
