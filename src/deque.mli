
module Dequeue : sig
  include Deque_sig.S
  val unsnoc : 'a t -> ('a t * 'a) option
end

module Steque : Deque_sig.S

module Deck : sig
  include Deque_sig.S
  val unsnoc : 'a t -> ('a t * 'a) option
end

include module type of Deck
