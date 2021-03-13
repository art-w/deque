module Dequeue = Dequeue

module Steque = struct
  include List_like.Make (Steque)
  include Steque
end

module Deck = struct
  include List_like.Make (Deck)
  include Deck
end

module Deckrev = struct
  include List_like.Make (Deckrev)
  include Deckrev
end

include Deck
