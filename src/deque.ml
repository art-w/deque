module Dequeue = struct
  include List_like.Make (Dequeue)
  include Dequeue
end

module Steque = struct
  include List_like.Make (Steque)
  include Steque
end

module Deck = struct
  include List_like.Make (Deck)
  include Deck
end

include Deck
