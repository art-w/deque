type 'a t
val of_deque : 'a Deque.t -> 'a t option
val to_deque : 'a t -> 'a Deque.t
val go_left  : 'a t -> 'a t option
val go_right : 'a t -> 'a t option
val focus : 'a t -> 'a
