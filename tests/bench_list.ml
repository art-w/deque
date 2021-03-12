
module type LIST = sig
  type 'a t
  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
  val uncons : 'a t -> ('a * 'a t) option
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

let bench name f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%20s : %.3f s\n%!" name (t1 -. t0) ;
  result

module Lst = struct
  type 'a t = 'a list
  let empty = []
  let cons x xs = x::xs
  let uncons = function
    | [] -> None
    | x::xs -> Some (x, xs)
  let fold_left = List.fold_left
end

module Test (L : LIST) = struct

  let make n =
    let rec go acc i =
      if i = 0
      then acc
      else go (L.cons i acc) (i - 1)
    in
    go L.empty n

  let sum_foldl xs = L.fold_left ( + ) 0 xs

  let rec sum_uncons acc xs =
    match L.uncons xs with
    | None -> acc
    | Some (x, xs) -> sum_uncons (acc + x) xs

  let sum_uncons xs = sum_uncons 0 xs

  let () =
    let xs = bench "make 10m" (fun () -> make 10_000_000) in
    let x = bench "sum_foldl" (fun () -> sum_foldl xs) in
    let y = bench "sum_uncons" (fun () -> sum_uncons xs) in
    assert (x = y) ;
    ()

end

let () = Printf.printf "-- List -------------------\n%!"
module A = Test (Lst)
let () = Printf.printf "\n%!"

let () = Printf.printf "-- Dequeue ----------------\n%!"
module B = Test (Deque.Dequeue)
let () = Printf.printf "\n%!"

let () = Printf.printf "-- Steque -----------------\n%!"
module C = Test (Deque.Steque)
let () = Printf.printf "\n%!"

let () = Printf.printf "-- Deck -------------------\n%!"
module D = Test (Deque.Deck)
