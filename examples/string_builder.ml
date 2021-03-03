module type S = sig
  type t
  val empty : t
  val ( @ ) : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
end

module Build_string = struct
  type t = string
  let empty = ""
  let ( @ ) a b = a ^ b
  let of_string s = s
  let to_string s = s
end

module type FOLD = sig
  type 'a t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

module Fold_to_string (F : FOLD) = struct
  let to_string ss =
    let n = F.fold_left (fun acc s -> acc + String.length s) 0 ss in
    let buf = Bytes.create n in
    let _ =
      F.fold_left
        (fun i s ->
          let n = String.length s in
          Bytes.blit_string s 0 buf i n ;
          i + n)
        0
        ss
    in
    Bytes.to_string buf
end

module Build_list = struct
  type t = string list
  let empty = []
  let ( @ ) a b = List.append a b
  let of_string s = [s]
  include Fold_to_string (List)
end

module Build_list_safe = struct
  include Build_list
  let ( @ ) a b = List.rev_append (List.rev a) b
end

module Build_dlist = struct
  type t = string list -> string list
  let empty = fun xs -> xs
  let ( @ ) a b = fun xs -> a (b xs)
  let of_string s = fun xs -> s :: xs
  let to_string ss = Build_list.to_string (ss [])
end

module Build_tree = struct
  module F = struct
    type 'a t = Empty | Single of 'a | Concat of 'a t * 'a t
    let rec fold_left f z = function
      | Empty -> z
      | Single s -> f z s
      | Concat (a, b) -> fold_left f (fold_left f z a) b
  end
  type t = string F.t
  let empty = F.Empty
  let ( @ ) a b = match a, b with
    | F.Empty, t | t, F.Empty -> t
    | _ -> F.Concat (a, b)
  let of_string s = F.Single s
  include Fold_to_string (F)
end

module Build_deque = struct
  type t = string Deque.t
  let empty = Deque.empty
  let ( @ ) a b = Deque.append a b
  let of_string s = Deque.cons s Deque.empty
  include Fold_to_string (Deque)
end

module Build_steque = struct
  module Deque = Deque.Steque
  type t = string Deque.t
  let empty = Deque.empty
  let ( @ ) a b = Deque.append a b
  let of_string s = Deque.cons s Deque.empty
  include Fold_to_string (Deque)
end


let bench name f =
  let t0 = Unix.gettimeofday () in
  let ok, str = try true, f () with e -> false, Printexc.to_string e in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%16s: %.3f s -- %s\n%!"
    name
    (t1 -. t0)
    (if ok
    then "length " ^ string_of_int (String.length str)
    else str) ;
  if ok
  then Some str
  else None

module Test (Builder : S) = struct
  open Builder

  let popen = of_string " begin "
  let pclose = of_string " end "
  let parens x = popen @ x @ pclose

  let parens_in =
    bench "parens" @@ fun () ->
      let rec go acc n =
        if n = 0
        then acc
        else go (parens acc) (n - 1)
      in
      to_string (go empty 20000)

  let () = Gc.full_major ()

  let fibonacci =
    bench "fibonacci" @@ fun () ->
      let rec go a b n =
        if n = 0
        then a
        else go (parens (a @ b)) (parens a) (n - 1)
      in
      to_string (go (of_string "1") (of_string "0") 32)

  let () = Gc.full_major ()
end

let header name =
  Printf.printf "-- %s %s\n%!"
    name
    (String.make (70 - String.length name - 4) '-')

let () = header "String"
module Test_string = Test (Build_string)
let () = Printf.printf "\n%!"

module Test_check (Builder : S) = struct
  module T = Test (Builder)
  let () =
    assert (T.parens_in = Test_string.parens_in) ;
    assert (T.fibonacci = None || T.fibonacci = Test_string.fibonacci)
end

let () = header "List"
module Test_list = Test_check (Build_list)
let () = Printf.printf "\n%!"

let () = header "List_safe (no Stack_overflow on append)"
module Test_list_safe = Test_check (Build_list_safe)
let () = Printf.printf "\n%!"

let () = header "Diff list"
module Test_dlist = Test_check (Build_dlist)
let () = Printf.printf "\n%!"

let () = header "Custom tree"
module Test_tree = Test_check (Build_tree)
let () = Printf.printf "\n%!"

let () = header "Deque"
module Test_deque = Test_check (Build_deque)
let () = Printf.printf "\n%!"

let () = header "Steque"
module Test_steque = Test_check (Build_steque)
let () = Printf.printf "\n%!"

let () = header "Buffer"
module Test_buffer = struct

  let popen = " begin "
  let pclose = " end "

  let parens_in =
    bench "parens" @@ fun () ->
      let buf = Buffer.create 0 in
      let rec go n =
        if n = 0
        then ()
        else begin
          Buffer.add_string buf popen ;
          go (n - 1) ;
          Buffer.add_string buf pclose
        end
      in
      go 20000 ;
      Buffer.contents buf

  let () = Gc.full_major ()

  let fibonacci =
    bench "fibonacci" @@ fun () ->
      let buf = Buffer.create 0 in
      let rec go = function
        | 0 -> Buffer.add_string buf "0"
        | 1 -> Buffer.add_string buf "1"
        | n ->
            Buffer.add_string buf popen ;
            go (n - 1) ;
            if n > 2 then Buffer.add_string buf popen ;
            go (n - 2) ;
            if n > 2 then Buffer.add_string buf pclose ;
            Buffer.add_string buf pclose
      in
      go (32 + 1) ;
      Buffer.contents buf

  let () = Gc.full_major ()

  let () =
    assert (parens_in = Test_string.parens_in) ;
    assert (fibonacci = Test_string.fibonacci)
end

(* $ dune exec examples/string_builder.exe

-- String ------------------------------------------------------------
          parens: 1.478 s -- length 240000
       fibonacci: 0.430 s -- length 116432443

-- List --------------------------------------------------------------
          parens: 7.131 s -- length 240000
       fibonacci: 0.232 s -- Stack overflow

-- List_safe (no Stack_overflow on append) ---------------------------
          parens: 7.856 s -- length 240000
       fibonacci: 16.243 s -- length 116432443

-- Diff list ---------------------------------------------------------
          parens: 0.003 s -- length 240000
       fibonacci: 1.963 s -- length 116432443

-- Custom tree -------------------------------------------------------
          parens: 0.001 s -- length 240000
       fibonacci: 0.448 s -- length 116432443

-- Deque -------------------------------------------------------------
          parens: 0.013 s -- length 240000
       fibonacci: 0.581 s -- length 116432443

-- Steque ------------------------------------------------------------
          parens: 0.006 s -- length 240000
       fibonacci: 0.559 s -- length 116432443

-- Buffer ------------------------------------------------------------
          parens: 0.001 s -- length 240000
       fibonacci: 0.284 s -- length 116432443

*)
