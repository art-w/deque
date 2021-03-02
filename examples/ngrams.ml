module Ngram_make (N : sig val size : int end) = struct
  type t = char Deque.t

  let empty = Deque.init N.size (fun _ -> '^')

  let add t chr = Deque.tl (Deque.snoc t chr)

  let to_string t =
    let arr = Deque.to_array t in
    String.init (Array.length arr) (Array.get arr)

  let compare = Deque.compare Stdlib.compare
end

module Ngram = Ngram_make (struct let size = 12 end)

module Histogram = struct
  module M = Map.Make (Ngram)

  let empty = M.empty

  let add k t =
    let c = try 1 + M.find k t with Not_found -> 1 in
    M.add k c t

  let stats t =
    M.iter (fun k c -> Printf.printf "%2i : %S\n%!" c (Ngram.to_string k))
    @@ M.filter (fun _ c -> c > 1) t
end

let histogram str =
  let rec go h window i =
    if i >= String.length str
    then h
    else let window = Ngram.add window str.[i] in
         let h = Histogram.add window h in
         go h window (i + 1)
  in
  go Histogram.empty Ngram.empty 0

let lorem = {|
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus a mi
  ultricies, tincidunt ex egestas, vulputate nisl. Aliquam gravida purus
  sapien, eu aliquet magna pellentesque non. Sed maximus, lectus sed varius
  mollis, ante odio dictum nisl, non commodo nunc tortor ut dui. Sed eget diam
  ultrices, mollis metus non, molestie enim. Curabitur dui augue, molestie id
  mollis luctus, congue eu nunc. Nulla molestie blandit fringilla. Cras
  ultrices velit ut ex porttitor faucibus. Sed vestibulum mauris nec quam
  rutrum semper. Aenean ullamcorper, nisi a accumsan lobortis, dui ex sagittis
  quam, at condimentum nunc ex eu magna. Sed vel neque vel arcu vehicula
  bibendum. Donec massa massa, consectetur vel ultrices nec, fringilla vel
  purus. Pellentesque sem metus, volutpat in eros facilisis, volutpat laoreet
  eros. Integer facilisis, lorem a hendrerit pharetra, tellus nulla tincidunt
  orci, ut vehicula nisi nibh et elit.  Ut tempor gravida sapien, a luctus enim
  imperdiet ut.
|}

let () = Histogram.stats (histogram lorem)
