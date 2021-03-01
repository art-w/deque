(*
   "Bridging the algorithm gap: A linear-time functional
    program for paragraph formatting"
   by Oege de Moor and Jeremy Gibbons
   Science of Computer Programming Volume 35, September 1999
   https://doi.org/10.1016/S0167-6423(99)00005-2
*)

let max_width = ref 80

let square x = x * x

let dynamic words = (* O(N * max_width) *)
  let words = Array.of_list words in
  let cache = Array.make_matrix (Array.length words) !max_width (-1) in
  let cost_of line = square (!max_width - line) in
  let rec go word_index current_line_length =
    if word_index >= Array.length words
    then 0
    else if current_line_length >= !max_width
    then go word_index 0
    else if cache.(word_index).(current_line_length) >= 0
    then cache.(word_index).(current_line_length)
    else begin
      let r = compute word_index current_line_length in
      cache.(word_index).(current_line_length) <- r ;
      r
    end
    and compute word_index current_line_length =
      let w = words.(word_index) in
      let new_length =
        if current_line_length = 0
        then String.length w
        else current_line_length + 1 + String.length w
      in
      let break () = cost_of current_line_length + go word_index 0 in
      if current_line_length = 0
      then go (word_index + 1) new_length
      else if new_length > !max_width
      then break ()
      else min (go (word_index + 1) new_length)
               (break ())
  in
  go 0 0


type fmt =
  { breaks : int list
  ; cost : int
  }

let empty_layout = { breaks = [] ; cost = 0 }

let current_length position fmt =
  match fmt.breaks with
  | [] -> position
  | latest :: _ -> position - latest - 1

let remaining position p = !max_width - current_length position p

let add_word w position = position + 1 + String.length w

let futur_cost position p =
  p.cost + square (remaining position p)

let add_break position fmt =
  { breaks = position :: fmt.breaks
  ; cost = fmt.cost + square (!max_width - current_length position fmt)
  }


module Deque = Deque.Dequeue

let rec drop_while p xs = match Deque.uncons xs with
  | Some (x, xs) when p x -> drop_while p xs
  | _ -> xs

let nbr_chars position p q =
  (* returns the smallest [i] such that:

      futur_cost (position + i) p  >=  futur_cost (position + i) q
  ===
      p.cost + square (remaining position p - i)
      >= q.cost + square (remaining position q - i)
  ===
      p.cost + rem position p ^ 2 - 2 * i * rem position p + i ^ 2
      >= q.cost + rem position q ^ 2 - 2 * i * rem position q + i ^ 2
  ===
      2 * i * rem position q - 2 * i * rem position p
      >= q.cost + rem position q ^ 2 - p.cost - rem position p ^ 2
  ===
      2 * i * (rem position q - rem position p)
      >= q.cost + rem position q ^ 2 - p.cost - rem position p ^ 2
  ===
      2 * i * (rem position q - rem position p)
      >= futur_cost position q - futur_cost position p
  ===
      i >= (q.cost + rem position q ^ 2 - p.cost - rem position p)
           / 2*(rem position q - rem position p)
  ===
      i >= (futur_cost position q - futur_cost position p)
           / 2*(rem position q - rem position p)
  *)
  let ( // ) a b = (* round up division *)
    a / b + (if a mod b > 0 then 1 else 0) in
  let i =
    (futur_cost position q - futur_cost position p)
    // (2 * (remaining position q - remaining position p))
  in
  min i (remaining position q)

let rec trim_tail new_position xs p = match Deque.unsnoc xs with
  | Some (rest, q) -> trim_tail' new_position rest q p
  | _ -> Deque.snoc xs p

and trim_tail' new_position rest q p =
  if futur_cost new_position p < futur_cost new_position q
  then trim_tail new_position rest p
  else match Deque.unsnoc rest with
    | Some (rest, r)
      when nbr_chars new_position p q <= nbr_chars new_position q r ->
        trim_tail' new_position rest r p
    | _ ->
        Deque.snoc (Deque.snoc rest q) p

let rec trim_head position xs =
  match Deque.uncons xs with
  | Some (p, rest) -> trim_head' position p rest xs
  | _ -> xs

and trim_head' position p xs p_xs =
  match Deque.uncons xs with
  | Some (q, rest) when futur_cost position p >= futur_cost position q ->
      trim_head' position q rest xs
  | _ -> p_xs

let overflow position c = current_length position c > !max_width

let step position candidates word =
  let new_position = add_word word position in
  let new_candidates =
    trim_head new_position
    @@ drop_while (overflow new_position) candidates
  in
  let new_candidates =
    if position <= 0
    then new_candidates
    else match Deque.uncons candidates with
      | None -> new_candidates
      | Some (c, _) ->
          let c = add_break position c in
          trim_tail new_position new_candidates c
  in
  new_position, new_candidates

let all_layouts words =
  let _, res =
    List.fold_left
      (fun (position, candidates) word ->
        step position candidates word)
      (-1, Deque.cons empty_layout Deque.empty)
      words
  in
  res

let get_best candidates =
  match Deque.uncons candidates with
  | None -> assert false
  | Some (x, xs) ->
      Deque.fold_left
        (fun best c ->
          if best.cost < c.cost
          then best
          else c)
        x
        xs

let knuth_plass ws = get_best (all_layouts ws)

let fmt words fmt =
  Printf.printf "COST: %i\n%!" fmt.cost ;
  Printf.printf "%s\n%!" (String.make !max_width 'v') ;
  let rec go current_width position breaks words =
    match breaks, words with
    | _, [] -> []
    | b :: bs, w :: ws when b = position ->
        let filler = String.make (max 0 (!max_width - current_width)) '~' in
        let new_position = add_word w position in
        filler :: "\n" :: w :: go (add_word w (-1)) new_position bs ws
    | _, w :: ws ->
        let new_position = add_word w position in
        let ws = go (add_word w current_width) new_position breaks ws in
        " " :: w :: ws
  in
  let res =
    match go (-1) (-1) (List.rev fmt.breaks) words with
    | " " :: rest -> rest
    | ok -> ok
  in
  List.iter (Printf.printf "%s") res ;
  Printf.printf "\n%!"

let linear words =
  let step (position, layout) w =
    let new_position = position + 1 + String.length w in
    if current_length new_position layout > !max_width
    then new_position, add_break position layout
    else new_position, layout
  in
  let _, layout = List.fold_left step (-1, empty_layout) words in
  layout

let whitespace = Str.regexp "[ \t\n]+"
let words str = Str.split whitespace str

let test str =
  let ws = words str in
  for i = 20 to 70 do
    max_width := i ;
    let opt = dynamic ws in
    Printf.printf "QUADRATIC DYNAMIC: %i\n%!" opt ;
    Printf.printf " KNUTH PLASS " ;
    let sol = knuth_plass ws in
    fmt ws sol ;
    assert (sol.cost = opt) ;
    Printf.printf "\n%!" ;
    Printf.printf "LINEAR " ;
    let sol_linear = linear ws in
    fmt ws sol_linear ;
    assert (sol_linear.cost >= opt) ;
    Printf.printf "\n%!" ;
  done ;
  ()

let lorem =
  {| Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas
     fermentum, ligula in interdum iaculis, orci metus ullamcorper nisl,
     in dapibus nulla turpis in orci. Cras pharetra neque vitae metus
     fringilla, vitae sodales neque tristique. Suspendisse sed diam a arcu
     rhoncus pretium nec a enim. Proin eget pulvinar neque. Nulla a sodales
     felis, ac mattis nisi. Nulla ac massa fermentum, iaculis lorem a,
     mattis est. Sed dignissim lectus sed consectetur volutpat. Vivamus
     condimentum convallis ligula a tin cidunt. Sed in odio neque. Mauris
     congue nisl consequat urna dictum hendrerit. Maecenas eu sagittis
     lectus. Nunc rutrum varius sollicitudin. Curabitur lobortis gravida
     turpis, vitae sollicitudin mi commodo et. Praesent sit amet imperdiet
     turpis. Nullam facilisis ligula quis posuere sodales. a a a a a a a a
     a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
     a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
     a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
     a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
     a a a a a a a a a a a a a a a
     bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  |}

let () = test lorem
