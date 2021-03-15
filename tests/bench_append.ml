let () =
  let steps = 1000 in
  for size = 0 to 500 do
    let lst = List.init size (fun i -> i) in
    let steq = Deque.Steque.of_list lst in
    let deck = Deque.Deck.of_list lst in
    let deckrev = Deque.Deckrev.of_list lst in
    Printf.printf "%i\t" size ;

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = lst @ lst in
      ()
    done ;
    let t1 = Unix.gettimeofday () in
    Printf.printf "%f\t" (t1 -. t0) ;

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = List.rev_append (List.rev lst) lst in
      ()
    done ;
    let t1 = Unix.gettimeofday () in
    Printf.printf "%f\t" (t1 -. t0) ;

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = Deque.Steque.(steq @ steq) in
      ()
    done ;
    let t1 = Unix.gettimeofday () in
    Printf.printf "%f\t" (t1 -. t0) ;

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = Deque.Deck.(deck @ deck) in
      ()
    done ;
    let t1 = Unix.gettimeofday () in
    Printf.printf "%f\t" (t1 -. t0) ;

    let t0 = Unix.gettimeofday () in
    for _ = 0 to steps do
      let _ = Deque.Deckrev.(deckrev @ deckrev) in
      ()
    done ;
    let t1 = Unix.gettimeofday () in
    Printf.printf "%f\n%!" (t1 -. t0)
  done
