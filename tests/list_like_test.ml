
let make_f () =
  let calls = ref [] in
  let f x = calls := x :: !calls ; x in
  f, calls

let make_fs () =
  let f, fc = make_f () in
  let g, gc = make_f () in
  let check () =
    if !fc <> !gc then failwith "differing call order" ;
    if !fc = [] then failwith "functions were not called" ;
  in
  f, g, check


let counter = ref min_int
let elt () = incr counter ; !counter

let make_list n = List.init n (fun _ -> elt ())

let test name fn =
  try fn () ;
      Printf.printf "OK %s\n%!" name
  with err ->
    Printf.fprintf stderr "ERROR %s\n%s\n%!" name (Printexc.to_string err) ;
    Printexc.print_backtrace stderr

module Test (D : module type of Deque.Dequeue) = struct

  let input_size = 10

  let make () =
    let lst = make_list input_size in
    let deq = D.of_list lst in
    lst, deq

  let assert_eq lst deq = assert (lst = D.to_list deq)
  let assert_not_found f =
    assert (try let _ = f () in false with Not_found -> true)

  let () = test "iter" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    List.iter (fun x -> ignore (f x)) lst ;
    D.iter (fun x -> ignore (g x)) deq ;
    assert_eq lst deq ;
    check ()

  let () = test "iteri" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    List.iteri (fun i x -> ignore (f (i, x))) lst ;
    D.iteri (fun i x -> ignore (g (i, x))) deq ;
    check ()

  let () = test "map" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let lst = List.map f lst in
    let deq = D.map g deq in
    assert_eq lst deq ;
    check ()

  let () = test "mapi" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let lst = List.mapi (fun i x -> f (i, x)) lst in
    let deq = D.mapi (fun i x -> g (i, x)) deq in
    assert_eq lst deq ;
    check ()

  let () = test "rev_map" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let lst = List.rev_map f lst in
    let deq = D.rev_map g deq in
    assert_eq lst deq ;
    check ()

  let () = test "filter_map" @@ fun () ->
    let lst as lst_orig, deq = make () in
    let f, g, check = make_fs () in
    let is_even f x = if f x mod 2 = 0 then Some (x * x) else None in
    let lst = List.filter_map (is_even f) lst in
    let deq = D.filter_map (is_even g) deq in
    assert (List.length lst < List.length lst_orig) ;
    assert_eq lst deq ;
    check ()

  let () = test "fold_left_map" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let go f acc x = x::acc, f x in
    let lst_acc, lst = List.fold_left_map (go f) [] lst in
    let deq_acc, deq = D.fold_left_map (go g) [] deq in
    assert (lst_acc = deq_acc) ;
    assert_eq lst deq ;
    check ()

  let () = test "exists" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let is_even f x = (f x) mod 2 = 0 in
    assert (true = List.exists (is_even f) lst) ;
    assert (true = D.exists (is_even g) deq) ;
    let nope f x = ignore (f x) ; false in
    assert (false = List.exists (nope f) lst) ;
    assert (false = D.exists (nope g) deq) ;
    let fail _ = failwith "should not be called" in
    assert (false = List.exists fail []) ;
    assert (false = D.exists fail D.empty) ;
    check ()

  let () = test "for_all" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let is_even f x = (f x) mod 2 = 0 in
    assert (false = List.for_all (is_even f) lst) ;
    assert (false = D.for_all (is_even g) deq) ;
    let yeap f x = ignore (f x) ; true in
    assert (true = List.for_all (yeap f) lst) ;
    assert (true = D.for_all (yeap g) deq) ;
    let fail _ = failwith "should not be called" in
    assert (true = List.for_all fail []) ;
    assert (true = D.for_all fail D.empty) ;
    check ()

  let () = test "mem" @@ fun () ->
    let lst, deq = make () in
    match D.unsnoc deq with
    | None -> assert false
    | Some (deq', x) ->
        assert (true = List.mem x lst) ;
        assert (true = D.mem x deq) ;
        assert (false = D.mem x deq') ;
        ()

  let () = test "memq" @@ fun () ->
    let lst = List.map (fun x -> ref x) (make_list input_size) in
    let deq = D.of_list lst in
    match D.unsnoc deq with
    | None -> assert false
    | Some (deq', x) ->
        assert (true = List.memq x lst) ;
        assert (true = D.memq x deq) ;
        assert (false = D.memq x deq') ;
        let y = ref !x in
        assert (true = List.mem y lst) ;
        assert (true = D.mem y deq) ;
        assert (false = List.memq y lst) ;
        assert (false = D.memq y deq) ;
        ()

  let () = test "find" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let elt = List.nth lst 3 in
    let eq f x = f x = elt in
    let x0 = List.find (eq f) lst in
    let x1 = D.find (eq g) deq in
    assert (x0 = elt) ;
    assert (x0 = x1) ;
    let nope f x = ignore (f x) ; false in
    assert_not_found (fun () -> List.find (nope f) lst) ;
    assert_not_found (fun () -> D.find (nope g) deq) ;
    check ()

  let () = test "find_opt" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let elt = List.nth lst 3 in
    let eq f x = f x = elt in
    let x0 = List.find_opt (eq f) lst in
    let x1 = D.find_opt (eq g) deq in
    assert (x0 = Some elt) ;
    assert (x0 = x1) ;
    let nope f x = ignore (f x) ; false in
    assert (None = List.find_opt (nope f) lst) ;
    assert (None = D.find_opt (nope g) deq) ;
    check ()

  let () = test "find_map" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let elt = List.nth lst 3 in
    let eq f x = if f x = elt then Some (x * x) else None in
    let x0 = List.find_map (eq f) lst in
    let x1 = D.find_map (eq g) deq in
    assert (x0 = Some (elt * elt)) ;
    assert (x0 = x1) ;
    let nope f x = ignore (f x) ; None in
    assert (None = List.find_map (nope f) lst) ;
    assert (None = D.find_map (nope g) deq) ;
    check ()

  let () = test "filter" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let is_even f x = (f x) mod 2 = 0 in
    let lst = List.filter (is_even f) lst in
    let deq = D.filter (is_even g) deq in
    assert_eq lst deq ;
    assert (List.length lst > 0) ;
    let nope f x = ignore (f x) ; false in
    assert ([] = List.filter (nope f) lst) ;
    assert (D.is_empty @@ D.filter (nope g) deq) ;
    check ()

  let () = test "find_all" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let is_even f x = (f x) mod 2 = 0 in
    let lst = List.find_all (is_even f) lst in
    let deq = D.find_all (is_even g) deq in
    assert_eq lst deq ;
    assert (List.length lst > 0) ;
    let nope f x = ignore (f x) ; false in
    assert ([] = List.find_all (nope f) lst) ;
    assert (D.is_empty @@ D.find_all (nope g) deq) ;
    check ()

  let () = test "partition" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let is_even f x = (f x) mod 2 = 0 in
    let lst0, lst1 = List.partition (is_even f) lst in
    let deq0, deq1 = D.partition (is_even g) deq in
    assert_eq lst0 deq0 ;
    assert_eq lst1 deq1 ;
    assert (List.length lst0 > 0) ;
    assert (List.length lst1 > 0) ;
    let nope f x = ignore (f x) ; false in
    assert ([] = fst @@ List.partition (nope f) lst) ;
    assert (D.is_empty @@ fst @@ D.partition (nope g) deq) ;
    let yeap f x = ignore (f x) ; true in
    assert ([] = snd @@ List.partition (yeap f) lst) ;
    assert (D.is_empty @@ snd @@ D.partition (yeap g) deq) ;
    check ()

  let () = test "assoc & mem_assoc" @@ fun () ->
    let keys, values = make_list input_size, make_list input_size in
    let lst = List.combine keys values in
    let deq = D.of_list lst in
    match D.unsnoc deq with
    | None -> assert false
    | Some (deq', (k, v)) ->
        assert (v = List.assoc k lst) ;
        assert (v = D.assoc k deq) ;
        assert (List.mem_assoc k lst) ;
        assert (D.mem_assoc k deq) ;
        let lst' = D.to_list deq' in
        assert_not_found (fun () -> List.assoc k lst') ;
        assert_not_found (fun () -> D.assoc k deq') ;
        assert (not (List.mem_assoc k lst')) ;
        assert (not (D.mem_assoc k deq')) ;
        ()

  let () = test "assoc_opt" @@ fun () ->
    let keys, values = make_list input_size, make_list input_size in
    let lst = List.combine keys values in
    let deq = D.of_list lst in
    match D.unsnoc deq with
    | None -> assert false
    | Some (deq', (k, v)) ->
        assert (Some v = List.assoc_opt k lst) ;
        assert (Some v = D.assoc_opt k deq) ;
        assert (List.mem_assoc k lst) ;
        assert (D.mem_assoc k deq) ;
        let lst' = D.to_list deq' in
        assert (None = List.assoc_opt k lst') ;
        assert (None = D.assoc_opt k deq') ;
        assert (not (List.mem_assq k lst')) ;
        assert (not (D.mem_assq k deq')) ;
        ()

  let () = test "assq & mem_assq" @@ fun () ->
    let keys, values = make_list input_size, make_list input_size in
    let keys = List.map (fun x -> ref x) keys in
    let lst = List.combine keys values in
    let deq = D.of_list lst in
    match D.unsnoc deq with
    | None -> assert false
    | Some (deq', (k, v)) ->
        assert (v = List.assq k lst) ;
        assert (v = D.assq k deq) ;
        let k' = ref !k in
        assert_not_found (fun () -> List.assq k' lst) ;
        assert_not_found (fun () -> D.assq k' deq) ;
        let lst' = D.to_list deq' in
        assert_not_found (fun () -> List.assq k lst') ;
        assert_not_found (fun () -> D.assq k deq') ;
        ()

  let () = test "assq_opt" @@ fun () ->
    let keys, values = make_list input_size, make_list input_size in
    let keys = List.map (fun x -> ref x) keys in
    let lst = List.combine keys values in
    let deq = D.of_list lst in
    match D.unsnoc deq with
    | None -> assert false
    | Some (deq', (k, v)) ->
        assert (Some v = List.assq_opt k lst) ;
        assert (Some v = D.assq_opt k deq) ;
        let k' = ref !k in
        assert (None = List.assq_opt k' lst) ;
        assert (None = D.assq_opt k' deq) ;
        let lst' = D.to_list deq' in
        assert (None = List.assq_opt k lst') ;
        assert (None = D.assq_opt k deq') ;
        ()

  let () = test "split" @@ fun () ->
    let keys, values = make_list input_size, make_list input_size in
    let lst = List.combine keys values in
    let deq = D.of_list lst in
    let lst0, lst1 = List.split lst in
    let deq0, deq1 = D.split deq in
    assert (lst0 = keys) ;
    assert_eq lst0 deq0 ;
    assert (lst1 = values) ;
    assert_eq lst1 deq1

  let () = test "to_seq & of_seq" @@ fun () ->
    let lst, deq = make () in
    let lst_s = List.to_seq lst in
    let deq_s = D.to_seq deq in
    let lst' = List.of_seq lst_s in
    let deq' = D.of_seq deq_s in
    assert (lst = lst') ;
    assert_eq lst deq'

  let () = test "to_array & of_array" @@ fun () ->
    let lst = make_list input_size in
    let arr = Array.of_list lst in
    let deq = D.of_array arr in
    assert_eq lst deq ;
    let arr' = D.to_array deq in
    assert (arr = arr')

  let () = test "init" @@ fun () ->
    let f, g, check = make_fs () in
    let lst = List.init input_size f in
    let deq = D.init input_size g in
    assert_eq lst deq ;
    check ()

end

let header name =
  Printf.printf "-- %s %s\n%!"
    name
    (String.make (70 - String.length name - 4) '-')

let () = header "Dequeue"
module Test_dequeue = Test (Deque.Dequeue)
let () = Printf.printf "\n%!"

let () = header "Deck"
module Test_deck = Test (Deque.Deck)
