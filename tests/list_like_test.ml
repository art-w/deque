let () = Random.self_init ()

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
    Printexc.print_backtrace stderr ;
    raise err

module Test (D : module type of Deque.Dequeue) = struct

  let input_size = 10

  let make ?(size = input_size) () =
    let lst = make_list size in
    let deq = D.of_list lst in
    lst, deq

  let assert_eq lst deq = assert (lst = D.to_list deq)
  let assert_not_found f =
    assert (try let _ = f () in false with Not_found -> true)
  let assert_invalid f =
    assert (try let _ = f () in false with Invalid_argument _ -> true)
  let assert_failure f =
    assert (try let _ = f () in false with Failure _ -> true)

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

  let () = test "rev" @@ fun () ->
    let lst as lst_orig, deq = make () in
    let lst = List.rev lst in
    let deq = D.rev deq in
    assert_eq lst deq ;
    assert (lst <> lst_orig)

  let () = test "append & rev_append" @@ fun () ->
    let lst0, deq0 = make ~size:input_size () in
    let lst1, deq1 = make ~size:(2 * input_size) () in
    let lst01 = List.append lst0 lst1 in
    let deq01 = D.append deq0 deq1 in
    assert_eq lst01 deq01 ;
    let lst10 = List.rev_append lst0 lst1 in
    let deq10 = D.rev_append deq0 deq1 in
    assert_eq lst10 deq10 ;
    assert (lst10 <> lst01)

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

  let () = test "nth & nth_opt" @@ fun () ->
    let lst, deq = make ~size:1234 () in
    for i = 0 to List.length lst - 1 do
      let x = List.nth lst i in
      let y = D.nth deq i in
      assert (x = y) ;
      match D.nth_opt deq i with
      | None -> assert false
      | Some y -> assert (x = y)
    done ;
    assert_invalid (fun () -> D.nth deq (-1)) ;
    assert_invalid (fun () -> D.nth_opt deq (-1)) ;
    assert_failure (fun () -> D.nth deq (D.length deq)) ;
    assert (None = D.nth_opt deq (D.length deq))

  type 'a acc = Z | X of 'a | F of 'a acc * 'a acc

  let () = test "fold_left" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let acc f z x = F (z, X (f x)) in
    let x = List.fold_left (acc f) Z lst in
    let y = D.fold_left (acc g) Z deq in
    assert (x = y) ;
    let x = List.fold_left (acc f) Z (List.rev lst) in
    let y = D.fold_left (acc g) Z (D.rev deq) in
    assert (x = y) ;
    check ()

  let () = test "fold_left2" @@ fun () ->
    let lst0, deq0 = make () in
    let lst1, deq1 = make () in
    let f, g, check = make_fs () in
    let acc f z x y = F (z, X (f (x, y))) in
    let x = List.fold_left2 (acc f) Z lst0 lst1 in
    let y = D.fold_left2 (acc g) Z deq0 deq1 in
    assert (x = y) ;
    check ()

  let () = test "fold_right" @@ fun () ->
    let lst, deq = make () in
    let f, g, check = make_fs () in
    let acc f x z = F (X (f x), z) in
    let x = List.fold_right (acc f) lst Z in
    let y = D.fold_right (acc g) deq Z in
    assert (x = y) ;
    let x = List.fold_right (acc f) (List.rev lst) Z in
    let y = D.fold_right (acc g) (D.rev deq) Z in
    assert (x = y) ;
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

  let () = test "iter2" @@ fun () ->
    let lst0, deq0 = make () in
    let lst1, deq1 = make () in
    let f, g, check = make_fs () in
    List.iter2 (fun x y -> ignore (f (x, y))) lst0 lst1 ;
    D.iter2 (fun x y -> ignore (g (x, y))) deq0 deq1 ;
    check ()

  let () = test "map2" @@ fun () ->
    let lst0, deq0 = make () in
    let lst1, deq1 = make () in
    let f, g, check = make_fs () in
    let lst = List.map2 (fun x y -> f (x, y)) lst0 lst1 in
    let deq = D.map2 (fun x y -> g (x, y)) deq0 deq1 in
    assert_eq lst deq ;
    check ()

  let () = test "rev_map2" @@ fun () ->
    let lst0, deq0 = make () in
    let lst1, deq1 = make () in
    let f, g, check = make_fs () in
    let lst = List.rev_map2 (fun x y -> f (x, y)) lst0 lst1 in
    let deq = D.rev_map2 (fun x y -> g (x, y)) deq0 deq1 in
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

  let () = test "exists2" @@ fun () ->
    let lst0, deq0 = make () in
    let lst1, deq1 = make () in
    let f, g, check = make_fs () in
    let is_even f x y = (f (x * y)) mod 2 = 0 in
    assert (true = List.exists2 (is_even f) lst0 lst1) ;
    assert (true = D.exists2 (is_even g) deq0 deq1) ;
    let nope f x y = ignore (f (x * y)) ; false in
    assert (false = List.exists2 (nope f) lst0 lst1) ;
    assert (false = D.exists2 (nope g) deq0 deq1) ;
    let fail _ _ = failwith "should not be called" in
    assert (false = List.exists2 fail [] []) ;
    assert (false = D.exists2 fail D.empty D.empty) ;
    assert (try List.exists2 fail [] lst1 with Invalid_argument _ -> true) ;
    assert (try D.exists2 fail D.empty deq1 with Invalid_argument _ -> true) ;
    check ()

  let () = test "for_all2" @@ fun () ->
    let lst0, deq0 = make () in
    let lst1, deq1 = make () in
    let f, g, check = make_fs () in
    let is_even f x y = (f (x * y)) mod 2 = 0 in
    assert (false = List.for_all2 (is_even f) lst0 lst1) ;
    assert (false = D.for_all2 (is_even g) deq0 deq1) ;
    let yeap f x y = ignore (f (x * y)) ; true in
    assert (true = List.for_all2 (yeap f) lst0 lst1) ;
    assert (true = D.for_all2 (yeap g) deq0 deq1) ;
    let fail _ _ = failwith "should not be called" in
    assert (true = List.for_all2 fail [] []) ;
    assert (true = D.for_all2 fail D.empty D.empty) ;
    assert (try List.for_all2 fail [] lst1 with Invalid_argument _ -> true) ;
    assert (try D.for_all2 fail D.empty deq1 with Invalid_argument _ -> true) ;
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

  let () = test "combine & split" @@ fun () ->
    let (keys, dkeys), (values, dvalues) = make (), make () in
    let lst = List.combine keys values in
    let deq = D.combine dkeys dvalues in
    assert_eq lst deq ;
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

  let () = test "make" @@ fun () ->
    for i = 0 to 100 do
      let deq = D.make i "x" in
      assert (D.length deq = i)
    done

  let () = test "init" @@ fun () ->
    let f, g, check = make_fs () in
    let lst = List.init input_size f in
    let deq = D.init input_size g in
    assert_eq lst deq ;
    check ()

  let random_list () = List.init 1000 (fun _ -> Random.int 100)
  let make_rnd () =
    let lst = random_list () in
    lst, D.of_list lst

  let rec is_sorted = function
    | [] | [_] -> true
    | x0 :: x1 :: xs -> x0 <= x1 && is_sorted (x1::xs)

  let () = test "sort & merge" @@ fun () ->
    let lst0, deq0 = make_rnd () in
    let lst0 = List.sort compare lst0 in
    let deq0 = D.sort compare deq0 in
    assert_eq lst0 deq0 ;
    let lst1, deq1 = make_rnd () in
    let lst1 = List.sort compare lst1 in
    let deq1 = D.sort compare deq1 in
    assert_eq lst1 deq1 ;
    let f, g, check = make_fs () in
    let count = ref 0 in
    let comparing f x y =
      incr count ;
      ignore (f (x, y)) ;
      compare x y in
    let lst = List.merge (comparing f) lst0 lst1 in
    let deq = D.merge (comparing g) deq0 deq1 in
    assert_eq lst deq ;
    assert (is_sorted lst) ;
    assert (!count <= 2 * D.length deq) ;
    check ()

  let () = test "merge not sorted" @@ fun () ->
    let lst0, deq0 = make_rnd () in
    let lst1, deq1 = make_rnd () in
    let f, g, check = make_fs () in
    let count = ref 0 in
    let comparing f x y =
      incr count ;
      ignore (f (x, y)) ;
      compare x y
    in
    let lst = List.merge (comparing f) lst0 lst1 in
    let deq = D.merge (comparing g) deq0 deq1 in
    assert_eq lst deq ;
    assert (not (is_sorted lst)) ;
    assert (!count <= 2 * D.length deq) ;
    check ()

end

let header name =
  Printf.printf "-- %s %s\n%!"
    name
    (String.make (70 - String.length name - 4) '-')

let () = header "Dequeue"
module Test_dequeue = Test (Deque.Dequeue)
let () = Printf.printf "\n%!"

let () = header "Steque"
module Test_steque = Test (struct
  include Deque.Steque
  let unsnoc t = match uncons (rev t) with
    | None -> None
    | Some (x, t) -> Some (rev t, x)
end)
let () = Printf.printf "\n%!"

let () = header "Deck"
module Test_deck = Test (Deque.Deck)
let () = Printf.printf "\n%!"

let () = header "Deckrev"
module Test_deckrev = Test (Deque.Deckrev)
let () = Printf.printf "\n%!"
