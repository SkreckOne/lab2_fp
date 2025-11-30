open OUnit2
open QCheck

module IntKey = struct
  type t = int

  let compare = compare
end

module Dict = Rb_dict_lazy.Make (IntKey)

let dict_from_list l = List.fold_left (fun acc (k, v) -> Dict.add k v acc) Dict.empty l
let dict_eq val_cmp d1 d2 = Dict.compare val_cmp d1 d2 = 0

let test_empty _ =
  assert_equal true (Dict.is_empty Dict.empty);
  let d = Dict.add 1 "a" Dict.empty in
  assert_equal false (Dict.is_empty d)

let test_add_find _ =
  let d = Dict.empty |> Dict.add 1 "one" |> Dict.add 2 "two" in

  assert_equal (Some "one") (Dict.find_opt 1 d);
  assert_equal (Some "two") (Dict.find_opt 2 d);

  assert_equal None (Dict.find_opt 3 d);

  let d2 = Dict.add 1 "one_updated" d in
  assert_equal (Some "one_updated") (Dict.find_opt 1 d2)

let test_remove _ =
  let d = dict_from_list [ (1, "a"); (2, "b"); (3, "c") ] in
  let d_removed = Dict.remove 2 d in

  assert_equal (Some "a") (Dict.find_opt 1 d_removed);
  assert_equal None (Dict.find_opt 2 d_removed);
  assert_equal (Some "c") (Dict.find_opt 3 d_removed)

let test_lazy_rebuild_trigger _ =
  let n = 100 in
  let rec build_up i acc = if i > n then acc else build_up (i + 1) (Dict.add i i acc) in
  let d_full = build_up 1 Dict.empty in

  let rec tear_down i acc = if i > 60 then acc else tear_down (i + 1) (Dict.remove i acc) in
  let d_cleaned = tear_down 1 d_full in

  assert_equal None (Dict.find_opt 10 d_cleaned);
  assert_equal (Some 70) (Dict.find_opt 70 d_cleaned);
  ()

let test_fold _ =
  let d = dict_from_list [ (1, 10); (2, 20); (3, 30) ] in
  let sum = Dict.fold (fun _k v acc -> acc + v) d 0 in
  assert_equal 60 sum

let test_filter _ =
  let d = dict_from_list [ (1, 1); (2, 2); (3, 3); (4, 4) ] in
  let d_even = Dict.filter (fun _k v -> v mod 2 = 0) d in

  assert_equal None (Dict.find_opt 1 d_even);
  assert_equal (Some 2) (Dict.find_opt 2 d_even);
  assert_equal None (Dict.find_opt 3 d_even);
  assert_equal (Some 4) (Dict.find_opt 4 d_even)

let test_map _ =
  let d = dict_from_list [ (1, 10); (2, 20) ] in
  let d_str = Dict.map (fun v -> string_of_int (v + 1)) d in
  assert_equal (Some "11") (Dict.find_opt 1 d_str);
  assert_equal (Some "21") (Dict.find_opt 2 d_str)

let unit_tests =
  "Unit Tests"
  >::: [
         "empty" >:: test_empty;
         "add_find" >:: test_add_find;
         "remove" >:: test_remove;
         "rebuild_trigger" >:: test_lazy_rebuild_trigger;
         "fold" >:: test_fold;
         "filter" >:: test_filter;
         "map" >:: test_map;
       ]

let dict_gen = QCheck.Gen.(list (pair small_int small_int) >>= fun l -> return (dict_from_list l))
let dict_arbitrary = QCheck.make dict_gen ~print:(fun _ -> "<dict>")

let prop_insert_find =
  Test.make ~name:"Property: Insert & Find" ~count:1000 (triple dict_arbitrary small_int small_int) (fun (d, k, v) ->
      let d_new = Dict.add k v d in
      match Dict.find_opt k d_new with Some found -> found = v | None -> false)

let prop_monoid_identity =
  Test.make ~name:"Property: Monoid Identity" ~count:500 dict_arbitrary (fun d ->
      let u_left = Dict.union Dict.empty d in
      let u_right = Dict.union d Dict.empty in
      dict_eq Int.compare d u_left && dict_eq Int.compare d u_right)

let prop_monoid_assoc =
  Test.make ~name:"Property: Monoid Associativity" ~count:500 (triple dict_arbitrary dict_arbitrary dict_arbitrary) (fun (a, b, c) ->
      let left = Dict.union (Dict.union a b) c in
      let right = Dict.union a (Dict.union b c) in

      dict_eq Int.compare left right)

let () =
  let open OUnit2 in
  run_test_tt_main
    ("All Tests"
    >::: [
           unit_tests;
           QCheck_ounit.to_ounit2_test prop_insert_find;
           QCheck_ounit.to_ounit2_test prop_monoid_identity;
           QCheck_ounit.to_ounit2_test prop_monoid_assoc;
         ])
