open! Import
open! Sorting_network

let%expect_test "power of 2 length" =
  require_does_raise (fun () -> create Bitonic_sort (fun _ _ -> assert false) [ 1; 2; 3 ]);
  [%expect
    {|
    ("Sorting networks require their input length to be a power of 2"
     (config Bitonic_sort)
     (length 3))
    |}]
;;

let%expect_test "sort integers" =
  let inputs = List.init 16 ~f:(fun _ -> Random.int 100) in
  let sorted =
    create
      Bitonic_sort
      (fun a b -> if a < b then { min = a; max = b } else { min = b; max = a })
      inputs
  in
  print_s [%message "" (inputs : int list) (sorted : int list)];
  [%expect
    {|
    ((inputs (53 42 0 84 36 78 84 61 93 0 35 35 66 97 84 39))
     (sorted (0 0 35 35 36 39 42 53 61 66 78 84 84 84 93 97)))
    |}]
;;

let sort_ascending_unsigned : Bits.t compare_and_swap =
  fun a b ->
  let open Bits in
  let sel = a <: b in
  { min = mux2 sel a b; max = mux2 sel b a }
;;

let%expect_test "bitonic, unsigned, ascending" =
  let inputs = List.init 4 ~f:(fun _ -> Bits.random ~width:4) in
  let sorted = create Bitonic_sort sort_ascending_unsigned inputs in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect
    {|
    ((inputs (1110 0101 1000 0011))
     (sorted (0011 0101 1000 1110)))
    |}]
;;

let sort_descending_signed : Bits.t compare_and_swap =
  fun a b ->
  let open Bits in
  let sel = a <+ b in
  { min = mux2 sel b a; max = mux2 sel a b }
;;

let%expect_test "bitonic, signed, descending" =
  let inputs = List.init 4 ~f:(fun _ -> Bits.random ~width:4) in
  let sorted = create Bitonic_sort sort_descending_signed inputs in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect
    {|
    ((inputs (1110 0101 1000 0011))
     (sorted (0101 0011 1110 1000)))
    |}]
;;

let%expect_test "odd_even_merge, unsigned, ascending" =
  let inputs = List.init 4 ~f:(fun _ -> Bits.random ~width:4) in
  let sorted = create Odd_even_merge_sort sort_ascending_unsigned inputs in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect
    {|
    ((inputs (1110 0101 1000 0011))
     (sorted (0011 0101 1000 1110)))
    |}]
;;

let%expect_test "odd_even_merge, signed, descending" =
  let inputs = List.init 4 ~f:(fun _ -> Bits.random ~width:4) in
  let sorted = create Odd_even_merge_sort sort_descending_signed inputs in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect
    {|
    ((inputs (1110 0101 1000 0011))
     (sorted (0101 0011 1110 1000)))
    |}]
;;

let%expect_test "sort by bottom 2 bits" =
  let inputs = List.init 8 ~f:(fun _ -> Bits.random ~width:4) in
  let sorted =
    create
      Bitonic_sort
      (fun a b ->
        let open Bits in
        let sel = a.:[1, 0] <: b.:[1, 0] in
        { min = mux2 sel a b; max = mux2 sel b a })
      inputs
  in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect
    {|
    ((inputs (1101 1100 1111 0011 1110 0101 1000 0011))
     (sorted (1000 1100 1101 0101 1110 0011 1111 0011)))
    |}]
;;

let%expect_test "check all possible zero-one inputs" =
  List.iter Config.all ~f:(fun config ->
    List.iter [ 1; 2; 4; 8; 16 ] ~f:(fun num_inputs ->
      for i = 0 to Int.pow 2 num_inputs - 1 do
        let inputs = Bits.bits_msb (Bits.of_int_trunc ~width:num_inputs i) in
        let sorted = create config sort_ascending_unsigned inputs in
        require
          (List.is_sorted sorted ~compare:(fun b1 b2 ->
             if Bits.equal (Bits.( <: ) b1 b2) Bits.vdd
             then -1
             else if Bits.equal b1 b2
             then 0
             else 1))
      done));
  [%expect {| |}]
;;
