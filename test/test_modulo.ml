open! Base
open! Hardcaml
open! Hardcaml_circuits
open! Expect_test_helpers_base

let test modulo =
  let result =
    List.init 16 ~f:(fun i ->
      Modulo.unsigned_by_constant (module Bits) (Bits.of_int_trunc ~width:4 i) modulo
      |> Bits.to_int_trunc)
  in
  print_s [%message (result : int list)];
  let utilization =
    Circuit.create_exn
      ~name:"modulo_pow2"
      [ Signal.output
          "q"
          (Modulo.unsigned_by_constant (module Signal) (Signal.input "d" 4) modulo)
      ]
    |> Circuit_utilization.create
  in
  print_s [%message (utilization : Circuit_utilization.t)]
;;

let%expect_test "modulo by pow 2" =
  test 4;
  [%expect
    {|
    (result (0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3))
    (utilization (
      (name modulo_pow2)
      (wires        ((count 2) (total_bits 6)))
      (part_selects ((count 1) (total_bits 2)))))
    |}]
;;

let%expect_test "modulo by non-pow 2" =
  test 3;
  [%expect
    {|
    (result (0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0))
    (utilization (
      (name modulo_pow2)
      (subtractors (
        (count             3)
        (total_bits        11)
        (max_instance_bits 4)))
      (not_gates (
        (count      3)
        (total_bits 3)))
      (comparators (
        (count             3)
        (total_bits        3)
        (max_instance_bits 1)))
      (multiplexers (
        (count      3)
        (total_bits 22)
        (multiplexers ((
          (number_of_data_elements 2)
          (max_instance_bits       8)
          (total_bits              22)
          (count                   3))))))
      (constants    ((count 6) (total_bits 22)))
      (wires        ((count 2) (total_bits 6)))
      (part_selects ((count 2) (total_bits 5)))))
    |}]
;;

let%expect_test "random tests" =
  let random num_tests =
    let width = 1 + Random.int 20 in
    let modu = max 1 (Random.int (1 lsl width)) in
    (* [num_tests] values with this modulo *)
    for _ = 1 to num_tests do
      let x = Random.int (1 lsl width) in
      let expected = x % modu in
      let got =
        Modulo.unsigned_by_constant (module Bits) (Bits.of_int_trunc ~width x) modu
        |> Bits.to_int_trunc
      in
      if expected <> got
      then print_s [%message (x : int) (modu : int) (expected : int) (got : int)]
    done
  in
  for _ = 1 to 100 do
    random 100
  done
;;

let%expect_test "mod 0 raises" =
  require_does_raise (fun () ->
    Modulo.unsigned_by_constant (module Bits) (Bits.of_int_trunc ~width:10 10) 0);
  [%expect {| "Cannot perform mod 0" |}]
;;
