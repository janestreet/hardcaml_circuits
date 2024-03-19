open! Base
open! Hardcaml
open! Hardcaml_circuits
open! Expect_test_helpers_base

let%expect_test "msb first" =
  let open Bits in
  let test input =
    let { Onehot_clean.any_bit_set; data } =
      Onehot_clean.scan_from_msb (module Bits) input
    in
    print_s [%message (input : Bits.t) (data : Bits.t) (any_bit_set : Bits.t)]
  in
  test (of_string "0000");
  test (of_string "0001");
  test (of_string "0011");
  test (of_string "0111");
  test (of_string "1111");
  test (of_string "0101");
  test (of_string "0011");
  [%expect
    {|
    ((input       0000)
     (data        0000)
     (any_bit_set 0))
    ((input       0001)
     (data        0001)
     (any_bit_set 1))
    ((input       0011)
     (data        0010)
     (any_bit_set 1))
    ((input       0111)
     (data        0100)
     (any_bit_set 1))
    ((input       1111)
     (data        1000)
     (any_bit_set 1))
    ((input       0101)
     (data        0100)
     (any_bit_set 1))
    ((input       0011)
     (data        0010)
     (any_bit_set 1))
    |}]
;;

let%expect_test "lsb first" =
  let open Bits in
  let test input =
    let { Onehot_clean.any_bit_set; data } =
      Onehot_clean.scan_from_lsb (module Bits) input
    in
    print_s [%message (input : Bits.t) (data : Bits.t) (any_bit_set : Bits.t)]
  in
  test (of_string "0000");
  test (of_string "0001");
  test (of_string "0010");
  test (of_string "0100");
  test (of_string "1000");
  test (of_string "1110");
  test (of_string "1100");
  test (of_string "1010");
  [%expect
    {|
    ((input       0000)
     (data        0000)
     (any_bit_set 0))
    ((input       0001)
     (data        0001)
     (any_bit_set 1))
    ((input       0010)
     (data        0010)
     (any_bit_set 1))
    ((input       0100)
     (data        0100)
     (any_bit_set 1))
    ((input       1000)
     (data        1000)
     (any_bit_set 1))
    ((input       1110)
     (data        0010)
     (any_bit_set 1))
    ((input       1100)
     (data        0100)
     (any_bit_set 1))
    ((input       1010)
     (data        0010)
     (any_bit_set 1))
    |}]
;;
