open! Base
open Hardcaml
open! Expect_test_helpers_base
module Div = Hardcaml_circuits.Divide_by_constant.Make (Bits)

let div ~dividend_bits ~dividend ~divisor =
  Bits.to_unsigned_int
    (Div.divide
       ~divisor:(Bigint.of_int divisor)
       (Bits.of_unsigned_int ~width:dividend_bits dividend))
;;

let test_div ~dividend_bits ~dividend ~divisor =
  let expected = dividend / divisor in
  let got = div ~dividend_bits ~dividend ~divisor in
  if expected <> got
  then raise_s [%message (dividend : int) (divisor : int) (expected : int) (got : int)]
;;

let%expect_test "random trials" =
  for _ = 0 to 100 do
    let divisor = 2 + Random.int 10 in
    let dividend_bits = Int.ceil_log2 (divisor + 1) + Random.int 20 in
    for _ = 0 to 1_000 do
      let dividend = Random.int (1 lsl dividend_bits) in
      test_div ~dividend_bits ~dividend ~divisor
    done
  done;
  [%expect {| |}]
;;

let%expect_test "Exhaustive over a few ranges" =
  let exhaustive dividend_bits divisor =
    for dividend = 0 to (1 lsl dividend_bits) - 1 do
      test_div ~dividend_bits ~dividend ~divisor
    done
  in
  exhaustive 8 3;
  exhaustive 5 20;
  exhaustive 10 33;
  exhaustive 9 97
;;

let%expect_test "At limits of divisor" =
  let test divisor =
    let divisor_bits = Bits.num_bits_to_represent divisor in
    let dividend_bits = divisor_bits + 6 in
    let max_dividend = (1 lsl dividend_bits) - 1 in
    let rec f i =
      (* This tests at the points at which the result changes (ie N*divisor plus or minus
         1). The number of trails is bounded by [dividend_bits=divisor_bits+6] meaning we
         do couple of hundered checks but can test very large divisors. *)
      let dividend = i * divisor in
      let dividend_plus_1 = dividend + 1 in
      let dividend_plus_divisor_minus_1 = dividend + divisor - 1 in
      let test_div ~dividend_bits ~dividend ~divisor =
        if dividend > max_dividend
        then Error ()
        else Ok (test_div ~dividend_bits ~dividend ~divisor)
      in
      match
        Result.all_unit
          [ test_div ~dividend_bits ~dividend ~divisor
          ; test_div ~dividend_bits ~dividend:dividend_plus_1 ~divisor
          ; test_div ~dividend_bits ~dividend:dividend_plus_divisor_minus_1 ~divisor
          ]
      with
      | Error () -> ()
      | Ok () -> f (i + 1)
    in
    f 0
  in
  List.iter
    [ 3
    ; 6
    ; 11
    ; 17
    ; 27
    ; 39
    ; 41
    ; 63
    ; 96
    ; 158
    ; 399
    ; 919
    ; 2_142
    ; 8_423
    ; 15_413
    ; 69_134
    ; 571_112
    ; 1_871_912
    ; 5_252_123
    ; 294_291_824
    ; 861_100_012
    ; 7_832_391_122
    ]
    ~f:test
;;

let%expect_test "divisor larger than dividend" =
  require_does_raise (fun () -> div ~dividend_bits:3 ~dividend:7 ~divisor:8);
  [%expect
    {| "Divisor is larger than largest possible dividend - result is always zero" |}];
  require_does_not_raise (fun () ->
    ignore (div ~dividend_bits:3 ~dividend:7 ~divisor:7 : int));
  [%expect {| |}];
  require_does_raise (fun () -> div ~dividend_bits:30 ~dividend:0 ~divisor:(1 lsl 30));
  [%expect
    {| "Divisor is larger than largest possible dividend - result is always zero" |}];
  require_does_not_raise (fun () ->
    ignore (div ~dividend_bits:30 ~dividend:0 ~divisor:((1 lsl 30) - 1) : int));
  [%expect {| |}]
;;
