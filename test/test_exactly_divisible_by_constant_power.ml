open! Core
open Hardcaml

include struct
  open Hardcaml_circuits.Exactly_divisible_by_constant_power
  module Bounds = Inclusive_integer_range
  module For_testing = For_testing
  module Div = Make (Bits)
end

let%expect_test "test mod inverse" =
  let test ~verbose ~modulo ~v =
    let modulo = Bigint.of_int modulo in
    let v = Bigint.of_int v in
    let inverse = For_testing.mod_inverse ~modulo v in
    if verbose
    then print_s [%message (modulo : Bigint.t) (v : Bigint.t) (inverse : Bigint.t)];
    [%test_result: Bigint.t] Bigint.(v * inverse % modulo) ~expect:Bigint.one
  in
  (* Some manual tests. *)
  for pow5 = 0 to 4 do
    test ~verbose:true ~modulo:(1 lsl 10) ~v:(Int.pow 5 pow5)
  done;
  [%expect
    {|
    ((modulo 1024) (v 1) (inverse 1))
    ((modulo 1024) (v 5) (inverse 205))
    ((modulo 1024) (v 25) (inverse 41))
    ((modulo 1024) (v 125) (inverse 213))
    ((modulo 1024) (v 625) (inverse 657))
    |}];
  Expect_test_helpers_base.require_does_raise ~here:[%here] (fun () ->
    test ~verbose:true ~modulo:(1 lsl 10) ~v:10);
  Expect_test_helpers_base.require_does_raise ~here:[%here] (fun () ->
    test ~verbose:true ~modulo:(1 lsl 10) ~v:2);
  Expect_test_helpers_base.require_does_raise ~here:[%here] (fun () ->
    test ~verbose:true ~modulo:(1 lsl 10) ~v:256);
  [%expect
    {|
    (Division_by_zero)
    (Division_by_zero)
    (Division_by_zero)
    |}];
  (* Quickcheck some more inputs that don't cause errors. *)
  let generate =
    let open Quickcheck.Generator.Let_syntax in
    let bits = 20 in
    let%bind tmp = Int.gen_incl 0 (1 lsl bits) in
    return (1 lsl bits, (2 * tmp) + 1)
  in
  Quickcheck.test ~trials:20 generate ~f:(fun (modulo, v) ->
    test ~verbose:false ~modulo ~v)
;;

let%expect_test "test inverse + bound lookup table" =
  let base = 5 in
  let pow_base_bounds = Bounds.create ~min:(-3) ~max:9 in
  let output_i_bits = 27 in
  let test ~check_for_error =
    let%tydi { inverse_bits; rom_values } =
      For_testing.inverse_and_bound_lookup
        ~check_for_error
        ~bounds:pow_base_bounds
        ~base
        ~output_bits:output_i_bits
    in
    print_s [%message (inverse_bits : int)];
    List.range ~stop:`inclusive pow_base_bounds.min pow_base_bounds.max
    |> List.iter2_exn rom_values ~f:(fun inverse pow_base ->
      print_s [%message (pow_base : int) (inverse : Bigint.t)])
  in
  test ~check_for_error:false;
  [%expect
    {|
    (inverse_bits 27)
    ((pow_base -3) (inverse 125))
    ((pow_base -2) (inverse 25))
    ((pow_base -1) (inverse 5))
    ((pow_base 0) (inverse 1))
    ((pow_base 1) (inverse 80530637))
    ((pow_base 2) (inverse 42949673))
    ((pow_base 3) (inverse 115964117))
    ((pow_base 4) (inverse 50036369))
    ((pow_base 5) (inverse 63694365))
    ((pow_base 6) (inverse 12738873))
    ((pow_base 7) (inverse 109921957))
    ((pow_base 8) (inverse 48827937))
    ((pow_base 9) (inverse 36609133))
    |}];
  test ~check_for_error:true;
  [%expect
    {|
    (inverse_bits 48)
    ((pow_base -3) (inverse 125))
    ((pow_base -2) (inverse 25))
    ((pow_base -1) (inverse 5))
    ((pow_base 0) (inverse 1))
    ((pow_base 1) (inverse 225179981368525))
    ((pow_base 2) (inverse 45035996273705))
    ((pow_base 3) (inverse 9007199254741))
    ((pow_base 4) (inverse 226981421219473))
    ((pow_base 5) (inverse 157986274928157))
    ((pow_base 6) (inverse 200482241012025))
    ((pow_base 7) (inverse 40096448202405))
    ((pow_base 8) (inverse 8019289640481))
    ((pow_base 9) (inverse 226783839296621))
    |}]
;;

let exhaustive ~(bounds : Bounds.t) ~base ~output_bits ~dividend_bits =
  for pow = bounds.min to bounds.max do
    Stdio.printf "------\n";
    for i = -(1 lsl (dividend_bits - 1)) to (1 lsl (dividend_bits - 1)) - 1 do
      let%tydi { valid; value } =
        Div.divide
          ~check_for_error:true
          ~bounds
          ~base
          ~output_bits
          ~dividend:(Bits.of_signed_int ~width:8 i)
          ~pow:(Bits.of_int_trunc ~width:(Div.pow_bits ~bounds) pow)
      in
      if pow < 0
      then (
        let base_pow = Int.pow base (-pow) in
        Stdio.printf
          "%4i * %4i = %4i [%4i] (valid=%i)\n"
          i
          base_pow
          (Bits.to_signed_int value)
          (i * base_pow)
          (Bits.to_unsigned_int valid))
      else (
        let base_pow = Int.pow base pow in
        Stdio.printf
          "%4i / %4i = %4i [%4i] (valid=%i)\n"
          i
          base_pow
          (Bits.to_signed_int value)
          (i / base_pow)
          (Bits.to_unsigned_int valid))
    done
  done
;;

let%expect_test "by 3^{-1,0,1}" =
  exhaustive
    ~bounds:(Bounds.create ~min:(-1) ~max:1)
    ~base:3
    ~output_bits:3
    ~dividend_bits:3;
  [%expect
    {|
    ------
      -4 *    3 =   -4 [ -12] (valid=0)
      -3 *    3 =   -1 [  -9] (valid=0)
      -2 *    3 =    2 [  -6] (valid=0)
      -1 *    3 =   -3 [  -3] (valid=1)
       0 *    3 =    0 [   0] (valid=1)
       1 *    3 =    3 [   3] (valid=1)
       2 *    3 =   -2 [   6] (valid=0)
       3 *    3 =    1 [   9] (valid=0)
    ------
      -4 /    1 =   -4 [  -4] (valid=1)
      -3 /    1 =   -3 [  -3] (valid=1)
      -2 /    1 =   -2 [  -2] (valid=1)
      -1 /    1 =   -1 [  -1] (valid=1)
       0 /    1 =    0 [   0] (valid=1)
       1 /    1 =    1 [   1] (valid=1)
       2 /    1 =    2 [   2] (valid=1)
       3 /    1 =    3 [   3] (valid=1)
    ------
      -4 /    3 =   -4 [  -1] (valid=0)
      -3 /    3 =   -1 [  -1] (valid=1)
      -2 /    3 =    2 [   0] (valid=0)
      -1 /    3 =   -3 [   0] (valid=0)
       0 /    3 =    0 [   0] (valid=1)
       1 /    3 =    3 [   0] (valid=0)
       2 /    3 =   -2 [   0] (valid=0)
       3 /    3 =    1 [   1] (valid=1)
    |}]
;;

let%expect_test "divide by 3" =
  exhaustive ~bounds:(Bounds.create ~min:1 ~max:1) ~base:3 ~output_bits:3 ~dividend_bits:3;
  [%expect
    {|
    ------
      -4 /    3 =   -4 [  -1] (valid=0)
      -3 /    3 =   -1 [  -1] (valid=1)
      -2 /    3 =    2 [   0] (valid=0)
      -1 /    3 =   -3 [   0] (valid=0)
       0 /    3 =    0 [   0] (valid=1)
       1 /    3 =    3 [   0] (valid=0)
       2 /    3 =   -2 [   0] (valid=0)
       3 /    3 =    1 [   1] (valid=1)
    |}]
;;

let%expect_test "divide by 5" =
  exhaustive ~bounds:(Bounds.create ~min:1 ~max:1) ~base:5 ~output_bits:6 ~dividend_bits:4;
  [%expect
    {|
    ------
      -8 /    5 =   24 [  -1] (valid=0)
      -7 /    5 =  -27 [  -1] (valid=0)
      -6 /    5 =  -14 [  -1] (valid=0)
      -5 /    5 =   -1 [  -1] (valid=1)
      -4 /    5 =   12 [   0] (valid=0)
      -3 /    5 =   25 [   0] (valid=0)
      -2 /    5 =  -26 [   0] (valid=0)
      -1 /    5 =  -13 [   0] (valid=0)
       0 /    5 =    0 [   0] (valid=1)
       1 /    5 =   13 [   0] (valid=0)
       2 /    5 =   26 [   0] (valid=0)
       3 /    5 =  -25 [   0] (valid=0)
       4 /    5 =  -12 [   0] (valid=0)
       5 /    5 =    1 [   1] (valid=1)
       6 /    5 =   14 [   1] (valid=0)
       7 /    5 =   27 [   1] (valid=0)
    |}]
;;

let random ~power ~num_trials =
  let within_signed_range bits x =
    let min = -(1 lsl (bits - 1)) in
    let max = (1 lsl (bits - 1)) - 1 in
    x >= min && x <= max
  in
  for _ = 1 to num_trials do
    let check_for_error = Random.bool () in
    let output_bits = 1 + Random.int 10 in
    let input_bits = 1 + Random.int 10 in
    let base = 3 + (Random.int 10 * 2) in
    let bounds = Bounds.create ~min:power ~max:power in
    let dividend = Random.int (1 lsl input_bits) - (1 lsl (input_bits - 1)) in
    let { With_valid.valid; value } =
      Div.divide
        ~check_for_error
        ~bounds
        ~base
        ~output_bits
        ~dividend:(Bits.of_signed_int ~width:input_bits dividend)
        ~pow:(Bits.of_int_trunc ~width:(Div.pow_bits ~bounds) power)
    in
    let got = Bits.to_signed_int value in
    let valid = Bits.to_bool valid in
    let error msg expected =
      print_s
        [%message
          msg
            (check_for_error : bool)
            (power : int)
            (output_bits : int)
            (input_bits : int)
            (base : int)
            (dividend : int)
            (expected : int)
            (got : int)
            (valid : bool)]
    in
    let check_multiplication () =
      (* multiplication - check result fits in [output_bits] *)
      let expected = dividend * Int.pow base (-power) in
      if not (within_signed_range output_bits expected)
      then
        if check_for_error && valid
        then error "Multiplication is out of range but was not detected" expected
        else ()
      else if not (equal expected got)
      then raise_s [%message "Multiplication result is incorrect"]
      else ()
    in
    let check_division () =
      let divisor = Int.pow base power in
      let expected = dividend / divisor in
      let exactly_divisible = dividend % divisor = 0 in
      if not check_for_error
      then
        if exactly_divisible
           && within_signed_range output_bits expected
           && not (equal expected got)
        then error "Division result is invalid" expected
        else ()
      else if not (within_signed_range output_bits expected)
      then
        if valid
        then error "Division is out of range but was not detected" expected
        else ()
      else if exactly_divisible
      then
        if not (equal expected got)
        then error "Division result is incorrect" expected
        else ()
      else if valid
      then error "Dividend not multiple of divisor not detected" expected
      else ()
    in
    if power <= 0 then check_multiplication () else check_division ()
  done
;;

let%expect_test "random" =
  let num_trials = 10_000 in
  random ~num_trials ~power:0;
  [%expect {| |}];
  random ~num_trials ~power:(-1);
  [%expect {| |}];
  random ~num_trials ~power:1;
  [%expect {| |}];
  random ~num_trials ~power:2;
  [%expect {| |}]
;;

let%expect_test "divide 64 bit by 1_000 downto 32 bits" =
  let%tydi { inverse_bits; _ } =
    let bounds = Bounds.create ~min:3 ~max:3 in
    For_testing.inverse_and_bound_lookup
      ~check_for_error:true
      ~bounds
      ~base:5
      ~output_bits:33
  in
  print_s [%message (inverse_bits : int)];
  [%expect {| (inverse_bits 40) |}];
  let div dividend =
    let open Bits in
    let shift = 3 in
    (* 65 bits - account for sign bit *)
    let dividend = of_signed_int ~width:65 dividend in
    let lo_dividend = sel_bottom dividend ~width:shift in
    let dividend = drop_bottom dividend ~width:shift in
    let%tydi { valid; value } =
      let bounds = Bounds.create ~min:3 ~max:3 in
      Div.divide
        ~check_for_error:true
        ~bounds
        ~base:5
        ~output_bits:33
        ~dividend
        ~pow:(of_int_trunc ~width:(Div.pow_bits ~bounds) 3)
    in
    (* must check the bits we drop for the power of 2 division are [0]. *)
    let valid = valid &: (lo_dividend ==:. 0) in
    let value =
      (* resize back down to 32 bits - basically drop the sign bit as we know the input
           was really unsigned. *)
      uresize value ~width:32
    in
    if to_bool valid then Some (to_unsigned_int value) else None
  in
  for i = 0 to 10_000 do
    let d0 = div (i * 1_000) in
    let d1 = div ((i * 1_000) + 1) in
    let d999 = div ((i * 1_000) + 999) in
    [%test_eq: int option] d0 (Some i);
    [%test_eq: int option] d1 None;
    [%test_eq: int option] d999 None
  done;
  [%expect {| |}];
  for i = 1 to 1_000 - 1 do
    [%test_eq: int option] (div ((10 * 1_000) + i)) None;
    [%test_eq: int option] (div ((88182 * 1_000) + i)) None;
    [%test_eq: int option] (div ((48_221_122 * 1_000) + i)) None
  done;
  let max_quotient = (1 lsl 32) - 1 in
  [%test_eq: int option] (div (max_quotient * 1_000)) (Some max_quotient);
  [%test_eq: int option] (div ((max_quotient + 1) * 1_000)) None
;;

let%expect_test "divide 64 bit by 1_000_000 downto 16 bits" =
  let%tydi { inverse_bits; _ } =
    let bounds = Bounds.create ~min:6 ~max:6 in
    For_testing.inverse_and_bound_lookup
      ~check_for_error:true
      ~bounds
      ~base:5
      ~output_bits:17
  in
  print_s [%message (inverse_bits : int)];
  [%expect {| (inverse_bits 31) |}];
  let div dividend =
    let open Bits in
    let shift = 6 in
    (* 65 bits - account for sign bit *)
    let dividend = of_signed_int ~width:65 dividend in
    let lo_dividend = sel_bottom dividend ~width:shift in
    let dividend = drop_bottom dividend ~width:shift in
    let%tydi { valid; value } =
      let bounds = Bounds.create ~min:6 ~max:6 in
      Div.divide
        ~check_for_error:true
        ~bounds
        ~base:5
        ~output_bits:17
        ~dividend
        ~pow:(of_int_trunc ~width:(Div.pow_bits ~bounds) 6)
    in
    (* must check the bits we drop for the power of 2 division are [0]. *)
    let valid = valid &: (lo_dividend ==:. 0) in
    let value = uresize value ~width:16 in
    if to_bool valid then Some (to_unsigned_int value) else None
  in
  for i = 0 to 10_000 do
    let d0 = div (i * 1_000_000) in
    let d1 = div ((i * 1_000_000) + 1) in
    let d999_999 = div ((i * 1_000_000) + 999_999) in
    [%test_eq: int option] d0 (Some i);
    [%test_eq: int option] d1 None;
    [%test_eq: int option] d999_999 None
  done;
  [%expect {| |}];
  for i = 1 to 1_000_000 - 1 do
    [%test_eq: int option] (div ((10 * 1_000_000) + i)) None;
    [%test_eq: int option] (div ((1267 * 1_000_000) + i)) None;
    [%test_eq: int option] (div ((47865 * 1_000_000) + i)) None
  done;
  let max_quotient = (1 lsl 16) - 1 in
  [%test_eq: int option] (div (max_quotient * 1_000_000)) (Some max_quotient);
  [%test_eq: int option] (div ((max_quotient + 1) * 1_000_000)) None
;;
