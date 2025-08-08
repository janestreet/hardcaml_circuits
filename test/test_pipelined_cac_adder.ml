open Import
open Pipelined_adder

(* show the truth table for the Carry Ahead Cell used in the adder *)
let%expect_test "cac truthtable" =
  let open Bits in
  let cac c =
    let a = gnd @: gnd @: c.:(0) in
    let b = gnd @: vdd @: c.:(1) in
    a +: b +: uresize c.:(2) ~width:3
  in
  let cacs =
    List.init 8 ~f:(fun i ->
      let x = of_int_trunc ~width:3 i in
      i, (x, cac x))
  in
  print_s [%message (cacs : (int * (Bits.t * Bits.t)) list)];
  [%expect
    {|
    (cacs (
      (0 (000 010))
      (1 (001 011))
      (2 (010 011))
      (3 (011 100))
      (4 (100 011))
      (5 (101 100))
      (6 (110 100))
      (7 (111 101))))
    |}]
;;

let exhaustive_test_bits ~width ~part_width =
  let max = (1 lsl width) - 1 in
  for a = 0 to max do
    for b = 0 to max do
      let sum =
        Short_latency.comb
          (module Bits)
          ~part_width
          (Bits.of_int_trunc ~width a)
          (Bits.of_int_trunc ~width b)
      in
      if Bits.to_int_trunc sum <> (a + b) land max
      then print_s [%message "mismatch" (a : int) "+" (b : int) "<>" (sum : Bits.t)]
    done
  done
;;

let%expect_test "4 bit adder, all part sizes" =
  exhaustive_test_bits ~width:4 ~part_width:1;
  [%expect {| |}];
  exhaustive_test_bits ~width:4 ~part_width:2;
  [%expect {| |}];
  exhaustive_test_bits ~width:4 ~part_width:3;
  [%expect {| |}];
  exhaustive_test_bits ~width:4 ~part_width:4;
  [%expect {| |}]
;;

let%expect_test "various sizes" =
  exhaustive_test_bits ~width:5 ~part_width:2;
  [%expect {| |}];
  exhaustive_test_bits ~width:8 ~part_width:2;
  [%expect {| |}];
  exhaustive_test_bits ~width:8 ~part_width:4;
  [%expect {| |}]
;;

let print_is_proved eqn =
  let open Hardcaml_verify in
  ignore (Solver.solve ~print_model:true Comb_gates.(cnf ~:eqn) : _ Or_error.t)
;;

let%expect_test "prove 64 bit variants" =
  let module Bits = Hardcaml_verify.Comb_gates in
  let a = Bits.input "a" 64 in
  let b = Bits.input "b" 64 in
  let prove ~part_width =
    let sum = Short_latency.comb (module Bits) ~part_width a b in
    print_is_proved Bits.(sum ==: a +: b)
  in
  prove ~part_width:16;
  [%expect
    {|
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}];
  prove ~part_width:8;
  [%expect
    {|
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}];
  prove ~part_width:13;
  [%expect
    {|
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}]
;;

let sim ~part_width =
  let open Signal in
  let clock = input "clock" 1 in
  let a = input "a" 4 in
  let b = input "b" 4 in
  let sum = Short_latency.create ~part_width ~clock a b in
  let circuit = Circuit.create_exn ~name:"pipelined_adder" [ output "sum" sum ] in
  Cyclesim.create ~config:{ Cyclesim.Config.default with store_circuit = true } circuit
;;

(* Simulate the pipeline. *)
let test sim =
  let a = Cyclesim.in_port sim "a" in
  let b = Cyclesim.in_port sim "b" in
  let sum = Cyclesim.out_port sim "sum" in
  let prev_result = ref 0 in
  for i = 0 to (16 * 16) - 1 do
    let x = Bits.of_int_trunc ~width:8 i in
    a := x.Bits.:[3, 0];
    b := x.Bits.:[7, 4];
    Cyclesim.cycle sim;
    if i > 0
    then
      if Bits.to_int_trunc !sum <> !prev_result
      then Stdio.printf "%i\n" (Bits.to_int_trunc !sum);
    prev_result := ((i lsr 4) + (i land 15)) land 15
  done;
  Cyclesim.cycle sim;
  if Bits.to_int_trunc !sum <> !prev_result
  then Stdio.printf "%i\n" (Bits.to_int_trunc !sum)
;;

let simulate ~part_width = test (sim ~part_width)

let%expect_test "simulation" =
  simulate ~part_width:1;
  simulate ~part_width:2
;;
