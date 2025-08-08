open Import

let sim ~part_width ~adder_width =
  let clock = Signal.input "clock" 1 in
  let clear = Signal.input "clear" 1 in
  let a = Signal.input "a" adder_width in
  let b = Signal.input "b" adder_width in
  let c = Pipelined_adder.create ~part_width ~clock ~clear a b in
  let circuit = Circuit.create_exn ~name:"pipelined_adder" [ Signal.output "c" c ] in
  let sim =
    Cyclesim.create ~config:{ Cyclesim.Config.default with store_circuit = true } circuit
  in
  sim
;;

let test ~sim ~part_width ~adder_width ~num_tests =
  let a_d = Array.init num_tests ~f:(fun _ -> Bits.random ~width:adder_width) in
  let b_d = Array.init num_tests ~f:(fun _ -> Bits.random ~width:adder_width) in
  let a, b, c =
    Cyclesim.in_port sim "a", Cyclesim.in_port sim "b", Cyclesim.out_port sim "c"
  in
  let cycles = ((adder_width + part_width - 1) / part_width) - 1 in
  for i = 0 to num_tests + cycles - 1 do
    if i < num_tests
    then (
      a := a_d.(i);
      b := b_d.(i));
    Cyclesim.cycle sim;
    if i >= cycles
    then (
      let i = i - cycles in
      let expected = Bits.(a_d.(i) +: b_d.(i)) in
      if not (Bits.equal !c expected)
      then
        raise_s
          [%message
            "adder mismatch"
              (i : int)
              (cycles : int)
              (part_width : int)
              (adder_width : int)
              (Bits.to_int_trunc a_d.(i) : Int.Hex.t)
              (Bits.to_int_trunc b_d.(i) : Int.Hex.t)
              (Bits.to_int_trunc expected : Int.Hex.t)
              (Bits.to_int_trunc !c : Int.Hex.t)])
  done
;;

let test_randomly ~part_width ~adder_width ~num_tests =
  let sim = sim ~part_width ~adder_width in
  test ~sim ~part_width ~adder_width ~num_tests
;;

let%expect_test "adder 8/4" =
  test_randomly ~part_width:4 ~adder_width:8 ~num_tests:1000;
  [%expect {| |}]
;;

let%expect_test "adder 8/1" =
  test_randomly ~part_width:1 ~adder_width:8 ~num_tests:1000;
  [%expect {| |}]
;;

let%expect_test "adder 32/8" =
  test_randomly ~part_width:8 ~adder_width:32 ~num_tests:1000;
  [%expect {| |}]
;;

let%expect_test "adder 57/7" =
  test_randomly ~part_width:7 ~adder_width:57 ~num_tests:1000;
  [%expect {| |}]
;;
