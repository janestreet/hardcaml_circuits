open Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_circuits

module Sim (Config : Counter_div_mod.Config) = struct
  module Counter_div_mod = Counter_div_mod.Make (Config)
  module Sim = Cyclesim.With_interface (Counter_div_mod.I) (Counter_div_mod.O)

  let create_sim () =
    let scope = Scope.create ~flatten_design:true () in
    Sim.create
      ~config:{ Cyclesim.Config.default with store_circuit = true }
      (Counter_div_mod.create scope)
  ;;

  let run_test
    ~(set_input_list : (int * int) option list)
    ~(input_list : bool list)
    (sim : Sim.t)
    =
    let waves, sim = Hardcaml_waveterm.Waveform.create sim in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    inputs.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;
    let conv x = Bits.to_int_trunc !x in
    let first_output = Counter_div_mod.O.map outputs ~f:conv in
    let outputs =
      List.map2_exn set_input_list input_list ~f:(fun set_opt incr ->
        inputs.increment := Bits.of_bool incr;
        (match set_opt with
         | None -> inputs.set := Bits.gnd
         | Some (q, r) ->
           inputs.set := Bits.vdd;
           inputs.set_quotient := Bits.of_int_trunc ~width:Counter_div_mod.quotient_bits q;
           inputs.set_remainder
           := Bits.of_int_trunc ~width:Counter_div_mod.remainder_bits r);
        Cyclesim.cycle sim;
        Counter_div_mod.O.map outputs ~f:conv)
    in
    inputs.increment := Bits.gnd;
    Cyclesim.cycle sim;
    waves, first_output :: outputs
  ;;

  let testbench ~(set_input_list : (int * int) option list) ~(input_list : bool list) =
    let sim = create_sim () in
    run_test ~set_input_list ~input_list sim
  ;;

  let random_test sim =
    let open Config in
    let max_value = (max_quotient * divisor) + max_remainder in
    let inputs = List.init ((2 * max_value) + 2) ~f:(Fn.const true) in
    let set_input_list = List.init (List.length inputs) ~f:(Fn.const None) in
    let _waves, outputs = run_test ~set_input_list ~input_list:inputs sim in
    let prev_value = ref (-1) in
    List.iter outputs ~f:(fun { quotient; remainder } ->
      let current_value = (quotient * divisor) + remainder in
      let pass =
        if !prev_value = max_value
        then current_value = 0
        else current_value = !prev_value + 1
      in
      if not pass
      then
        raise_s
          [%message
            (max_value : int)
              (divisor : int)
              (!prev_value : int)
              (current_value : int)
              (quotient : int)
              (remainder : int)];
      prev_value := current_value)
  ;;
end

let expect waves = Waveform.expect ~wave_width:2 ~display_width:86 waves

let%expect_test "custom" =
  let module Config = struct
    let max_quotient = 2
    let max_remainder = 0
    let divisor = 3
  end
  in
  let module Sim = Sim (Config) in
  (* input list is [incr] signal *)
  (* set_input_list is None meaning no set, or Some (quotient, remainder) when setting. *)
  let input_list = [ true; true; false; true; true; true; false; true; true ] in
  let set_input_list = List.init (List.length input_list) ~f:(Fn.const None) in
  let waves, _outputs = Sim.testbench ~set_input_list ~input_list in
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────────────────────────────────────────────│
    │increment         ││      ┌───────────┐     ┌─────────────────┐     ┌───────────┐   │
    │                  ││──────┘           └─────┘                 └─────┘           └───│
    │set               ││                                                                │
    │                  ││────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────│
    │set_quotient      ││ 0                                                              │
    │                  ││────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────│
    │set_remainder     ││ 0                                                              │
    │                  ││────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────┬───────────────────────┬─────┬───│
    │quotient          ││ 0                            │1                      │2    │0  │
    │                  ││──────────────────────────────┴───────────────────────┴─────┴───│
    │                  ││────────────┬─────┬───────────┬─────┬─────┬───────────┬─────────│
    │remainder         ││ 0          │1    │2          │0    │1    │2          │0        │
    │                  ││────────────┴─────┴───────────┴─────┴─────┴───────────┴─────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    7c3050d4d426f09cd7ca818604fb44bc
    |}]
;;

let%expect_test "divisor = 1" =
  let module Config = struct
    let max_quotient = 3
    let max_remainder = 0
    let divisor = 1
  end
  in
  let module Sim = Sim (Config) in
  (* input list is [incr] signal *)
  (* set_input_list is None meaning no set, or Some (quotient, remainder) when setting. *)
  let input_list = [ true; true; false; true; true; true; false; true; true ] in
  let set_input_list = List.init (List.length input_list) ~f:(Fn.const None) in
  let waves, _outputs = Sim.testbench ~set_input_list ~input_list in
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────────────────────────────────────────────│
    │increment         ││      ┌───────────┐     ┌─────────────────┐     ┌───────────┐   │
    │                  ││──────┘           └─────┘                 └─────┘           └───│
    │set               ││                                                                │
    │                  ││────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────│
    │set_quotient      ││ 0                                                              │
    │                  ││────────────────────────────────────────────────────────────────│
    │set_remainder     ││                                                                │
    │                  ││────────────────────────────────────────────────────────────────│
    │                  ││────────────┬─────┬───────────┬─────┬─────┬───────────┬─────┬───│
    │quotient          ││ 0          │1    │2          │3    │0    │1          │2    │3  │
    │                  ││────────────┴─────┴───────────┴─────┴─────┴───────────┴─────┴───│
    │remainder         ││                                                                │
    │                  ││────────────────────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    e911836d727b90f77327c7205d54ff5f
    |}]
;;

let%expect_test "set" =
  let module Config = struct
    let max_quotient = 2
    let max_remainder = 0
    let divisor = 2
  end
  in
  let module Sim = Sim (Config) in
  (* input list is [incr] signal *)
  (* set_input_list is None = no set, or Some (quotient, remainder) when setting. *)
  let input_list = [ true; true; true; false; false; true ] in
  let set_input_list = [ None; Some (2, 0); None; None; Some (0, 1); None ] in
  let waves, _outputs = Sim.testbench ~set_input_list ~input_list in
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────────────────────────────                │
    │increment         ││      ┌─────────────────┐           ┌─────┐                     │
    │                  ││──────┘                 └───────────┘     └─────                │
    │set               ││            ┌─────┐           ┌─────┐                           │
    │                  ││────────────┘     └───────────┘     └───────────                │
    │                  ││────────────┬─────────────────┬─────────────────                │
    │set_quotient      ││ 0          │2                │0                                │
    │                  ││────────────┴─────────────────┴─────────────────                │
    │set_remainder     ││                              ┌─────────────────                │
    │                  ││──────────────────────────────┘                                 │
    │                  ││──────────────────┬─────┬─────────────────┬─────                │
    │quotient          ││ 0                │2    │0                │1                    │
    │                  ││──────────────────┴─────┴─────────────────┴─────                │
    │remainder         ││            ┌─────┐                 ┌─────┐                     │
    │                  ││────────────┘     └─────────────────┘     └─────                │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    0040c1caf60424948b122a1e2f09610c
    |}]
;;

let%expect_test "iterate configs" =
  for max_quotient = 1 to 10 do
    for divisor = 1 to 10 do
      for max_remainder = 0 to divisor - 1 do
        let module Config = struct
          let max_quotient = max_quotient
          let max_remainder = max_remainder
          let divisor = divisor
        end
        in
        let module Sim = Sim (Config) in
        Sim.random_test (Sim.create_sim ())
      done
    done
  done;
  [%expect {| |}]
;;
