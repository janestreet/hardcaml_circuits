open Base
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_circuits

let test ~arity n =
  let clock = Signal.input "clock" 1 in
  let numbers = List.init n ~f:(fun i -> Signal.input ("x_" ^ Int.to_string i) 32) in
  let enable = Signal.input "enable" 1 in
  let spec = Reg_spec.create ~clock () in
  let output =
    Pipelined_tree_reduce.create ~f:Signal.( +: ) ~enable ~arity spec numbers
  in
  let circuit =
    Circuit.create_exn
      ~name:"tree_adder"
      [ Signal.( -- ) (Signal.wireof output.valid) "a_valid"
      ; Signal.( -- ) (Signal.wireof output.value) "a_value"
      ]
  in
  let sim = Cyclesim.create circuit in
  let waves, sim = Waveform.create sim in
  let get_input name =
    List.find_exn ~f:(fun (a, _) -> String.equal name a) (Cyclesim.inputs sim) |> snd
  in
  (* First request *)
  get_input "enable" := Bits.vdd;
  for i = 0 to n - 1 do
    get_input ("x_" ^ Int.to_string i) := Bits.of_int ~width:32 (i + 1)
  done;
  Cyclesim.cycle sim;
  get_input "enable" := Bits.gnd;
  Cyclesim.cycle sim;
  (*
     Second request
  *)
  get_input "enable" := Bits.vdd;
  for i = 0 to n - 1 do
    get_input ("x_" ^ Int.to_string i) := Bits.of_int ~width:32 (2 * (i + 1))
  done;
  Cyclesim.cycle sim;
  get_input "enable" := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Core.print_s
    [%message
      ""
        ~expected_answer_1:(n * (1 + n) / 2 : int)
        ~expected_answer_2:(n * (1 + n) / 2 * 2 : int)];
  Waveform.expect
    ~display_width:80
    ~display_height:13
    ~display_rules:
      Display_rule.
        [ port_name_is "clock" ~wave_format:Bit
        ; port_name_is "enable" ~wave_format:Bit
        ; port_name_is "a_valid" ~wave_format:Bit
        ; port_name_is "a_value" ~wave_format:Unsigned_int
        ]
    waves
;;

let%expect_test "3 pipelined stages" =
  test ~arity:3 27;
  [%expect
    {|
    ((expected_answer_1 378) (expected_answer_2 756))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │enable            ││────────┐       ┌───────┐                                 │
    │                  ││        └───────┘       └─────────────────────────────────│
    │a_valid           ││                        ┌───────┐       ┌───────┐         │
    │                  ││────────────────────────┘       └───────┘       └─────────│
    │                  ││────────────────────────┬───────────────┬─────────────────│
    │a_value           ││ 0                      │378            │756              │
    │                  ││────────────────────────┴───────────────┴─────────────────│
    │                  ││                                                          │
    │                  ││                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    65f0df6dcfdced833cf1302c1f8e6494 |}]
;;

let%expect_test "only 1 argument" =
  test ~arity:3 1;
  [%expect
    {|
    ((expected_answer_1 1) (expected_answer_2 2))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │enable            ││────────┐       ┌───────┐                                 │
    │                  ││        └───────┘       └─────────────────────────────────│
    │a_valid           ││────────┐       ┌───────┐                                 │
    │                  ││        └───────┘       └─────────────────────────────────│
    │                  ││────────────────┬─────────────────────────────────────────│
    │a_value           ││ 1              │2                                        │
    │                  ││────────────────┴─────────────────────────────────────────│
    │                  ││                                                          │
    │                  ││                                                          │
    │                  ││                                                          │
    │                  ││                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    9a7410754234ccf8705a3c3d39e3a701 |}]
;;

let%expect_test "less than arity, but more than 1" =
  test ~arity:32 26;
  [%expect
    {|
    ((expected_answer_1 351) (expected_answer_2 702))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │enable            ││────────┐       ┌───────┐                                 │
    │                  ││        └───────┘       └─────────────────────────────────│
    │a_valid           ││        ┌───────┐       ┌───────┐                         │
    │                  ││────────┘       └───────┘       └─────────────────────────│
    │                  ││────────┬───────────────┬─────────────────────────────────│
    │a_value           ││ 0      │351            │702                              │
    │                  ││────────┴───────────────┴─────────────────────────────────│
    │                  ││                                                          │
    │                  ││                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    414ccd8909704037d1f3300f7d85d963 |}]
;;
