open! Import
open Hardcaml_waveterm

let%expect_test "sum stages" =
  let stages = Stages.create 10 ~init:0 ~f:(fun _i x -> x + 1) in
  print_s [%message (stages : int Stages.t array)];
  [%expect
    {|
    (stages (
      ((input 0) (output 1))
      ((input 1) (output 2))
      ((input 2) (output 3))
      ((input 3) (output 4))
      ((input 4) (output 5))
      ((input 5) (output 6))
      ((input 6) (output 7))
      ((input 7) (output 8))
      ((input 8) (output 9))
      ((input 9) (output 10))))
    |}];
  let stages = Stages.create 10 ~init:0 ~f:(fun i x -> x + i) in
  print_s [%message (stages : int Stages.t array)];
  [%expect
    {|
    (stages (
      ((input 0)  (output 0))
      ((input 0)  (output 1))
      ((input 1)  (output 3))
      ((input 3)  (output 6))
      ((input 6)  (output 10))
      ((input 10) (output 15))
      ((input 15) (output 21))
      ((input 21) (output 28))
      ((input 28) (output 36))
      ((input 36) (output 45))))
    |}]
;;

let%expect_test "pipeline" =
  let d = Signal.input "d" 8 in
  let q =
    Stages.pipeline
      (Reg_spec.create ~clock:(Signal.input "clock" 1) ())
      3
      ~enable:Signal.vdd
      ~init:d
      ~f:(Fn.const Fn.id)
  in
  let sim =
    Cyclesim.create
      (Circuit.create_exn ~name:"pipeline" [ Signal.output "q" (Stages.output q) ])
  in
  let waves, sim = Waveform.create sim in
  let d = Cyclesim.in_port sim "d" in
  for i = 5 to 9 do
    d := Bits.of_int ~width:8 i;
    Cyclesim.cycle sim
  done;
  for _ = 0 to 3 do
    d := Bits.of_int ~width:8 0;
    Cyclesim.cycle sim
  done;
  Waveform.expect ~display_width:84 ~display_height:10 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
    │                  ││──────┬─────┬─────┬─────┬─────┬───────────────────────        │
    │d                 ││ 05   │06   │07   │08   │09   │00                             │
    │                  ││──────┴─────┴─────┴─────┴─────┴───────────────────────        │
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬─────┬─────        │
    │q                 ││ 00               │05   │06   │07   │08   │09   │00           │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴─────┴─────        │
    └──────────────────┘└──────────────────────────────────────────────────────────────┘
    19fb3fdb863d6393440c3174fc641f00
    |}]
;;

let%expect_test "pipeline with enable" =
  let d = Signal.input "d" 8 in
  let q =
    Stages.pipeline_with_enable
      (Reg_spec.create ~clock:(Signal.input "clock" 1) ())
      3
      ~enable:(Signal.input "enable" 1)
      ~init:d
      ~f:(fun i d -> if i = 1 then Signal.( +:. ) d 1 else d)
  in
  let sim =
    let { Stages.data; enable } = Stages.output q in
    Cyclesim.create
      (Circuit.create_exn
         ~name:"pipeline"
         [ Signal.output "q" data; Signal.output "valid" enable ])
  in
  let waves, sim = Waveform.create sim in
  let enable = Cyclesim.in_port sim "enable" in
  let d = Cyclesim.in_port sim "d" in
  enable := Bits.vdd;
  for i = 1 to 2 do
    d := Bits.of_int ~width:8 i;
    Cyclesim.cycle sim
  done;
  enable := Bits.gnd;
  d := Bits.of_int ~width:8 0xee;
  Cyclesim.cycle sim;
  enable := Bits.vdd;
  d := Bits.of_int ~width:8 0xfe;
  Cyclesim.cycle sim;
  enable := Bits.gnd;
  for _ = 0 to 3 do
    Cyclesim.cycle sim
  done;
  Waveform.expect ~display_width:84 ~display_height:15 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
    │enable            ││────────────┐     ┌─────┐                                     │
    │                  ││            └─────┘     └───────────────────────              │
    │                  ││──────┬─────┬─────┬─────────────────────────────              │
    │d                 ││ 01   │02   │EE   │FE                                         │
    │                  ││──────┴─────┴─────┴─────────────────────────────              │
    │                  ││──────────────────┬─────┬───────────┬───────────              │
    │q                 ││ 00               │02   │03         │FF                       │
    │                  ││──────────────────┴─────┴───────────┴───────────              │
    │valid             ││                  ┌───────────┐     ┌─────┐                   │
    │                  ││──────────────────┘           └─────┘     └─────              │
    │                  ││                                                              │
    └──────────────────┘└──────────────────────────────────────────────────────────────┘
    80d601d312c1a1050d63faaab6f39939
    |}]
;;
