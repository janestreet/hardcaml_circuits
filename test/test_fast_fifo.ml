open! Core
open Hardcaml
open Hardcaml_circuits
open Hardcaml_waveterm

module Foo = struct
  type 'a t =
    { hello : 'a [@bits 16]
    ; world : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module Fast_fifo = Fast_fifo.Make (Foo)
module Sim = Cyclesim.With_interface (Fast_fifo.I) (Fast_fifo.O)

let create_sim ?(capacity = 2) ?(cut_through = true) () =
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create
      ~config:{ Cyclesim.Config.default with store_circuit = true }
      (Fast_fifo.create ~cut_through ~capacity scope)
  in
  let waves, sim = Hardcaml_waveterm.Waveform.create sim in
  waves, sim
;;

let expect waves = Waveform.expect ~wave_width:2 ~display_width:86 waves

let get_inputs_and_clear (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  inputs
;;

let combinational_read_write (sim : Sim.t) =
  let inputs = get_inputs_and_clear sim in
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 0x123;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 0x456;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  Cyclesim.cycle sim
;;

let%expect_test "combinational read/write" =
  let waves, sim = create_sim () in
  combinational_read_write sim;
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └───────────                                              │
    │rd_enable         ││      ┌───────────                                              │
    │                  ││──────┘                                                         │
    │                  ││──────┬───────────                                              │
    │wr$hello          ││ 0000 │0123                                                     │
    │                  ││──────┴───────────                                              │
    │                  ││──────┬───────────                                              │
    │wr$world          ││ 0000 │0456                                                     │
    │                  ││──────┴───────────                                              │
    │wr_enable         ││      ┌─────┐                                                   │
    │                  ││──────┘     └─────                                              │
    │full              ││                                                                │
    │                  ││──────────────────                                              │
    │one_from_full     ││                                                                │
    │                  ││──────────────────                                              │
    │                  ││──────┬───────────                                              │
    │rd$hello          ││ 0000 │0123                                                     │
    │                  ││──────┴───────────                                              │
    │                  ││──────┬───────────                                              │
    │rd$world          ││ 0000 │0456                                                     │
    │                  ││──────┴───────────                                              │
    │rd_valid          ││      ┌─────┐                                                   │
    │                  ││──────┘     └─────                                              │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    de447d24f72f2a8b5ec95ec3ef6048b7
    |}]
;;

let read_exact_one_cycle_after_write (sim : Sim.t) =
  let inputs = get_inputs_and_clear sim in
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 0x123;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 0x456;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim
;;

let%expect_test "read exact one-cycle after write" =
  let waves, sim = create_sim () in
  read_exact_one_cycle_after_write sim;
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────                                        │
    │rd_enable         ││            ┌─────┐                                             │
    │                  ││────────────┘     └─────                                        │
    │                  ││──────┬─────────────────                                        │
    │wr$hello          ││ 0000 │0123                                                     │
    │                  ││──────┴─────────────────                                        │
    │                  ││──────┬─────────────────                                        │
    │wr$world          ││ 0000 │0456                                                     │
    │                  ││──────┴─────────────────                                        │
    │wr_enable         ││      ┌─────┐                                                   │
    │                  ││──────┘     └───────────                                        │
    │full              ││                                                                │
    │                  ││────────────────────────                                        │
    │one_from_full     ││                                                                │
    │                  ││────────────────────────                                        │
    │                  ││──────┬─────────────────                                        │
    │rd$hello          ││ 0000 │0123                                                     │
    │                  ││──────┴─────────────────                                        │
    │                  ││──────┬─────────────────                                        │
    │rd$world          ││ 0000 │0456                                                     │
    │                  ││──────┴─────────────────                                        │
    │rd_valid          ││      ┌───────────┐                                             │
    │                  ││──────┘           └─────                                        │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    46fb6adba25232f37777aa8c9cbcd792
    |}]
;;

let read_and_write_same_cycle_when_not_empty (sim : Sim.t) =
  let inputs = get_inputs_and_clear sim in
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 0x123;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 0x456;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 0xabc;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 0xdef;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim
;;

let%expect_test "read and write at the same cycle when underlying fifo not empty" =
  let waves, sim = create_sim () in
  read_and_write_same_cycle_when_not_empty sim;
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────────────────                            │
    │rd_enable         ││            ┌─────┐     ┌─────┐                                 │
    │                  ││────────────┘     └─────┘     └─────                            │
    │                  ││──────┬─────┬───────────────────────                            │
    │wr$hello          ││ 0000 │0123 │0ABC                                               │
    │                  ││──────┴─────┴───────────────────────                            │
    │                  ││──────┬─────┬───────────────────────                            │
    │wr$world          ││ 0000 │0456 │0DEF                                               │
    │                  ││──────┴─────┴───────────────────────                            │
    │wr_enable         ││      ┌───────────┐                                             │
    │                  ││──────┘           └─────────────────                            │
    │full              ││                                                                │
    │                  ││────────────────────────────────────                            │
    │one_from_full     ││                                                                │
    │                  ││────────────────────────────────────                            │
    │                  ││──────┬───────────┬─────────────────                            │
    │rd$hello          ││ 0000 │0123       │0ABC                                         │
    │                  ││──────┴───────────┴─────────────────                            │
    │                  ││──────┬───────────┬─────────────────                            │
    │rd$world          ││ 0000 │0456       │0DEF                                         │
    │                  ││──────┴───────────┴─────────────────                            │
    │rd_valid          ││      ┌───────────────────────┐                                 │
    │                  ││──────┘                       └─────                            │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    8a7c5f3e1def34159e70e7e009282add
    |}]
;;

let read_and_write_same_cycle_when_empty (sim : Sim.t) =
  let inputs = get_inputs_and_clear sim in
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 0x123;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 0x456;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 0xabc;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 0xdef;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim
;;

let%expect_test "read and write at the same cycle when empty" =
  let waves, sim = create_sim ~cut_through:false () in
  read_and_write_same_cycle_when_empty sim;
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────────────────                            │
    │rd_enable         ││      ┌───────────┐     ┌─────┐                                 │
    │                  ││──────┘           └─────┘     └─────                            │
    │                  ││──────┬─────┬───────────────────────                            │
    │wr$hello          ││ 0000 │0123 │0ABC                                               │
    │                  ││──────┴─────┴───────────────────────                            │
    │                  ││──────┬─────┬───────────────────────                            │
    │wr$world          ││ 0000 │0456 │0DEF                                               │
    │                  ││──────┴─────┴───────────────────────                            │
    │wr_enable         ││      ┌───────────┐                                             │
    │                  ││──────┘           └─────────────────                            │
    │full              ││                                                                │
    │                  ││────────────────────────────────────                            │
    │one_from_full     ││                                                                │
    │                  ││────────────────────────────────────                            │
    │                  ││────────────┬─────┬───────────┬─────                            │
    │rd$hello          ││ 0000       │0123 │0ABC       │0000                             │
    │                  ││────────────┴─────┴───────────┴─────                            │
    │                  ││────────────┬─────┬───────────┬─────                            │
    │rd$world          ││ 0000       │0456 │0DEF       │0000                             │
    │                  ││────────────┴─────┴───────────┴─────                            │
    │rd_valid          ││            ┌─────────────────┐                                 │
    │                  ││────────────┘                 └─────                            │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    68b0f5d98279b9693ae208d0434e556f
    |}]
;;

let write_when_full_not_registered (sim : Sim.t) =
  let inputs = get_inputs_and_clear sim in
  for i = 1 to 4 do
    inputs.wr_enable := Bits.vdd;
    inputs.wr_data.hello := Bits.of_int_trunc ~width:16 i;
    inputs.wr_data.world := Bits.of_int_trunc ~width:16 i;
    Cyclesim.cycle sim
  done;
  (* Demonstrate behaviour of writing & reading at the very cycle the fifo gets full. *)
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 5;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 5;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 0;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 0;
  inputs.wr_enable := Bits.gnd;
  for _ = 2 to 5 do
    Cyclesim.cycle sim
  done;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim
;;

let%expect_test "write when full will not be registered" =
  let waves, sim = create_sim () in
  write_when_full_not_registered sim;
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────────────────────────────────────────────│
    │rd_enable         ││                              ┌─────────────────────────────┐   │
    │                  ││──────────────────────────────┘                             └───│
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬───────────────────────────│
    │wr$hello          ││ 0000 │0001 │0002 │0003 │0004 │0005 │0000                       │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴───────────────────────────│
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬───────────────────────────│
    │wr$world          ││ 0000 │0001 │0002 │0003 │0004 │0005 │0000                       │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴───────────────────────────│
    │wr_enable         ││      ┌─────────────────────────────┐                           │
    │                  ││──────┘                             └───────────────────────────│
    │full              ││                        ┌───────────┐                           │
    │                  ││────────────────────────┘           └───────────────────────────│
    │one_from_full     ││                  ┌───────────────────────┐                     │
    │                  ││──────────────────┘                       └─────────────────────│
    │                  ││──────┬─────────────────────────────┬─────┬─────┬───────────────│
    │rd$hello          ││ 0000 │0001                         │0002 │0003 │0000           │
    │                  ││──────┴─────────────────────────────┴─────┴─────┴───────────────│
    │                  ││──────┬─────────────────────────────┬─────┬─────┬───────────────│
    │rd$world          ││ 0000 │0001                         │0002 │0003 │0000           │
    │                  ││──────┴─────────────────────────────┴─────┴─────┴───────────────│
    │rd_valid          ││      ┌─────────────────────────────────────────┐               │
    │                  ││──────┘                                         └───────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    df8a3e1f51076507ae217ec91f666548
    |}]
;;

let demo_rd_en_1_rd_valid_0_until_available (sim : Sim.t) =
  let inputs = get_inputs_and_clear sim in
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int_trunc ~width:16 0xFFFF;
  inputs.wr_data.world := Bits.of_int_trunc ~width:16 0xFFFF;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  Cyclesim.cycle sim
;;

let%expect_test "demonstrate [rd_enable=1], [rd_valid=0], until data is really available."
  =
  let waves, sim = create_sim () in
  demo_rd_en_1_rd_valid_0_until_available sim;
  expect waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────────────────                            │
    │rd_enable         ││      ┌─────────────────────────────                            │
    │                  ││──────┘                                                         │
    │                  ││────────────────────────┬───────────                            │
    │wr$hello          ││ 0000                   │FFFF                                   │
    │                  ││────────────────────────┴───────────                            │
    │                  ││────────────────────────┬───────────                            │
    │wr$world          ││ 0000                   │FFFF                                   │
    │                  ││────────────────────────┴───────────                            │
    │wr_enable         ││                        ┌─────┐                                 │
    │                  ││────────────────────────┘     └─────                            │
    │full              ││                                                                │
    │                  ││────────────────────────────────────                            │
    │one_from_full     ││                                                                │
    │                  ││────────────────────────────────────                            │
    │                  ││────────────────────────┬───────────                            │
    │rd$hello          ││ 0000                   │FFFF                                   │
    │                  ││────────────────────────┴───────────                            │
    │                  ││────────────────────────┬───────────                            │
    │rd$world          ││ 0000                   │FFFF                                   │
    │                  ││────────────────────────┴───────────                            │
    │rd_valid          ││                        ┌─────┐                                 │
    │                  ││────────────────────────┘     └─────                            │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    711dcff48a3624d70f5606109032ca2d
    |}]
;;
