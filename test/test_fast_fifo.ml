open Hardcaml
open Hardcaml_circuits
open Hardcaml_waveterm

module Foo = struct
  type 'a t =
    { hello : 'a [@bits 16]
    ; world : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module Fast_fifo = Fast_fifo.Make (Foo)

let create_sim ?(capacity = 2) () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface (Fast_fifo.I) (Fast_fifo.O) in
  let sim = Sim.create (Fast_fifo.create ~cut_through:true ~capacity scope) in
  let waves, sim = Hardcaml_waveterm.Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  waves, sim
;;

let expect waves =
  Waveform.expect ~wave_width:2 ~display_height:28 ~display_width:86 waves
;;

let%expect_test "combinational read/write" =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int ~width:16 0x123;
  inputs.wr_data.world := Bits.of_int ~width:16 0x456;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  Cyclesim.cycle sim;
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
    af5bc98dfbf6793bc560d9296dce719e |}]
;;

let%expect_test "read exact one-cycle after write" =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int ~width:16 0x123;
  inputs.wr_data.world := Bits.of_int ~width:16 0x456;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim;
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
    371846e3a268398984b86ecb224a9406 |}]
;;

let%expect_test "read and write at the same cycle when underlying fifo not empty" =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int ~width:16 0x123;
  inputs.wr_data.world := Bits.of_int ~width:16 0x456;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int ~width:16 0xabc;
  inputs.wr_data.world := Bits.of_int ~width:16 0xdef;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim;
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
    57e438bfa5b572acaa0e2e4820657947 |}]
;;

let%expect_test "write when full will not be registered" =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  for i = 1 to 4 do
    inputs.wr_enable := Bits.vdd;
    inputs.wr_data.hello := Bits.of_int ~width:16 i;
    inputs.wr_data.world := Bits.of_int ~width:16 i;
    Cyclesim.cycle sim
  done;
  (* Demonstrate behaviour of writing & reading at the very cycle the fifo gets full. *)
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int ~width:16 5;
  inputs.wr_data.world := Bits.of_int ~width:16 5;
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.wr_data.hello := Bits.of_int ~width:16 0;
  inputs.wr_data.world := Bits.of_int ~width:16 0;
  inputs.wr_enable := Bits.gnd;
  for _ = 2 to 5 do
    Cyclesim.cycle sim
  done;
  inputs.rd_enable := Bits.gnd;
  Cyclesim.cycle sim;
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
    124d22cfcc680b49e42fb3131cecb36b |}]
;;

let%expect_test "demonstrate [rd_enable=1], [rd_valid=0], until data is really available."
  =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  inputs.rd_enable := Bits.vdd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.vdd;
  inputs.wr_data.hello := Bits.of_int ~width:16 0xFFFF;
  inputs.wr_data.world := Bits.of_int ~width:16 0xFFFF;
  Cyclesim.cycle sim;
  inputs.wr_enable := Bits.gnd;
  Cyclesim.cycle sim;
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
    118f152a56c7c6aa84cadb78081dfaa6 |}]
;;
