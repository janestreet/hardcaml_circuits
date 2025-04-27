open Base
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_circuits

let init ~cycles ~num_data =
  let open Signal in
  let clock = input "clock" 1 in
  let clear = input "clear" 1 in
  let data = List.init num_data ~f:(fun i -> of_int_trunc ~width:8 (i + 1)) in
  let selector = input "selector" (Bits.address_bits_for (List.length data)) in
  let value =
    Pipelined_tree_mux.pipelined_tree_mux
      ~cycles
      ~reg:(reg (Reg_spec.create ~clock ~clear ()))
      ~selector
      data
  in
  let value = output "value" value in
  let sim = Cyclesim.create (Circuit.create_exn ~name:"pipelined_tree_mux" [ value ]) in
  let open Bits in
  let waves, sim = Waveform.create sim in
  let selector = Cyclesim.in_port sim "selector" in
  for i = 0 to num_data - 1 do
    selector <--. i;
    Cyclesim.cycle sim
  done;
  for _ = 0 to cycles - 1 do
    Cyclesim.cycle sim
  done;
  waves
;;

let%expect_test "" =
  let waves = init ~cycles:1 ~num_data:4 in
  Waveform.print ~display_width:86 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││                                                                │
    │                  ││────────────────────                                            │
    │                  ││────┬───┬───┬───────                                            │
    │selector          ││ 0  │1  │2  │3                                                  │
    │                  ││────┴───┴───┴───────                                            │
    │                  ││────┬───┬───┬───┬───                                            │
    │value             ││ 00 │01 │02 │03 │04                                             │
    │                  ││────┴───┴───┴───┴───                                            │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "" =
  let waves = init ~cycles:2 ~num_data:4 in
  Waveform.print ~display_width:86 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││                                                                │
    │                  ││────────────────────────                                        │
    │                  ││────┬───┬───┬───────────                                        │
    │selector          ││ 0  │1  │2  │3                                                  │
    │                  ││────┴───┴───┴───────────                                        │
    │                  ││────────┬───┬───┬───┬───                                        │
    │value             ││ 00     │01 │02 │03 │04                                         │
    │                  ││────────┴───┴───┴───┴───                                        │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "" =
  let waves = init ~cycles:4 ~num_data:2 in
  Waveform.print ~display_width:86 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││                                                                │
    │                  ││────────────────────────                                        │
    │selector          ││    ┌───────────────────                                        │
    │                  ││────┘                                                           │
    │                  ││────────────────┬───┬───                                        │
    │value             ││ 00             │01 │02                                         │
    │                  ││────────────────┴───┴───                                        │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "" =
  let waves = init ~cycles:4 ~num_data:9 in
  Waveform.print ~display_width:86 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││                                                                │
    │                  ││────────────────────────────────────────────────────            │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───────────────────            │
    │selector          ││ 0  │1  │2  │3  │4  │5  │6  │7  │8                              │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───────────────────            │
    │                  ││────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───            │
    │value             ││ 00             │01 │02 │03 │04 │05 │06 │07 │08 │09             │
    │                  ││────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───            │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;
