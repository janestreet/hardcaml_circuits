open Import
open Hardcaml_waveterm

let sim ~part_width ~increment_width ~total_width =
  let clock = Signal.input "clock" 1 in
  let clear = Signal.input "clear" 1 in
  let set =
    { With_valid.valid = Signal.input "set" 1; value = Signal.input "value" total_width }
  in
  let increment = Signal.input "increment" increment_width in
  let q = Pipelined_incrementer.create ~part_width ~clock ~clear ~set ~increment in
  let circuit = Circuit.create_exn ~name:"pipelined_incr" [ Signal.output "q" q ] in
  let sim = Cyclesim.create circuit in
  let increment, set, value, q =
    ( Cyclesim.in_port sim "increment"
    , Cyclesim.in_port sim "set"
    , Cyclesim.in_port sim "value"
    , Cyclesim.out_port sim "q" )
  in
  let waves, sim = Waveform.create sim in
  sim, waves, increment, set, value, q
;;

let%expect_test "increment by 1" =
  let sim, waves, increment, _set, _value, _q =
    sim ~part_width:2 ~increment_width:2 ~total_width:4
  in
  increment := Bits.of_int ~width:2 1;
  for _ = 0 to 30 do
    Cyclesim.cycle sim
  done;
  Waveform.expect ~wave_width:0 ~display_width:86 ~display_height:17 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──────────────────────────────────────────────────────────────  │
    │increment         ││ 1                                                              │
    │                  ││──────────────────────────────────────────────────────────────  │
    │set               ││                                                                │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──────────────────────────────────────────────────────────────  │
    │value             ││ 0                                                              │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─  │
    │q                 ││ 0  │1│2│3│4│5│6│7│8│9│A│B│C│D│E│F│0│1│2│3│4│5│6│7│8│9│A│B│C│D  │
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─  │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    ed498a993d74f63af1b39fea2bddf255
    |}]
;;

let%expect_test "increment by 3" =
  let sim, waves, increment, _set, _value, _q =
    sim ~part_width:2 ~increment_width:2 ~total_width:4
  in
  increment := Bits.of_int ~width:2 3;
  for _ = 0 to 30 do
    Cyclesim.cycle sim
  done;
  Waveform.expect ~wave_width:0 ~display_width:86 ~display_height:17 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──────────────────────────────────────────────────────────────  │
    │increment         ││ 3                                                              │
    │                  ││──────────────────────────────────────────────────────────────  │
    │set               ││                                                                │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──────────────────────────────────────────────────────────────  │
    │value             ││ 0                                                              │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─  │
    │q                 ││ 0  │3│6│9│C│F│2│5│8│B│E│1│4│7│A│D│0│3│6│9│C│F│2│5│8│B│E│1│4│7  │
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─  │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    bf8f8ed4bed52fc42216c46e2a86202f
    |}]
;;

let%expect_test "increment by differing amounts" =
  let sim, waves, increment, _set, _value, _q =
    sim ~part_width:2 ~increment_width:2 ~total_width:4
  in
  for i = 0 to 30 do
    increment := Bits.of_int ~width:2 (i % 4);
    Cyclesim.cycle sim
  done;
  Waveform.expect ~wave_width:0 ~display_width:86 ~display_height:17 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─  │
    │increment         ││ 0│1│2│3│0│1│2│3│0│1│2│3│0│1│2│3│0│1│2│3│0│1│2│3│0│1│2│3│0│1│2  │
    │                  ││──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─  │
    │set               ││                                                                │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──────────────────────────────────────────────────────────────  │
    │value             ││ 0                                                              │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──────┬─┬─┬───┬─┬─┬───┬─┬─┬───┬─┬─┬───┬─┬─┬───┬─┬─┬───┬─┬─┬───  │
    │q                 ││ 0    │1│3│6  │7│9│C  │D│F│2  │3│5│8  │9│B│E  │F│1│4  │5│7│A    │
    │                  ││──────┴─┴─┴───┴─┴─┴───┴─┴─┴───┴─┴─┴───┴─┴─┴───┴─┴─┴───┴─┴─┴───  │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    8dd135be26b5228a43a1d845da92d1aa
    |}]
;;

let%expect_test "set value" =
  let sim, waves, increment, set, value, _q =
    sim ~part_width:2 ~increment_width:2 ~total_width:4
  in
  increment := Bits.of_int ~width:2 1;
  for _ = 0 to 8 do
    Cyclesim.cycle sim
  done;
  set := Bits.vdd;
  value := Bits.of_int ~width:4 0xf;
  Cyclesim.cycle sim;
  set := Bits.gnd;
  for _ = 0 to 8 do
    Cyclesim.cycle sim
  done;
  set := Bits.vdd;
  value := Bits.of_int ~width:4 0x3;
  Cyclesim.cycle sim;
  set := Bits.gnd;
  for _ = 0 to 8 do
    Cyclesim.cycle sim
  done;
  Waveform.expect ~wave_width:0 ~display_width:86 ~display_height:17 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────────      │
    │                  ││──────────────────────────────────────────────────────────      │
    │increment         ││ 1                                                              │
    │                  ││──────────────────────────────────────────────────────────      │
    │set               ││                  ┌─┐                 ┌─┐                       │
    │                  ││──────────────────┘ └─────────────────┘ └─────────────────      │
    │                  ││──────────────────┬───────────────────┬───────────────────      │
    │value             ││ 0                │F                  │3                        │
    │                  ││──────────────────┴───────────────────┴───────────────────      │
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─      │
    │q                 ││ 0  │1│2│3│4│5│6│7│8│9│F│0│1│2│3│4│5│6│7│8│3│4│5│6│7│8│9│A      │
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─      │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    59d3f03ba9c7395d48d5f797feff508f
    |}]
;;

let%expect_test "weird sizes" =
  let sim, waves, increment, _set, _value, _q =
    sim ~part_width:3 ~increment_width:2 ~total_width:4
  in
  increment := Bits.of_int ~width:2 1;
  for _ = 0 to 30 do
    Cyclesim.cycle sim
  done;
  Waveform.expect ~wave_width:0 ~display_width:86 ~display_height:17 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──────────────────────────────────────────────────────────────  │
    │increment         ││ 1                                                              │
    │                  ││──────────────────────────────────────────────────────────────  │
    │set               ││                                                                │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││──────────────────────────────────────────────────────────────  │
    │value             ││ 0                                                              │
    │                  ││──────────────────────────────────────────────────────────────  │
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─  │
    │q                 ││ 0  │1│2│3│4│5│6│7│8│9│A│B│C│D│E│F│0│1│2│3│4│5│6│7│8│9│A│B│C│D  │
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─  │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    ed498a993d74f63af1b39fea2bddf255
    |}]
;;

let%expect_test "64 bit" =
  let sim, _waves, increment, set, value, q =
    sim ~part_width:32 ~increment_width:1 ~total_width:64
  in
  (* roll over at 32 bits *)
  increment := Bits.vdd;
  set := Bits.vdd;
  value := Bits.of_int ~width:64 0xFFFF_FFFF;
  Cyclesim.cycle sim;
  set := Bits.gnd;
  Cyclesim.cycle sim;
  print_s [%message (Bits.to_int64 !q : Int64.Hex.t)];
  [%expect {| ("Bits.to_int64 (!q)" 0xffffffff) |}];
  Cyclesim.cycle sim;
  print_s [%message (Bits.to_int64 !q : Int64.Hex.t)];
  [%expect {| ("Bits.to_int64 (!q)" 0x100000000) |}];
  Cyclesim.cycle sim;
  print_s [%message (Bits.to_int64 !q : Int64.Hex.t)];
  [%expect {| ("Bits.to_int64 (!q)" 0x100000001) |}]
;;
