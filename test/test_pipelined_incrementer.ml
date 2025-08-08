open Import
open Hardcaml_waveterm

type ports =
  { increment : Bits.t ref
  ; set : Bits.t ref
  ; value : Bits.t ref
  ; q : Bits.t ref
  }

let sim ~part_width ~increment_width ~total_width =
  let clock = Signal.input "clock" 1 in
  let clear = Signal.input "clear" 1 in
  let set =
    { With_valid.valid = Signal.input "set" 1; value = Signal.input "value" total_width }
  in
  let increment = Signal.input "increment" increment_width in
  let q = Pipelined_incrementer.create ~part_width ~clock ~clear ~set ~increment in
  let circuit = Circuit.create_exn ~name:"pipelined_incr" [ Signal.output "q" q ] in
  let sim =
    Cyclesim.create ~config:{ Cyclesim.Config.default with store_circuit = true } circuit
  in
  let ports =
    { increment = Cyclesim.in_port sim "increment"
    ; set = Cyclesim.in_port sim "set"
    ; value = Cyclesim.in_port sim "value"
    ; q = Cyclesim.out_port sim "q"
    }
  in
  let waves, sim = Waveform.create sim in
  sim, waves, ports
;;

let incr_by_one ~sim { increment; _ } =
  increment := Bits.of_int_trunc ~width:2 1;
  for _ = 0 to 30 do
    Cyclesim.cycle sim
  done
;;

let%expect_test "increment by 1" =
  let sim, waves, ports = sim ~part_width:2 ~increment_width:2 ~total_width:4 in
  incr_by_one ~sim ports;
  Waveform.expect ~wave_width:0 ~display_width:86 waves;
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

let incr_by_three ~sim { increment; _ } =
  increment := Bits.of_int_trunc ~width:2 3;
  for _ = 0 to 30 do
    Cyclesim.cycle sim
  done
;;

let%expect_test "increment by 3" =
  let sim, waves, ports = sim ~part_width:2 ~increment_width:2 ~total_width:4 in
  incr_by_three ~sim ports;
  Waveform.expect ~wave_width:0 ~display_width:86 waves;
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

let incr_by_differing ~sim { increment; _ } =
  for i = 0 to 30 do
    increment := Bits.of_int_trunc ~width:2 (i % 4);
    Cyclesim.cycle sim
  done
;;

let%expect_test "increment by differing amounts" =
  let sim, waves, ports = sim ~part_width:2 ~increment_width:2 ~total_width:4 in
  incr_by_differing ~sim ports;
  Waveform.expect ~wave_width:0 ~display_width:86 waves;
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

let set_value ~sim { increment; set; value; _ } =
  increment := Bits.of_int_trunc ~width:2 1;
  for _ = 0 to 8 do
    Cyclesim.cycle sim
  done;
  set := Bits.vdd;
  value := Bits.of_int_trunc ~width:4 0xf;
  Cyclesim.cycle sim;
  set := Bits.gnd;
  for _ = 0 to 8 do
    Cyclesim.cycle sim
  done;
  set := Bits.vdd;
  value := Bits.of_int_trunc ~width:4 0x3;
  Cyclesim.cycle sim;
  set := Bits.gnd;
  for _ = 0 to 8 do
    Cyclesim.cycle sim
  done
;;

let%expect_test "set value" =
  let sim, waves, ports = sim ~part_width:2 ~increment_width:2 ~total_width:4 in
  set_value ~sim ports;
  Waveform.expect ~wave_width:0 ~display_width:86 waves;
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

let weird_sizes ~sim { increment; _ } =
  increment := Bits.of_int_trunc ~width:2 1;
  for _ = 0 to 30 do
    Cyclesim.cycle sim
  done
;;

let%expect_test "weird sizes" =
  let sim, waves, ports = sim ~part_width:3 ~increment_width:2 ~total_width:4 in
  weird_sizes ~sim ports;
  Waveform.expect ~wave_width:0 ~display_width:86 waves;
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

let sixty_four_bit ~sim { increment; set; value; q } =
  (* roll over at 32 bits *)
  increment := Bits.vdd;
  set := Bits.vdd;
  value := Bits.of_int_trunc ~width:64 0xFFFF_FFFF;
  Cyclesim.cycle sim;
  set := Bits.gnd;
  Cyclesim.cycle sim;
  print_s [%message (Bits.to_int64_trunc !q : Int64.Hex.t)];
  Cyclesim.cycle sim;
  print_s [%message (Bits.to_int64_trunc !q : Int64.Hex.t)];
  Cyclesim.cycle sim;
  print_s [%message (Bits.to_int64_trunc !q : Int64.Hex.t)]
;;

let%expect_test "64 bit" =
  let sim, _waves, ports = sim ~part_width:32 ~increment_width:1 ~total_width:64 in
  sixty_four_bit ~sim ports;
  [%expect
    {|
    ("Bits.to_int64_trunc (!q)" 0xffffffff)
    ("Bits.to_int64_trunc (!q)" 0x100000000)
    ("Bits.to_int64_trunc (!q)" 0x100000001)
    |}]
;;
