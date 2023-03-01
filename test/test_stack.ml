open Core
open Hardcaml
open Hardcaml_waveterm
open Bits

let width = 4
let default_capacity = 8

module Int4 = Hardcaml.Scalar.Make (struct
    let port_name = "int4"
    let port_width = width
  end)

module Stack = Hardcaml_circuits.Stack.Make (Int4)
open Stack
module Sim = Cyclesim.With_interface (I) (O)

let display_rules =
  Display_rule.(
    [ I.map I.port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)) |> I.to_list
    ; O.map O.port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)) |> O.to_list
    ; [ port_name_matches
          Re.Posix.(re "used" |> compile)
          ~wave_format:(Bit_or Unsigned_int)
      ; port_name_matches
          Re.Posix.(re "cut_through" |> compile)
          ~wave_format:(Bit_or Unsigned_int)
      ]
    ]
    |> List.concat)
;;

type event =
  | Push of int
  | Pop
  | Push_pop of int
[@@deriving sexp_of]

let create ~read_latency ~capacity () =
  let circuit =
    hierarchical
      ~read_latency
      ~capacity
      (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())
  in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all circuit in
  Waveform.create sim
;;

(* runs through a series of events and validates the behavior against a software stack. *)
let run_test ?(read_latency = 1) ?(capacity = default_capacity) ?(verbose = false) events =
  let waves, sim = create ~read_latency ~capacity () in
  let i, o = Cyclesim.inputs sim, Cyclesim.outputs sim in
  (* clear *)
  i.clear := vdd;
  Cyclesim.cycle sim;
  i.clear := gnd;
  (* run through events *)
  let pop_pipeline = Array.init read_latency ~f:(fun _ -> false) in
  let shift_pops () =
    for i = read_latency - 1 downto 1 do
      pop_pipeline.(i) <- pop_pipeline.(i - 1)
    done;
    pop_pipeline.(0) <- false
  in
  let model = Core.Stack.create () in
  let handle_pending_pop () =
    (match pop_pipeline.(read_latency - 1) with
     | false -> ()
     | true ->
       let valid = to_bool !(o.q.valid) in
       let value = to_int !(o.q.value) in
       (match Core.Stack.pop model with
        | None -> [%test_result: bool] valid ~expect:false
        | Some model_value ->
          [%test_result: bool] valid ~expect:true;
          [%test_result: int] value ~expect:model_value);
       if verbose then print_s [%message "popped" (valid : bool) (value : int)]);
    shift_pops ()
  in
  List.iter events ~f:(fun event ->
    (* handle pending pop *)
    handle_pending_pop ();
    (* reset inputs *)
    i.push := gnd;
    i.pop := gnd;
    (* set new inputs *)
    let push ?(cut_through = false) v =
      i.wr_data := Bits.of_int ~width v;
      i.push := vdd;
      if cut_through || Core.Stack.length model < capacity then Core.Stack.push model v
    in
    let pop () =
      i.pop := vdd;
      pop_pipeline.(0) <- true
    in
    (match event with
     | Push v -> push v
     | Pop -> pop ()
     | Push_pop v ->
       push ~cut_through:true v;
       pop ());
    (* cycle *)
    Cyclesim.cycle sim);
  handle_pending_pop ();
  Cyclesim.cycle sim;
  waves
;;

let%expect_test "basic stack test - push a couple, push and pop, pop and underflow" =
  let (events : event list) = [ Push 1; Push 2; Push_pop 3; Pop; Pop; Pop; Pop ] in
  let waves = run_test ~verbose:true events in
  Waveform.print ~display_width:52 ~display_height:27 ~wave_width:1 ~display_rules waves;
  [%expect
    {|
    (popped (valid true) (value 3))
    (popped (valid true) (value 2))
    (popped (valid true) (value 1))
    (popped (valid false) (value 0))
    (popped (valid false) (value 0))
    ┌Signals────┐┌Waves────────────────────────────────┐
    │clock      ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌│
    │           ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘│
    │clear      ││────┐                                │
    │           ││    └─────────────────────────────── │
    │           ││────┬───┬───┬─────────────────────── │
    │int4       ││ 0  │1  │2  │3                       │
    │           ││────┴───┴───┴─────────────────────── │
    │push       ││    ┌───────────┐                    │
    │           ││────┘           └─────────────────── │
    │pop        ││            ┌─────────────────────── │
    │           ││────────────┘                        │
    │q$valid    ││                ┌───────────┐        │
    │           ││────────────────┘           └─────── │
    │           ││────────────┬───┬───┬───┬───┬─────── │
    │q$value$int││ 0          │1  │3  │2  │1  │0       │
    │           ││────────────┴───┴───┴───┴───┴─────── │
    │full       ││                                     │
    │           ││──────────────────────────────────── │
    │empty      ││    ┌───┐               ┌─────────── │
    │           ││────┘   └───────────────┘            │
    │           ││────────┬───┬───────┬───┬─────────── │
    │stack$used ││ 0      │1  │2      │1  │0           │
    │           ││────────┴───┴───────┴───┴─────────── │
    │stack$cut_t││            ┌───┐                    │
    │           ││────────────┘   └─────────────────── │
    └───────────┘└─────────────────────────────────────┘ |}]
;;

let%expect_test "basic stack test - fill up, overflow, push and pop, pop everything" =
  let (events : event list) =
    List.init default_capacity ~f:(fun i -> Push i)
    @ [ Push 10; Push_pop 11 ]
    @ List.init default_capacity ~f:(Fn.const Pop)
  in
  let waves = run_test events in
  Waveform.print ~display_width:102 ~display_height:27 ~wave_width:1 ~display_rules waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││────┐                                                                           │
    │                  ││    └───────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬───┬───┬───┬───┬───┬───┬───┬───┬───────────────────────────────────────│
    │int4              ││ 0      │1  │2  │3  │4  │5  │6  │7  │10 │11                                     │
    │                  ││────────┴───┴───┴───┴───┴───┴───┴───┴───┴───────────────────────────────────────│
    │push              ││    ┌───────────────────────────────────────┐                                   │
    │                  ││────┘                                       └───────────────────────────────────│
    │pop               ││                                        ┌───────────────────────────────────────│
    │                  ││────────────────────────────────────────┘                                       │
    │q$valid           ││                                            ┌───────────────────────────────────│
    │                  ││────────────────────────────────────────────┘                                   │
    │                  ││────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
    │q$value$int4      ││ 0              │1  │2  │3  │4  │5  │6  │7  │11 │7  │6  │5  │4  │3  │2  │1  │0  │
    │                  ││────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
    │full              ││                                    ┌───────────┐                               │
    │                  ││────────────────────────────────────┘           └───────────────────────────────│
    │empty             ││    ┌───┐                                                                   ┌───│
    │                  ││────┘   └───────────────────────────────────────────────────────────────────┘   │
    │                  ││────────┬───┬───┬───┬───┬───┬───┬───┬───────────┬───┬───┬───┬───┬───┬───┬───┬───│
    │stack$used        ││ 0      │1  │2  │3  │4  │5  │6  │7  │8          │7  │6  │5  │4  │3  │2  │1  │0  │
    │                  ││────────┴───┴───┴───┴───┴───┴───┴───┴───────────┴───┴───┴───┴───┴───┴───┴───┴───│
    │stack$cut_through ││                                        ┌───┐                                   │
    │                  ││────────────────────────────────────────┘   └───────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "stack test with read latency 2: fill, overflow, push and pop, pop all" =
  let (events : event list) =
    List.init default_capacity ~f:(fun i -> Push i)
    @ [ Push 10; Push_pop 11 ]
    @ List.init default_capacity ~f:(Fn.const Pop)
  in
  let waves = run_test ~read_latency:2 events in
  Waveform.print ~display_width:102 ~display_height:27 ~wave_width:1 ~display_rules waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││────┐                                                                           │
    │                  ││    └───────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬───┬───┬───┬───┬───┬───┬───┬───┬───────────────────────────────────────│
    │int4              ││ 0      │1  │2  │3  │4  │5  │6  │7  │10 │11                                     │
    │                  ││────────┴───┴───┴───┴───┴───┴───┴───┴───┴───────────────────────────────────────│
    │push              ││    ┌───────────────────────────────────────┐                                   │
    │                  ││────┘                                       └───────────────────────────────────│
    │pop               ││                                        ┌───────────────────────────────────────│
    │                  ││────────────────────────────────────────┘                                       │
    │q$valid           ││                                                ┌───────────────────────────────│
    │                  ││────────────────────────────────────────────────┘                               │
    │                  ││────────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
    │q$value$int4      ││ 0                  │1  │2  │3  │4  │5  │6  │7  │11 │7  │6  │5  │4  │3  │2  │1  │
    │                  ││────────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
    │full              ││                                    ┌───────────┐                               │
    │                  ││────────────────────────────────────┘           └───────────────────────────────│
    │empty             ││    ┌───┐                                                                   ┌───│
    │                  ││────┘   └───────────────────────────────────────────────────────────────────┘   │
    │                  ││────────┬───┬───┬───┬───┬───┬───┬───┬───────────┬───┬───┬───┬───┬───┬───┬───┬───│
    │stack$used        ││ 0      │1  │2  │3  │4  │5  │6  │7  │8          │7  │6  │5  │4  │3  │2  │1  │0  │
    │                  ││────────┴───┴───┴───┴───┴───┴───┴───┴───────────┴───┴───┴───┴───┴───┴───┴───┴───│
    │stack$cut_through ││                                        ┌───┐                                   │
    │                  ││────────────────────────────────────────┘   └───────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "stack test with read latency 3: fill, overflow, push and pop, pop all" =
  let (events : event list) =
    List.init default_capacity ~f:(fun i -> Push i)
    @ [ Push 10; Push_pop 11 ]
    @ List.init default_capacity ~f:(Fn.const Pop)
  in
  let waves = run_test ~read_latency:3 events in
  Waveform.print ~display_width:102 ~display_height:27 ~wave_width:1 ~display_rules waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││────┐                                                                           │
    │                  ││    └───────────────────────────────────────────────────────────────────────────│
    │                  ││────────┬───┬───┬───┬───┬───┬───┬───┬───┬───────────────────────────────────────│
    │int4              ││ 0      │1  │2  │3  │4  │5  │6  │7  │10 │11                                     │
    │                  ││────────┴───┴───┴───┴───┴───┴───┴───┴───┴───────────────────────────────────────│
    │push              ││    ┌───────────────────────────────────────┐                                   │
    │                  ││────┘                                       └───────────────────────────────────│
    │pop               ││                                        ┌───────────────────────────────────────│
    │                  ││────────────────────────────────────────┘                                       │
    │q$valid           ││                                                    ┌───────────────────────────│
    │                  ││────────────────────────────────────────────────────┘                           │
    │                  ││────────────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
    │q$value$int4      ││ 0                      │1  │2  │3  │4  │5  │6  │7  │11 │7  │6  │5  │4  │3  │2  │
    │                  ││────────────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
    │full              ││                                    ┌───────────┐                               │
    │                  ││────────────────────────────────────┘           └───────────────────────────────│
    │empty             ││    ┌───┐                                                                   ┌───│
    │                  ││────┘   └───────────────────────────────────────────────────────────────────┘   │
    │                  ││────────┬───┬───┬───┬───┬───┬───┬───┬───────────┬───┬───┬───┬───┬───┬───┬───┬───│
    │stack$used        ││ 0      │1  │2  │3  │4  │5  │6  │7  │8          │7  │6  │5  │4  │3  │2  │1  │0  │
    │                  ││────────┴───┴───┴───┴───┴───┴───┴───┴───────────┴───┴───┴───┴───┴───┴───┴───┴───│
    │stack$cut_through ││                                        ┌───┐                                   │
    │                  ││────────────────────────────────────────┘   └───────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "non-pow2 stack test - fill up, overflow, push_pop, pop all, underflow" =
  let capacity = 5 in
  let (events : event list) =
    List.init capacity ~f:(fun i -> Push i)
    @ [ Push 10; Push_pop 11 ]
    @ List.init (capacity + 1) ~f:(Fn.const Pop)
  in
  let _waves = run_test ~capacity ~verbose:true events in
  [%expect
    {|
    (popped (valid true) (value 11))
    (popped (valid true) (value 4))
    (popped (valid true) (value 3))
    (popped (valid true) (value 2))
    (popped (valid true) (value 1))
    (popped (valid true) (value 0))
    (popped (valid false) (value 0)) |}]
;;
