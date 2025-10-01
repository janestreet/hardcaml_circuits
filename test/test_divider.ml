open! Core
open! Hardcaml
open Hardcaml_waveterm
open Expect_test_helpers_base
open Quickcheck
include Hardcaml_circuits.Divider

let spec width signedness architecture =
  (module struct
    let width = width
    let signedness = signedness
    let architecture = architecture
  end : Spec)
;;

module Make_test (Spec : Spec) = struct
  module Div = Make (Spec)
  open Div
  module Sim = Cyclesim.With_interface (I) (O)

  let create ?(all_waves = false) () =
    let div =
      Div.create
        (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())
    in
    let sim =
      let config =
        if all_waves
        then { Cyclesim.Config.trace_all with store_circuit = true }
        else { Cyclesim.Config.default with store_circuit = true }
      in
      Sim.create ~config div
    in
    Waveform.create sim
  ;;

  module I_stim = struct
    type t =
      { n : int
      ; d : int
      ; start : bool
      }
    [@@deriving sexp_of]
  end

  let check_outputs (inputs : I_stim.t) (outputs : _ O.t) =
    let msg_inputs =
      [%string
        "numerator=%{inputs.n#Int} denominator=%{inputs.d#Int} start=%{inputs.start#Bool}"]
    in
    let bits_to_int =
      match Spec.signedness with
      | Signed -> Bits.to_signed_int
      | Unsigned -> Bits.to_int_trunc
    in
    require_equal
      ~message:("(Failed Valid) " ^ msg_inputs)
      (module Bits)
      !(outputs.valid)
      (Bits.of_bool inputs.start);
    if inputs.start
    then (
      require_equal
        ~message:("(Failed Quotient) " ^ msg_inputs)
        (module Int)
        (bits_to_int !(outputs.quotient))
        (inputs.n / inputs.d);
      require_equal
        ~message:("(Failed Remainder) " ^ msg_inputs)
        (module Int)
        (bits_to_int !(outputs.remainder))
        (inputs.n mod inputs.d))
    else ()
  ;;

  let get_input_range =
    match Spec.signedness with
    | Signed -> -Int.(2 ** (Spec.width - 1)), Int.((2 ** (Spec.width - 1)) - 1)
    | Unsigned -> 0, Int.((2 ** Spec.width) - 1)
  ;;

  let ( <--. ) = Bits.( <--. )

  let apply_input_stim (i_stim : I_stim.t) (inputs : _ I.t) =
    let min_input, max_input = get_input_range in
    assert (
      (not i_stim.start)
      || (i_stim.d <> 0 && i_stim.d >= min_input && i_stim.d <= max_input));
    assert ((not i_stim.start) || (i_stim.n >= min_input && i_stim.n <= max_input));
    inputs.numerator <--. i_stim.n;
    inputs.denominator <--. i_stim.d;
    inputs.start := if i_stim.start then Bits.vdd else Bits.gnd
  ;;

  let quickcheck_test_setup =
    lazy
      (let waves, sim = create () in
       let inputs = Cyclesim.inputs sim in
       let outputs = Cyclesim.outputs sim in
       waves, sim, inputs, outputs)
  ;;

  let quickcheck_gen pstart =
    let open Generator in
    let min_input, max_input = get_input_range in
    Int.gen_uniform_incl min_input max_input
    >>= fun n ->
    Int.gen_uniform_incl min_input max_input
    >>= fun d ->
    Float.gen_uniform_excl 0.0 1.0
    >>| fun s ->
    let start = Float.(s < pstart) in
    match d with
    | 0 -> { I_stim.n; d = 1; start } (* Adjust d if = 0 *)
    | _ -> { I_stim.n; d; start }
  ;;

  let quickcheck_div_iterative sim inputs outputs i_stim =
    apply_input_stim i_stim inputs;
    for _ = 0 to Spec.width + 1 do
      Cyclesim.cycle sim;
      apply_input_stim { n = 0; d = 0; start = false } inputs
    done;
    check_outputs i_stim outputs
  ;;

  let quickcheck_div_test () =
    let _, sim, inputs, outputs = Lazy.force quickcheck_test_setup in
    Quickcheck.test
      ~trials:50000
      (quickcheck_gen 1.0)
      ~sexp_of:[%sexp_of: I_stim.t]
      ~f:(fun i_stim -> quickcheck_div_iterative sim inputs outputs i_stim)
  ;;

  let random_test_unrolled ~pipe_depth ~n_tests ~pvalid () =
    let (_ : Waveform.t), sim, inputs, outputs = Lazy.force quickcheck_test_setup in
    let gen_sequence = quickcheck_gen pvalid in
    let random = Splittable_random.of_int 0 in
    let input_queue = Queue.create ~capacity:pipe_depth () in
    for i = 0 to n_tests do
      let i_stim = Generator.generate gen_sequence ~size:n_tests ~random in
      Queue.enqueue_front input_queue i_stim;
      apply_input_stim i_stim inputs;
      Cyclesim.cycle sim;
      if i >= pipe_depth
      then (
        let check_i =
          if pipe_depth > 0 then Queue.dequeue_back input_queue else Some i_stim
        in
        match check_i with
        | None -> raise_s [%message "dequeue error"]
        | Some check_i -> check_outputs check_i outputs)
    done
  ;;

  let start_division ~numerator ~denominator =
    let waves, sim = create () in
    let inputs = Cyclesim.inputs sim in
    inputs.numerator <--. numerator;
    inputs.denominator <--. denominator;
    inputs.start := Bits.vdd;
    sim, waves, inputs
  ;;
end

let print waves = Waveform.print ~wave_width:1 ~display_width:80 waves

let%expect_test "Wavetest, 7 divided by 2, width 8, unsigned, combinational" =
  let open Make_test ((val spec 8 Unsigned Combinational)) in
  let sim, waves, inputs = start_division ~numerator:7 ~denominator:2 in
  for _ = 0 to Div.I.port_widths.numerator + 2 do
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd
  done;
  print waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││────────────────────────────────────────────              │
    │denominator       ││ 02                                                       │
    │                  ││────────────────────────────────────────────              │
    │                  ││────────────────────────────────────────────              │
    │numerator         ││ 07                                                       │
    │                  ││────────────────────────────────────────────              │
    │start             ││────┐                                                     │
    │                  ││    └───────────────────────────────────────              │
    │                  ││────┬───────────────────────────────────────              │
    │quotient          ││ 03 │01                                                   │
    │                  ││────┴───────────────────────────────────────              │
    │                  ││────────────────────────────────────────────              │
    │remainder         ││ 01                                                       │
    │                  ││────────────────────────────────────────────              │
    │valid             ││────┐                                                     │
    │                  ││    └───────────────────────────────────────              │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Wavetest, -9 divided by 4, width 8, signed, combinational" =
  let open Make_test ((val spec 8 Signed Combinational)) in
  let sim, waves, inputs = start_division ~numerator:(-9) ~denominator:4 in
  for _ = 0 to Div.I.port_widths.numerator + 2 do
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd
  done;
  print waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││────────────────────────────────────────────              │
    │denominator       ││ 04                                                       │
    │                  ││────────────────────────────────────────────              │
    │                  ││────────────────────────────────────────────              │
    │numerator         ││ F7                                                       │
    │                  ││────────────────────────────────────────────              │
    │start             ││────┐                                                     │
    │                  ││    └───────────────────────────────────────              │
    │                  ││────┬───────────────────────────────────────              │
    │quotient          ││ 1FE│000                                                  │
    │                  ││────┴───────────────────────────────────────              │
    │                  ││────────────────────────────────────────────              │
    │remainder         ││ 1FF                                                      │
    │                  ││────────────────────────────────────────────              │
    │valid             ││────┐                                                     │
    │                  ││    └───────────────────────────────────────              │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Wavetest, 7 divided by 2, width 8, unsigned, iterative" =
  let open Make_test ((val spec 8 Unsigned Iterative)) in
  let sim, waves, inputs = start_division ~numerator:7 ~denominator:2 in
  for _ = 0 to Div.I.port_widths.numerator + 2 do
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd
  done;
  print waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││                                                          │
    │                  ││────────────────────────────────────────────              │
    │                  ││────────────────────────────────────────────              │
    │denominator       ││ 02                                                       │
    │                  ││────────────────────────────────────────────              │
    │                  ││────────────────────────────────────────────              │
    │numerator         ││ 07                                                       │
    │                  ││────────────────────────────────────────────              │
    │start             ││────┐                                                     │
    │                  ││    └───────────────────────────────────────              │
    │                  ││────────┬───────────────────────┬───┬───────              │
    │quotient          ││ 01     │00                     │01 │03                   │
    │                  ││────────┴───────────────────────┴───┴───────              │
    │                  ││────────────────────────────┬───────────────              │
    │remainder         ││ 00                         │01                           │
    │                  ││────────────────────────────┴───────────────              │
    │valid             ││                                    ┌───────              │
    │                  ││────────────────────────────────────┘                     │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Wavetest, -9 divided by 4, width 8, signed, iterative" =
  let open Make_test ((val spec 8 Signed Iterative)) in
  let sim, waves, inputs = start_division ~numerator:(-9) ~denominator:4 in
  for _ = 0 to Div.I.port_widths.numerator + 4 do
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    inputs.numerator <--. 0;
    inputs.denominator <--. 0
  done;
  print waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││                                                          │
    │                  ││────────────────────────────────────────────────────      │
    │                  ││────┬───────────────────────────────────────────────      │
    │denominator       ││ 04 │00                                                   │
    │                  ││────┴───────────────────────────────────────────────      │
    │                  ││────┬───────────────────────────────────────────────      │
    │numerator         ││ F7 │00                                                   │
    │                  ││────┴───────────────────────────────────────────────      │
    │start             ││────┐                                                     │
    │                  ││    └───────────────────────────────────────────────      │
    │                  ││────┬───┬───────────────────────────┬───┬───────────      │
    │quotient          ││ 001│1FF│000                        │1FF│1FE              │
    │                  ││────┴───┴───────────────────────────┴───┴───────────      │
    │                  ││────────────────────────────┬───┬───┬───┬───────────      │
    │remainder         ││ 000                        │1FF│1FE│000│1FF              │
    │                  ││────────────────────────────┴───┴───┴───┴───────────      │
    │valid             ││                                        ┌───────────      │
    │                  ││────────────────────────────────────────┘                 │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Wavetest, 81 divided in loop, width 8, unsigned, pipelined" =
  let open Make_test ((val spec 8 Unsigned Pipelined)) in
  let sim, waves, inputs = start_division ~numerator:81 ~denominator:0 in
  for i = 1 to 2 * Div.I.port_widths.numerator do
    inputs.denominator <--. i;
    Cyclesim.cycle sim;
    if i = 3 then inputs.start := Bits.gnd else inputs.start := Bits.vdd
  done;
  print waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││                                                          │
    │                  ││──────────────────────────────────────────────────────────│
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬─│
    │denominator       ││ 01 │02 │03 │04 │05 │06 │07 │08 │09 │0A │0B │0C │0D │0E │0│
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴─│
    │                  ││──────────────────────────────────────────────────────────│
    │numerator         ││ 51                                                       │
    │                  ││──────────────────────────────────────────────────────────│
    │start             ││────────────┐   ┌─────────────────────────────────────────│
    │                  ││            └───┘                                         │
    │                  ││────────────────────────────────┬───┬───┬───┬───┬───┬───┬─│
    │quotient          ││ 01                             │51 │28 │1B │00 │10 │0D │0│
    │                  ││────────────────────────────────┴───┴───┴───┴───┴───┴───┴─│
    │                  ││────────────────────────────────────┬───┬───┬───────┬───┬─│
    │remainder         ││ 00                                 │01 │00 │01     │03 │0│
    │                  ││────────────────────────────────────┴───┴───┴───────┴───┴─│
    │valid             ││                                ┌───────────┐   ┌─────────│
    │                  ││────────────────────────────────┘           └───┘         │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Wavetest, -81 divided in loop, width 8, signed, pipelined" =
  let open Make_test ((val spec 8 Signed Pipelined)) in
  let sim, waves, inputs = start_division ~numerator:(-81) ~denominator:0 in
  for i = 1 to 2 * Div.I.port_widths.numerator do
    inputs.denominator <--. i;
    Cyclesim.cycle sim;
    if i = 3 then inputs.start := Bits.gnd else inputs.start := Bits.vdd
  done;
  print waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││                                                          │
    │                  ││──────────────────────────────────────────────────────────│
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬─│
    │denominator       ││ 01 │02 │03 │04 │05 │06 │07 │08 │09 │0A │0B │0C │0D │0E │0│
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴─│
    │                  ││──────────────────────────────────────────────────────────│
    │numerator         ││ AF                                                       │
    │                  ││──────────────────────────────────────────────────────────│
    │start             ││────────────┐   ┌─────────────────────────────────────────│
    │                  ││            └───┘                                         │
    │                  ││────────────────────────────────────┬───┬───┬───┬───┬───┬─│
    │quotient          ││ 001                                │1AF│1D8│1E5│000│1F0│1│
    │                  ││────────────────────────────────────┴───┴───┴───┴───┴───┴─│
    │                  ││────────────────────────────────────────┬───┬───┬───────┬─│
    │remainder         ││ 000                                    │1FF│000│1FF    │1│
    │                  ││────────────────────────────────────────┴───┴───┴───────┴─│
    │valid             ││                                    ┌───────────┐   ┌─────│
    │                  ││────────────────────────────────────┘           └───┘     │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Wavetest, clear test, width 8, signed" =
  let open Make_test ((val spec 8 Signed Iterative)) in
  let waves, sim = create () in
  let inputs = Cyclesim.inputs sim in
  inputs.start := Bits.vdd;
  for _ = 0 to Div.I.port_widths.numerator - 2 do
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd
  done;
  inputs.clear := Bits.vdd;
  for _ = 0 to Div.I.port_widths.numerator - 2 do
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd
  done;
  print waves;
  (* Ensure valid does not go high after clear *)
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││                            ┌───┐                         │
    │                  ││────────────────────────────┘   └───────────────────────  │
    │                  ││────────────────────────────────────────────────────────  │
    │denominator       ││ 00                                                       │
    │                  ││────────────────────────────────────────────────────────  │
    │                  ││────────────────────────────────────────────────────────  │
    │numerator         ││ 00                                                       │
    │                  ││────────────────────────────────────────────────────────  │
    │start             ││────┐                                                     │
    │                  ││    └───────────────────────────────────────────────────  │
    │                  ││────────────┬───┬───┬───┬───┬───┬───────────────────────  │
    │quotient          ││ 001        │101│181│1C1│1E1│1F1│001                      │
    │                  ││────────────┴───┴───┴───┴───┴───┴───────────────────────  │
    │                  ││────────────────────────────────────────────────────────  │
    │remainder         ││ 000                                                      │
    │                  ││────────────────────────────────────────────────────────  │
    │valid             ││                                                          │
    │                  ││────────────────────────────────────────────────────────  │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Wavetest, -8 divided by -1, width 4, signed, iterative (test the two's \
                 complement representability problem)"
  =
  let open Make_test ((val spec 4 Signed Iterative)) in
  let sim, waves, inputs = start_division ~numerator:(-8) ~denominator:(-1) in
  for _ = 0 to Div.I.port_widths.numerator + 4 do
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    inputs.numerator <--. 0;
    inputs.denominator <--. 0
  done;
  print waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││                                                          │
    │                  ││────────────────────────────────────                      │
    │                  ││────┬───────────────────────────────                      │
    │denominator       ││ F  │0                                                    │
    │                  ││────┴───────────────────────────────                      │
    │                  ││────┬───────────────────────────────                      │
    │numerator         ││ 8  │0                                                    │
    │                  ││────┴───────────────────────────────                      │
    │start             ││────┐                                                     │
    │                  ││    └───────────────────────────────                      │
    │                  ││────────┬───┬───┬───────────────────                      │
    │quotient          ││ 01     │00 │01 │08                                       │
    │                  ││────────┴───┴───┴───────────────────                      │
    │                  ││────────────────────────────────────                      │
    │remainder         ││ 00                                                       │
    │                  ││────────────────────────────────────                      │
    │valid             ││                        ┌───────────                      │
    │                  ││────────────────────────┘                                 │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    |}]
;;

(* Constrained random stim *)

let test_widths = [ 4; 5; 6; 7; 8; 12; 16; 24; 32; 33; 48 ]

let%expect_test "Quickcheck, random unsigned" =
  List.iter test_widths ~f:(fun width ->
    let open Make_test ((val spec width Unsigned Iterative)) in
    quickcheck_div_test ())
;;

let%expect_test "Quickcheck, random signed" =
  List.iter test_widths ~f:(fun width ->
    let open Make_test ((val spec width Signed Iterative)) in
    quickcheck_div_test ())
;;

let n_tests = 1000 (* Number of tests specified manually for non-Quickcheck tests *)

let%expect_test "Random test, width 32, unsigned, pipelined" =
  let open Make_test ((val spec 32 Unsigned Pipelined)) in
  random_test_unrolled ~pipe_depth:31 ~n_tests ~pvalid:0.8 ()
;;

let%expect_test "Random test, width 32, signed, pipelined" =
  let open Make_test ((val spec 32 Signed Pipelined)) in
  random_test_unrolled ~pipe_depth:32 ~n_tests ~pvalid:0.8 ()
;;

let%expect_test "Random test, width 32, unsigned, combinational" =
  let open Make_test ((val spec 32 Unsigned Combinational)) in
  random_test_unrolled ~pipe_depth:0 ~n_tests ~pvalid:0.8 ()
;;

let%expect_test "Random test, width 32, signed, combinational" =
  let open Make_test ((val spec 32 Signed Combinational)) in
  random_test_unrolled ~pipe_depth:0 ~n_tests ~pvalid:0.8 ()
;;
