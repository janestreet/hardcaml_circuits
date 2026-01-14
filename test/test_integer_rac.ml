open! Import
open Hardcaml_waveterm

module Make (Config : Rac.Config) = struct
  module Rac = Rac.Make (Config)
  module Circuit = Circuit.With_interface (Rac.I) (Rac.O)
  module Step = Hardcaml_step_testbench.Effectful.Functional.Cyclesim.Make (Rac.I) (Rac.O)
  module Sim = Cyclesim.With_interface (Rac.I) (Rac.O)

  let testbench (h @ local) ~data_in _ =
    let is_integer_mode =
      match Config.mode with
      | Integer -> Bits.vdd
      | Fixed -> Bits.gnd
    in
    let i : Bits.t Rac.I.t = Rac.I.const Bits.empty in
    Step.delay h { i with clr = Bits.vdd };
    Step.delay
      h
      { i with
        en = Bits.vdd
      ; ld = Bits.vdd
      ; x = Array.map ~f:(Bits.of_int_trunc ~width:8) data_in
      };
    (* This cycle's [addsub] setting is because in [Integer] mode, the first bit that we
       process is the (negative) high bit in the twos-complement integer, so we want the
       accumulator to subtract rather than add. *)
    Step.delay h { i with addsub = is_integer_mode };
    Step.delay h ~num_cycles:(Config.data_bits - 2) i;
    (* This cycle's [addsub] setting is because in [Fixed] mode, the last bit that we
       process is the (negative) high bit in the twos-complement fixnum, so we accumulator
       to subtract rather than add. *)
    let o = Step.cycle h { i with addsub = Bits.( ~: ) is_integer_mode } in
    (* The result is available above if the testbench outputs are calculated [After] the
       clock edge, otherwise below. Either way we need to final step to display the data
       in the waveform. *)
    Step.delay h i;
    Bits.to_signed_int (Step.O_data.after_edge o).q
  ;;

  let run ~simulator ~testbench ~data_in =
    Step.run_until_finished
      ~simulator
      ~testbench:(fun h o -> testbench h ~data_in o)
      ~input_default:{ Step.input_zero with en = Bits.empty }
      ()
  ;;

  let create_sim ~coefs =
    let coefs = Array.map coefs ~f:(Bits.of_int_trunc ~width:8) in
    Sim.create
      ~config:{ Cyclesim.Config.default with store_circuit = true }
      (Rac.create ~coefs)
  ;;

  let run_and_print_waves ~simulator ~testbench ~data_in =
    let waves, simulator = Waveform.create simulator in
    let result = run ~simulator ~testbench ~data_in in
    Waveform.expect ~display_width:120 ~wave_width:2 waves;
    result
  ;;

  let test ?(print = false) () ~coefs ~data_in =
    let simulator = create_sim ~coefs in
    (if print then run_and_print_waves else run) ~simulator ~testbench ~data_in
  ;;
end

module Config = struct
  let mode = Rac.Mode.Integer
  let accumulator_bits = 20
  let data_bits = 8
  let num_coefs = 4
  let rom_shift = 0
end

include Make (Config)

let test ?print () ~coefs ~data_in =
  let eval ~coefs ~data_in =
    Array.map2_exn coefs data_in ~f:( * ) |> Array.reduce_exn ~f:( + )
  in
  print_s [%message "" ~expected:(eval ~coefs ~data_in : int)];
  let result = test ?print () ~coefs ~data_in in
  print_s [%message "" (result : int)]
;;

let%expect_test ("simulation example" [@tags "runtime5-only"]) =
  test ~print:true () ~coefs:[| 1; 2; 3; 4 |] ~data_in:[| 1; 2; 3; 4 |];
  [%expect
    {|
    (expected 30)
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clk               ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
    │clr               ││──────┐                                                                                           │
    │                  ││      └─────────────────────────────────────────────────────────────────                          │
    │addsub            ││            ┌─────┐                                                                               │
    │                  ││────────────┘     └─────────────────────────────────────────────────────                          │
    │en                ││      ┌─────────────────────────────────────────────────────────────────                          │
    │                  ││──────┘                                                                                           │
    │ld                ││      ┌─────┐                                                                                     │
    │                  ││──────┘     └───────────────────────────────────────────────────────────                          │
    │                  ││──────┬─────┬───────────────────────────────────────────────────────────                          │
    │x_0               ││ 00   │01   │00                                                                                   │
    │                  ││──────┴─────┴───────────────────────────────────────────────────────────                          │
    │                  ││──────┬─────┬───────────────────────────────────────────────────────────                          │
    │x_1               ││ 00   │02   │00                                                                                   │
    │                  ││──────┴─────┴───────────────────────────────────────────────────────────                          │
    │                  ││──────┬─────┬───────────────────────────────────────────────────────────                          │
    │x_2               ││ 00   │03   │00                                                                                   │
    │                  ││──────┴─────┴───────────────────────────────────────────────────────────                          │
    │                  ││──────┬─────┬───────────────────────────────────────────────────────────                          │
    │x_3               ││ 00   │04   │00                                                                                   │
    │                  ││──────┴─────┴───────────────────────────────────────────────────────────                          │
    │                  ││────────────────────────────────────────────────┬─────┬─────┬─────┬─────                          │
    │q                 ││ 00000                                          │00004│0000D│0001E│0003C                          │
    │                  ││────────────────────────────────────────────────┴─────┴─────┴─────┴─────                          │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
    d2ae5bacae9add968e82b39c76f6dd3e
    (result 30)
    |}]
;;

let%expect_test ("tests" [@tags "runtime5-only"]) =
  test () ~coefs:[| 3; 6; 1; 2 |] ~data_in:[| 7; 9; 3; 5 |];
  [%expect
    {|
    (expected 88)
    (result 88)
    |}];
  test () ~coefs:[| 33; 26; 61; 12 |] ~data_in:[| 17; 39; 43; 15 |];
  [%expect
    {|
    (expected 4378)
    (result 4378)
    |}];
  (* signed data *)
  test () ~coefs:[| 33; 26; 61; 12 |] ~data_in:[| 17; -39; 43; 15 |];
  [%expect
    {|
    (expected 2350)
    (result 2350)
    |}];
  (* signed coefficient *)
  test () ~coefs:[| 33; 26; -61; 12 |] ~data_in:[| 17; 39; 43; 15 |];
  [%expect
    {|
    (expected -868)
    (result -868)
    |}];
  (* signed data and coefficients *)
  test () ~coefs:[| -33; 26; -61; 12 |] ~data_in:[| 17; 39; -43; -15 |];
  [%expect
    {|
    (expected 2896)
    (result 2896)
    |}]
;;
