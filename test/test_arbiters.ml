open! Import
open Hardcaml_waveterm

let%expect_test "select next with clz" =
  let test valids =
    let next =
      List.init (Bits.width valids) ~f:(fun i ->
        Arbiters.select_next_with_clz
          (module Bits)
          ~index:(Offset (Bits.of_int_trunc ~width:(Int.ceil_log2 (Bits.width valids)) i))
          valids
        |> Bits.to_int_trunc)
    in
    print_s [%message (valids : Bits.t)];
    List.iteri next ~f:(fun index next ->
      Stdio.printf "  offset = %d, next = %d\n" index next)
  in
  (* try all 4 bit cases *)
  for i = 0 to 15 do
    test (Bits.of_int_trunc ~width:4 i);
    Stdio.printf "\n"
  done;
  [%expect
    {|
    (valids 0000)
      offset = 0, next = 0
      offset = 1, next = 0
      offset = 2, next = 0
      offset = 3, next = 0

    (valids 0001)
      offset = 0, next = 0
      offset = 1, next = 0
      offset = 2, next = 0
      offset = 3, next = 0

    (valids 0010)
      offset = 0, next = 1
      offset = 1, next = 1
      offset = 2, next = 1
      offset = 3, next = 1

    (valids 0011)
      offset = 0, next = 0
      offset = 1, next = 1
      offset = 2, next = 0
      offset = 3, next = 0

    (valids 0100)
      offset = 0, next = 2
      offset = 1, next = 2
      offset = 2, next = 2
      offset = 3, next = 2

    (valids 0101)
      offset = 0, next = 0
      offset = 1, next = 2
      offset = 2, next = 2
      offset = 3, next = 0

    (valids 0110)
      offset = 0, next = 1
      offset = 1, next = 1
      offset = 2, next = 2
      offset = 3, next = 1

    (valids 0111)
      offset = 0, next = 0
      offset = 1, next = 1
      offset = 2, next = 2
      offset = 3, next = 0

    (valids 1000)
      offset = 0, next = 3
      offset = 1, next = 3
      offset = 2, next = 3
      offset = 3, next = 3

    (valids 1001)
      offset = 0, next = 0
      offset = 1, next = 3
      offset = 2, next = 3
      offset = 3, next = 3

    (valids 1010)
      offset = 0, next = 1
      offset = 1, next = 1
      offset = 2, next = 3
      offset = 3, next = 3

    (valids 1011)
      offset = 0, next = 0
      offset = 1, next = 1
      offset = 2, next = 3
      offset = 3, next = 3

    (valids 1100)
      offset = 0, next = 2
      offset = 1, next = 2
      offset = 2, next = 2
      offset = 3, next = 3

    (valids 1101)
      offset = 0, next = 0
      offset = 1, next = 2
      offset = 2, next = 2
      offset = 3, next = 3

    (valids 1110)
      offset = 0, next = 1
      offset = 1, next = 1
      offset = 2, next = 2
      offset = 3, next = 3

    (valids 1111)
      offset = 0, next = 0
      offset = 1, next = 1
      offset = 2, next = 2
      offset = 3, next = 3
    |}];
  (* try some random examples *)
  (* A single bit input - not really possible as we cannot express the index value *)
  require_does_raise (fun () -> test (Bits.of_string "1"));
  [%expect
    {|
    ("Width of constant must be greater than zero"
     (width 0)
     (const 0))
    |}];
  test (Bits.of_string "01");
  [%expect
    {|
    (valids 01)
      offset = 0, next = 0
      offset = 1, next = 0
    |}];
  test (Bits.of_string "10110");
  [%expect
    {|
    (valids 10110)
      offset = 0, next = 1
      offset = 1, next = 1
      offset = 2, next = 2
      offset = 3, next = 4
      offset = 4, next = 4
    |}];
  test (Bits.of_string "1011001101000100110");
  [%expect
    {|
    (valids 1011001101000100110)
      offset = 0, next = 1
      offset = 1, next = 1
      offset = 2, next = 2
      offset = 3, next = 5
      offset = 4, next = 5
      offset = 5, next = 5
      offset = 6, next = 9
      offset = 7, next = 9
      offset = 8, next = 9
      offset = 9, next = 9
      offset = 10, next = 11
      offset = 11, next = 11
      offset = 12, next = 12
      offset = 13, next = 15
      offset = 14, next = 15
      offset = 15, next = 15
      offset = 16, next = 16
      offset = 17, next = 18
      offset = 18, next = 18
    |}]
;;

module Test_round_robin_comb (X : sig
    val round_robin
      :  index:Bits.t Arbiters.Index.t
      -> data:Bits.t With_valid.t list
      -> Bits.t With_valid.t
  end) =
struct
  let%expect_test "round robin with priority" =
    let open Bits in
    let test valids =
      let valids = bits_lsb valids in
      let with_valids =
        List.mapi valids ~f:(fun i valid ->
          { With_valid.valid; value = of_int_trunc ~width:8 i })
      in
      let active =
        List.init (List.length valids) ~f:(fun i ->
          X.round_robin
            ~index:
              (Arbiters.Index.Offset
                 (of_int_trunc ~width:(Int.ceil_log2 (List.length valids)) i))
            ~data:with_valids)
      in
      let sexp_of_result (t : Bits.t With_valid.t) =
        let r =
          if Bits.to_int_trunc t.valid <> 0
          then Some (Bits.to_int_trunc t.value)
          else None
        in
        [%sexp (r : int option)]
      in
      List.iteri active ~f:(fun offset active ->
        Stdio.printf !"  offset = %d, active = %{Sexp}\n" offset (sexp_of_result active))
    in
    [ "0000"; "0001"; "0010"; "0100"; "1000"; "1110"; "1010"; "1111"; "1010110" ]
    |> List.iter ~f:(fun valids ->
      print_s [%message (valids : string)];
      test (of_string valids);
      Stdio.printf "\n");
    [%expect
      {|
      (valids 0000)
        offset = 0, active = ()
        offset = 1, active = ()
        offset = 2, active = ()
        offset = 3, active = ()

      (valids 0001)
        offset = 0, active = (0)
        offset = 1, active = (0)
        offset = 2, active = (0)
        offset = 3, active = (0)

      (valids 0010)
        offset = 0, active = (1)
        offset = 1, active = (1)
        offset = 2, active = (1)
        offset = 3, active = (1)

      (valids 0100)
        offset = 0, active = (2)
        offset = 1, active = (2)
        offset = 2, active = (2)
        offset = 3, active = (2)

      (valids 1000)
        offset = 0, active = (3)
        offset = 1, active = (3)
        offset = 2, active = (3)
        offset = 3, active = (3)

      (valids 1110)
        offset = 0, active = (1)
        offset = 1, active = (1)
        offset = 2, active = (2)
        offset = 3, active = (3)

      (valids 1010)
        offset = 0, active = (1)
        offset = 1, active = (1)
        offset = 2, active = (3)
        offset = 3, active = (3)

      (valids 1111)
        offset = 0, active = (0)
        offset = 1, active = (1)
        offset = 2, active = (2)
        offset = 3, active = (3)

      (valids 1010110)
        offset = 0, active = (1)
        offset = 1, active = (1)
        offset = 2, active = (2)
        offset = 3, active = (4)
        offset = 4, active = (4)
        offset = 5, active = (6)
        offset = 6, active = (6)
      |}]
  ;;
end

module%test Test_log_shift_comb = Test_round_robin_comb (struct
    let round_robin =
      Arbiters.Round_robin_with_priority.Log_shift.combinational (module Bits)
    ;;
  end)

module%test Test_priority_count_zeros_comb = Test_round_robin_comb (struct
    let round_robin =
      Arbiters.Round_robin_with_priority.Count_zeros.combinational (module Bits)
    ;;
  end)

module%test Test_priority_onehot_cleaner_comb = Test_round_robin_comb (struct
    let round_robin =
      Arbiters.Round_robin_with_priority.Onehot_cleaner.combinational (module Bits)
    ;;
  end)

let display_rules =
  Display_rule.
    [ port_name_is "index" ~wave_format:Unsigned_int
    ; port_name_is "valid" ~wave_format:Bit
    ; port_name_is "value" ~wave_format:Unsigned_int
    ; port_name_matches (Posix "valid.+") ~wave_format:Bit
    ]
;;

type arbiter =
  clock:Signal.t
  -> clear:Signal.t
  -> index:Signal.t Hardcaml_circuits.Arbiters.Index.t
  -> data:(Signal.t, Signal.t) Comb.with_valid2 list
  -> Signal.t With_valid.t

let create_round_robin_seq ~num_sources ~data_width ~use_mask arbiter =
  let open Signal in
  let log_num_sources = Int.ceil_log2 num_sources in
  let index = input "index" log_num_sources in
  let clock = input "clock" 1 in
  let clear = input "clear" 1 in
  let data =
    List.init num_sources ~f:(fun i ->
      { With_valid.valid = input ("valid" ^ Int.to_string i) 1
      ; value = input ("value" ^ Int.to_string i) data_width
      })
  in
  let arb : _ With_valid.t =
    let index : _ Arbiters.Index.t =
      if use_mask
      then Mask (log_shift ~f:sll (ones num_sources) ~by:index)
      else Arbiters.Index.Offset index
    in
    arbiter ~clock ~clear ~index ~data
  in
  let circuit =
    Circuit.create_exn
      ~name:"arbiter"
      [ output "valid" arb.valid; output "value" arb.value ]
  in
  let sim =
    Cyclesim.create ~config:{ Cyclesim.Config.default with store_circuit = true } circuit
  in
  sim
;;

let test_round_robin_seq ~num_sources ~data_width sim =
  let log_num_sources = Int.ceil_log2 num_sources in
  let waves, sim = Waveform.create sim in
  let clear = Cyclesim.in_port sim "clear" in
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  let index =
    try Cyclesim.in_port sim "index" with
    | _ -> ref (Bits.zero log_num_sources)
  in
  let data =
    Array.init num_sources ~f:(fun i ->
      { With_valid.valid = Cyclesim.in_port sim ("valid" ^ Int.to_string i)
      ; value = Cyclesim.in_port sim ("value" ^ Int.to_string i)
      })
  in
  for i = 0 to num_sources - 1 do
    data.(i).value := Bits.of_int_trunc ~width:data_width i
  done;
  index := Bits.of_int_trunc ~width:log_num_sources 0;
  for i = num_sources - 1 downto 0 do
    data.(i).valid := Bits.vdd;
    Cyclesim.cycle sim
  done;
  for i = 0 to 3 do
    data.(i).valid := Bits.gnd
  done;
  data.(1).valid := Bits.vdd;
  data.(num_sources - 1).valid := Bits.vdd;
  for i = 0 to num_sources - 1 do
    index := Bits.of_int_trunc ~width:log_num_sources i;
    Cyclesim.cycle sim
  done;
  Cyclesim.cycle sim;
  waves
;;

let test ~use_mask arbiter =
  let data_width = 8 in
  let num_sources = 4 in
  let sim = create_round_robin_seq ~num_sources ~data_width ~use_mask arbiter in
  test_round_robin_seq ~num_sources ~data_width sim
;;

module Test_round_robin_seq (F : sig
    val round_robin
      :  clock:Signal.t
      -> clear:Signal.t
      -> index:Signal.t Arbiters.Index.t
      -> data:Signal.t With_valid.t list
      -> Signal.t With_valid.t
  end) =
struct
  let%expect_test "testbench" =
    test ~use_mask:false F.round_robin |> Waveform.expect ~display_rules ~wave_width:1;
    [%expect
      {|
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │               ││────────────────────────┬───┬───┬───────           │
      │index          ││ 0                      │1  │2  │3                 │
      │               ││────────────────────────┴───┴───┴───────           │
      │valid          ││        ┌───────────────────────────────           │
      │               ││────────┘                                          │
      │               ││────────┬───┬───┬───┬───┬───────┬───────           │
      │value          ││ 0      │3  │2  │1  │0  │1      │3                 │
      │               ││────────┴───┴───┴───┴───┴───────┴───────           │
      │valid0         ││                ┌───┐                              │
      │               ││────────────────┘   └───────────────────           │
      │valid1         ││            ┌───────────────────────────           │
      │               ││────────────┘                                      │
      │valid2         ││        ┌───────────┐                              │
      │               ││────────┘           └───────────────────           │
      │valid3         ││    ┌───────────────────────────────────           │
      │               ││────┘                                              │
      └───────────────┘└───────────────────────────────────────────────────┘
      2e2932e173d8209aeaaf01d084e347e9
      |}];
    test ~use_mask:true F.round_robin |> Waveform.expect ~display_rules ~wave_width:1;
    [%expect
      {|
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │               ││────────────────────────┬───┬───┬───────           │
      │index          ││ 0                      │1  │2  │3                 │
      │               ││────────────────────────┴───┴───┴───────           │
      │valid          ││        ┌───────────────────────────────           │
      │               ││────────┘                                          │
      │               ││────────┬───┬───┬───┬───┬───────┬───────           │
      │value          ││ 0      │3  │2  │1  │0  │1      │3                 │
      │               ││────────┴───┴───┴───┴───┴───────┴───────           │
      │valid0         ││                ┌───┐                              │
      │               ││────────────────┘   └───────────────────           │
      │valid1         ││            ┌───────────────────────────           │
      │               ││────────────┘                                      │
      │valid2         ││        ┌───────────┐                              │
      │               ││────────┘           └───────────────────           │
      │valid3         ││    ┌───────────────────────────────────           │
      │               ││────┘                                              │
      └───────────────┘└───────────────────────────────────────────────────┘
      2e2932e173d8209aeaaf01d084e347e9
      |}]
  ;;
end

module%test Test_log_shift_seq = Test_round_robin_seq (struct
    let round_robin = Arbiters.Round_robin_with_priority.Log_shift.sequential
  end)

module%test Test_priority_count_zeros_seq = Test_round_robin_seq (struct
    let round_robin = Arbiters.Round_robin_with_priority.Count_zeros.sequential
  end)

module%test Test_priority_onehot_cleaner_seq = Test_round_robin_seq (struct
    let round_robin = Arbiters.Round_robin_with_priority.Onehot_cleaner.sequential
  end)

(* Prove combinational architectures are the same *)
open Hardcaml_verify
open Comb_gates

let print_is_proved eqn =
  ignore (Solver.solve ~print_model:true (cnf ~:eqn) : _ Or_error.t)
;;

let%expect_test "prove the combinational architectures are equivalent" =
  let log_size = 2 in
  let data_bits = 2 in
  let build_proof index =
    let data =
      List.init (1 lsl log_size) ~f:(fun i ->
        With_valid.
          { valid = input ("valid" ^ Int.to_string i) 1
          ; value = input ("value" ^ Int.to_string i) data_bits
          })
    in
    let balanced =
      Arbiters.Round_robin_with_priority.combinational
        ~arch:Balanced
        (module Comb_gates)
        ~index
        ~data
    in
    let fast =
      Arbiters.Round_robin_with_priority.combinational
        ~arch:Fast
        (module Comb_gates)
        ~index
        ~data
    in
    let small =
      Arbiters.Round_robin_with_priority.combinational
        ~arch:Small
        (module Comb_gates)
        ~index
        ~data
    in
    let prove (a : _ With_valid.t) (b : _ With_valid.t) =
      print_s [%message "valid"];
      print_is_proved (a.valid ==: b.valid);
      print_s [%message "value"];
      print_is_proved (a.valid -->: (a.value ==: b.value))
    in
    prove, balanced, fast, small
  in
  let index = Arbiters.Index.Offset (input "index" log_size) in
  let prove, balanced, fast, small = build_proof index in
  prove balanced fast;
  [%expect
    {|
    valid
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    value
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}];
  prove balanced small;
  [%expect
    {|
    valid
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    value
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}];
  prove small fast;
  [%expect
    {|
    valid
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    value
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}];
  let index = input "index" log_size in
  let mask =
    Arbiters.Index.Mask
      (mux
         index
         (List.init (1 lsl log_size) ~f:(fun i ->
            of_int_trunc ~width:(1 lsl log_size) (-1 lsl i))))
  in
  let prove, balanced, fast, small = build_proof mask in
  prove balanced fast;
  [%expect
    {|
    valid
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    value
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}];
  prove balanced small;
  [%expect
    {|
    valid
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    value
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}];
  prove small fast;
  [%expect
    {|
    valid
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    value
       ____    __________
      / __ \  / ____/ __ \
     / / / / / __/ / / / /
    / /_/ / / /___/ /_/ /
    \___\_\/_____/_____/
    |}]
;;
