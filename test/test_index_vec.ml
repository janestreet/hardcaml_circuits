open! Base
open! Hardcaml
open! Hardcaml_waveterm
module Vec = Hardcaml_circuits.Vec
module Index_vec = Hardcaml_circuits.Index_vec

let create_sim log_vec_size =
  let module IVec =
    Index_vec.Make (struct
      let vec_size = 1 lsl log_vec_size
    end)
  in
  let clock = Signal.input "clock" 1 in
  let clear = Signal.input "clear" 1 in
  let op =
    { IVec.slot = Signal.input "slot" log_vec_size
    ; op = Signal.input "op" (Signal.width (Vec.noop (module Signal)))
    }
  in
  let vec = IVec.create (Signal.Reg_spec.create ~clock ~clear ()) op in
  let indexes = IVec.indexes vec in
  let circuit =
    Circuit.create_exn
      ~name:"index_vec"
      (List.concat
         [ Array.to_list indexes
           |> List.mapi ~f:(fun index -> Signal.output ("idx" ^ Int.to_string index))
         ; [ Signal.output "length" (IVec.length vec)
           ; Signal.output "full" (IVec.full vec)
           ; Signal.output "empty" (IVec.empty vec)
           ; Signal.output "insertion_index" (IVec.insertion_index vec)
           ; Signal.output "deletion_index" (IVec.deletion_index vec)
           ]
         ])
  in
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
  let clear = Cyclesim.in_port sim "clear" in
  let slot = Cyclesim.in_port sim "slot" in
  let op = Cyclesim.in_port sim "op" in
  (* Circuit must be reset to initialize the vec index registers *)
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  ( sim
  , (fun i -> slot := Bits.of_int_trunc ~width:log_vec_size i)
  , fun (f : (module Comb.S with type t = 'a) -> 'a) -> op := f (module Bits) )
;;

let%expect_test "fill and remove from start" =
  let sim, slot, op = create_sim 2 in
  let waves, sim = Waveform.create sim in
  for _ = 0 to 3 do
    slot 0;
    op Vec.insert;
    Cyclesim.cycle sim
  done;
  for _ = 0 to 3 do
    slot 0;
    op Vec.remove;
    Cyclesim.cycle sim
  done;
  op Vec.noop;
  Cyclesim.cycle sim;
  Waveform.print ~display_width:86 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │                  ││────────────────────────┬───────────────────────┬─────          │
    │op                ││ 2                      │3                      │0              │
    │                  ││────────────────────────┴───────────────────────┴─────          │
    │                  ││──────────────────────────────────────────────────────          │
    │slot              ││ 0                                                              │
    │                  ││──────────────────────────────────────────────────────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │deletion_index    ││ 0    │3    │2    │1    │0    │1    │2    │3    │0              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │empty             ││──────┐                                         ┌─────          │
    │                  ││      └─────────────────────────────────────────┘               │
    │full              ││                        ┌─────┐                                 │
    │                  ││────────────────────────┘     └───────────────────────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │idx0              ││ 0    │3    │2    │1    │0    │1    │2    │3    │0              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │idx1              ││ 1    │0    │3    │2    │1    │2    │3    │0    │1              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │idx2              ││ 2    │1    │0    │3    │2    │3    │0    │1    │2              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │idx3              ││ 3    │2    │1    │0    │3    │0    │1    │2    │3              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │insertion_index   ││ 3    │2    │1    │0    │3    │0    │1    │2    │3              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │length            ││ 0    │1    │2    │3    │4    │3    │2    │1    │0              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │gnd               ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "fill and remove from end" =
  let sim, slot, op = create_sim 2 in
  let waves, sim = Waveform.create sim in
  for i = 0 to 3 do
    slot i;
    op Vec.insert;
    Cyclesim.cycle sim
  done;
  for i = 3 downto 0 do
    slot i;
    op Vec.remove;
    Cyclesim.cycle sim
  done;
  op Vec.noop;
  Cyclesim.cycle sim;
  Waveform.print ~display_width:86 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │                  ││────────────────────────┬───────────────────────┬─────          │
    │op                ││ 2                      │3                      │0              │
    │                  ││────────────────────────┴───────────────────────┴─────          │
    │                  ││──────┬─────┬─────┬───────────┬─────┬─────┬───────────          │
    │slot              ││ 0    │1    │2    │3          │2    │1    │0                    │
    │                  ││──────┴─────┴─────┴───────────┴─────┴─────┴───────────          │
    │                  ││──────────────────────────────┬─────┬─────┬─────┬─────          │
    │deletion_index    ││ 0                            │1    │2    │3    │0              │
    │                  ││──────────────────────────────┴─────┴─────┴─────┴─────          │
    │empty             ││──────┐                                         ┌─────          │
    │                  ││      └─────────────────────────────────────────┘               │
    │full              ││                        ┌─────┐                                 │
    │                  ││────────────────────────┘     └───────────────────────          │
    │                  ││──────┬─────────────────────────────────────────┬─────          │
    │idx0              ││ 0    │3                                        │0              │
    │                  ││──────┴─────────────────────────────────────────┴─────          │
    │                  ││──────┬─────┬─────────────────────────────┬─────┬─────          │
    │idx1              ││ 1    │0    │2                            │0    │1              │
    │                  ││──────┴─────┴─────────────────────────────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────────────────┬─────┬─────┬─────          │
    │idx2              ││ 2    │1    │0    │1                │0    │1    │2              │
    │                  ││──────┴─────┴─────┴─────────────────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────────────────┬─────┬─────┬─────          │
    │idx3              ││ 3    │2    │1    │0                │1    │2    │3              │
    │                  ││──────┴─────┴─────┴─────────────────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────────────────┬─────┬─────┬─────          │
    │insertion_index   ││ 3    │2    │1    │0                │1    │2    │3              │
    │                  ││──────┴─────┴─────┴─────────────────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │length            ││ 0    │1    │2    │3    │4    │3    │2    │1    │0              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │gnd               ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "fill at end and remove from start" =
  let sim, slot, op = create_sim 2 in
  let waves, sim = Waveform.create sim in
  for i = 0 to 3 do
    slot i;
    op Vec.insert;
    Cyclesim.cycle sim
  done;
  for _ = 0 to 3 do
    slot 0;
    op Vec.remove;
    Cyclesim.cycle sim
  done;
  op Vec.noop;
  Cyclesim.cycle sim;
  Waveform.print ~display_width:86 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │                  ││────────────────────────┬───────────────────────┬─────          │
    │op                ││ 2                      │3                      │0              │
    │                  ││────────────────────────┴───────────────────────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────────────────────────────          │
    │slot              ││ 0    │1    │2    │3    │0                                      │
    │                  ││──────┴─────┴─────┴─────┴─────────────────────────────          │
    │                  ││────────────────────────┬─────┬─────┬─────┬─────┬─────          │
    │deletion_index    ││ 0                      │3    │2    │1    │0    │3              │
    │                  ││────────────────────────┴─────┴─────┴─────┴─────┴─────          │
    │empty             ││──────┐                                         ┌─────          │
    │                  ││      └─────────────────────────────────────────┘               │
    │full              ││                        ┌─────┐                                 │
    │                  ││────────────────────────┘     └───────────────────────          │
    │                  ││──────┬───────────────────────┬─────┬─────┬─────┬─────          │
    │idx0              ││ 0    │3                      │2    │1    │0    │3              │
    │                  ││──────┴───────────────────────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────────────────┬─────┬─────┬─────┬─────          │
    │idx1              ││ 1    │0    │2                │1    │0    │3    │2              │
    │                  ││──────┴─────┴─────────────────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬───────────┬─────┬─────┬─────┬─────          │
    │idx2              ││ 2    │1    │0    │1          │0    │3    │2    │1              │
    │                  ││──────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬───────────┬─────┬─────┬─────┬─────          │
    │idx3              ││ 3    │2    │1    │0          │3    │2    │1    │0              │
    │                  ││──────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬───────────┬─────┬─────┬─────┬─────          │
    │insertion_index   ││ 3    │2    │1    │0          │3    │2    │1    │0              │
    │                  ││──────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │length            ││ 0    │1    │2    │3    │4    │3    │2    │1    │0              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │gnd               ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "fill at start and remove from end" =
  let sim, slot, op = create_sim 2 in
  let waves, sim = Waveform.create sim in
  for _ = 0 to 3 do
    slot 0;
    op Vec.insert;
    Cyclesim.cycle sim
  done;
  for i = 3 downto 0 do
    slot i;
    op Vec.remove;
    Cyclesim.cycle sim
  done;
  op Vec.noop;
  Cyclesim.cycle sim;
  Waveform.print ~display_width:86 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │                  ││────────────────────────┬───────────────────────┬─────          │
    │op                ││ 2                      │3                      │0              │
    │                  ││────────────────────────┴───────────────────────┴─────          │
    │                  ││────────────────────────┬─────┬─────┬─────┬───────────          │
    │slot              ││ 0                      │3    │2    │1    │0                    │
    │                  ││────────────────────────┴─────┴─────┴─────┴───────────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │deletion_index    ││ 0    │3    │2    │1    │3    │2    │1    │0    │3              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │empty             ││──────┐                                         ┌─────          │
    │                  ││      └─────────────────────────────────────────┘               │
    │full              ││                        ┌─────┐                                 │
    │                  ││────────────────────────┘     └───────────────────────          │
    │                  ││──────┬─────┬─────┬─────┬───────────────────────┬─────          │
    │idx0              ││ 0    │3    │2    │1    │0                      │3              │
    │                  ││──────┴─────┴─────┴─────┴───────────────────────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────────────────┬─────┬─────          │
    │idx1              ││ 1    │0    │3    │2    │1                │3    │2              │
    │                  ││──────┴─────┴─────┴─────┴─────────────────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬───────────┬─────┬─────┬─────          │
    │idx2              ││ 2    │1    │0    │3    │2          │3    │2    │1              │
    │                  ││──────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬───────────┬─────┬─────┬─────          │
    │idx3              ││ 3    │2    │1    │0    │3          │2    │1    │0              │
    │                  ││──────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬───────────┬─────┬─────┬─────          │
    │insertion_index   ││ 3    │2    │1    │0    │3          │2    │1    │0              │
    │                  ││──────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────          │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────          │
    │length            ││ 0    │1    │2    │3    │4    │3    │2    │1    │0              │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────          │
    │gnd               ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let create_sim log_vec_size =
  let vec_size = 1 lsl log_vec_size in
  let sim, set_slot, set_op = create_sim log_vec_size in
  (* the index fields are combinational - look at their value after the index is set and
     before the register update. *)
  let insertion_index = Cyclesim.out_port ~clock_edge:Before sim "insertion_index" in
  let deletion_index = Cyclesim.out_port ~clock_edge:Before sim "deletion_index" in
  let slots =
    Array.init vec_size ~f:(fun i -> Cyclesim.out_port sim ("idx" ^ Int.to_string i))
  in
  let length = Cyclesim.out_port sim "length" in
  let table = Array.init (1 lsl log_vec_size) ~f:(Fn.const '_') in
  let validate_slots =
    (* Only every clock cycle perform a validation step on the indexes. Although they can
       switch position, we should never loose an index. In normal operation we expect
       this, but we also check that it is true when we perform operations that over/under
       flow the table, or are performed outside the current "length". *)
    let expect = Array.init vec_size ~f:Fn.id in
    fun () ->
      let slots = Array.map slots ~f:(fun b -> Bits.to_int_trunc !b) in
      Array.sort slots ~compare:Int.compare;
      [%test_result: int array] slots ~expect
  in
  let cycle () =
    Cyclesim.cycle sim;
    validate_slots ()
  in
  let insert ~slot data =
    set_slot slot;
    set_op Vec.insert;
    cycle ();
    table.(Bits.to_int_trunc !insertion_index) <- data
  in
  let remove ~slot =
    set_slot slot;
    set_op Vec.remove;
    cycle ();
    table.(Bits.to_int_trunc !deletion_index) <- '_'
  in
  let show () =
    let len = Bits.to_int_trunc !length in
    let s = String.init vec_size ~f:(fun i -> table.(i)) in
    let s' = String.init vec_size ~f:(fun i -> table.(Bits.to_int_trunc !(slots.(i)))) in
    let s'' = String.subo s' ~len in
    Stdio.printf "[%s] [%s] %s [%i]\n" s s' s'' len
  in
  insert, remove, show
;;

let%expect_test "connect to a table and insert and delete characters to form a string" =
  let insert, remove, show = create_sim 4 in
  (* Build 'hello world' *)
  let insertions =
    [ 0, 'l'
    ; 1, 'l'
    ; 0, 'h'
    ; 3, 'o'
    ; 1, 'e'
    ; 5, 'l'
    ; 6, 'd'
    ; 5, 'r'
    ; 4, 'w'
    ; 4, 'o'
    ; 5, ' '
    ]
  in
  List.iter insertions ~f:(fun (slot, data) ->
    insert ~slot data;
    show ());
  [%expect
    {|
    [_______________l] [l_______________] l [1]
    [______________ll] [ll______________] ll [2]
    [_____________hll] [hll_____________] hll [3]
    [____________ohll] [hllo____________] hllo [4]
    [___________eohll] [hello___________] hello [5]
    [__________leohll] [hellol__________] hellol [6]
    [_________dleohll] [hellold_________] hellold [7]
    [________rdleohll] [hellorld________] hellorld [8]
    [_______wrdleohll] [hellworld_______] hellworld [9]
    [______owrdleohll] [helloworld______] helloworld [10]
    [_____ owrdleohll] [hello world_____] hello world [11]
    |}];
  (* Randomly remove characters until empty *)
  for i = 0 to 10 do
    let slot = Random.int (11 - i) in
    remove ~slot;
    show ()
  done;
  [%expect
    {|
    [_____ owrdleo_ll] [ello world______] ello world [10]
    [______owrdleo_ll] [elloworld_______] elloworld [9]
    [______o_rdleo_ll] [elloorld________] elloorld [8]
    [______o_rd_eo_ll] [elloord_________] elloord [7]
    [______o__d_eo_ll] [ellood__________] ellood [6]
    [______o____eo_ll] [elloo___________] elloo [5]
    [______o_____o_ll] [lloo____________] lloo [4]
    [______o_____o__l] [loo_____________] loo [3]
    [______o________l] [lo______________] lo [2]
    [______o_________] [o_______________] o [1]
    [________________] [________________]  [0]
    |}];
  (* If we build it again, we end up with the characters in different places in the table,
     but the same result. This is because it doesn't really matter what order the 'free'
     indexes are, and they got shuffled by the previous operation. *)
  List.iter insertions ~f:(fun (slot, data) -> insert ~slot data);
  show ();
  [%expect {| [_____olwdlreh ol] [hello world_____] hello world [11] |}];
  (* Remove again until empty *)
  for i = 0 to 10 do
    let slot = Random.int (11 - i) in
    remove ~slot
  done;
  show ();
  [%expect {| [________________] [________________]  [0] |}]
;;

let%expect_test "insert into full table" =
  let insert, _remove, show = create_sim 4 in
  for i = 0 to 15 do
    insert ~slot:i (Char.of_int_exn (Char.to_int 'a' + i))
  done;
  show ();
  [%expect {| [ponmlkjihgfedcba] [abcdefghijklmnop] abcdefghijklmnop [16] |}];
  insert ~slot:0 'X';
  show ();
  [%expect {| [Xonmlkjihgfedcba] [Xabcdefghijklmno] Xabcdefghijklmno [16] |}];
  insert ~slot:8 'X';
  show ();
  [%expect {| [XXnmlkjihgfedcba] [XabcdefgXhijklmn] XabcdefgXhijklmn [16] |}];
  insert ~slot:15 'X';
  show ();
  [%expect {| [XXXmlkjihgfedcba] [XabcdefgXhijklmX] XabcdefgXhijklmX [16] |}]
;;

let%expect_test "remove from empty slots" =
  let insert, remove, show = create_sim 4 in
  (* we dont see anything happening here, but we have validated that the indexes in the
     table remain 'normal'. *)
  remove ~slot:0;
  show ();
  [%expect {| [________________] [________________]  [0] |}];
  remove ~slot:8;
  show ();
  [%expect {| [________________] [________________]  [0] |}];
  remove ~slot:15;
  show ();
  [%expect {| [________________] [________________]  [0] |}];
  (* insert a few things and remove empty slotes *)
  for i = 0 to 3 do
    insert ~slot:i (Char.of_int_exn (Char.to_int 'a' + i))
  done;
  (* Because we delete from > length, the length doesn't change *)
  remove ~slot:8;
  show ();
  [%expect {| [b________a____dc] [abcd____________] abcd [4] |}];
  remove ~slot:15;
  show ();
  [%expect {| [b________a____dc] [abcd____________] abcd [4] |}];
  (* first empty slot *)
  remove ~slot:4;
  show ();
  (* actual data removal *)
  [%expect {| [b________a____dc] [abcd____________] abcd [4] |}];
  remove ~slot:3;
  show ();
  [%expect {| [b________a_____c] [abc_____________] abc [3] |}]
;;

let%expect_test "insert into empty slots" =
  let insert, _remove, show = create_sim 4 in
  insert ~slot:5 'x';
  insert ~slot:6 'y';
  insert ~slot:9 'z';
  show ();
  [%expect {| [_____________zyx] [_____xy__z______] _____xy__z [10] |}];
  insert ~slot:0 'c';
  insert ~slot:0 'b';
  insert ~slot:0 'a';
  show ();
  [%expect {| [__________abczyx] [abc_____xy__z___] abc_____xy__z [13] |}]
;;

let tagging_sim ~log_vec_size =
  let module Tag = struct
    type 'a t = { value : 'a [@bits 4] } [@@deriving hardcaml]
  end
  in
  let module IVec =
    Index_vec.Make_tagged (struct
      module Tag = Tag

      let vec_size = 1 lsl log_vec_size
      let spec ~index:_ = Tag.Of_signal.zero ()
    end)
  in
  let clock = Signal.input "clock" 1 in
  let clear = Signal.input "clear" 1 in
  (* The tag is set to A on insertion, D on deletion. The tags are set to [0,1,2,3] on
     [set_tags]. *)
  let op =
    { IVec.slot = Signal.input "slot" log_vec_size
    ; op = Signal.input "op" (Signal.width (Vec.noop (module Signal)))
    ; insertion_tag = Some { value = Signal.of_int_trunc ~width:4 0xA }
    ; deletion_tag = Some { value = Signal.of_int_trunc ~width:4 0xD }
    }
  in
  let set_tags = Signal.input "set_tags" 1 in
  let set_indexes = Signal.input "set_indexes" 1 in
  let vec =
    IVec.create
      ~tag_next:(fun ~index d ->
        { Tag.value = Signal.mux2 set_tags (Signal.of_int_trunc ~width:4 index) d.value })
      ~index_next:(fun ~index d ->
        Signal.mux2
          set_indexes
          (Signal.of_int_trunc ~width:log_vec_size ((1 lsl log_vec_size) - index - 1))
          d)
      (Signal.Reg_spec.create ~clock ~clear ())
      op
  in
  let indexes = IVec.indexes vec in
  let tags = IVec.tags vec |> Array.map ~f:(fun d -> d.value) in
  let circuit =
    Circuit.create_exn
      ~name:"index_vec"
      (List.concat
         [ Array.to_list indexes
           |> List.mapi ~f:(fun index -> Signal.output ("idx" ^ Int.to_string index))
         ; Array.to_list tags
           |> List.mapi ~f:(fun index -> Signal.output ("tag" ^ Int.to_string index))
         ; [ Signal.output "length" (IVec.length vec)
           ; Signal.output "full" (IVec.full vec)
           ; Signal.output "empty" (IVec.empty vec)
           ; Signal.output "insertion_index" (IVec.insertion_index vec)
           ; Signal.output "deletion_index" (IVec.deletion_index vec)
           ]
         ])
  in
  Cyclesim.create ~config:{ Cyclesim.Config.trace_all with store_circuit = true } circuit
;;

let test_tagging ~log_vec_size sim =
  let clear = Cyclesim.in_port sim "clear" in
  let slot = Cyclesim.in_port sim "slot" in
  let op = Cyclesim.in_port sim "op" in
  let set_tags = Cyclesim.in_port sim "set_tags" in
  let set_indexes = Cyclesim.in_port sim "set_indexes" in
  (* Circuit must be reset to initialize the vec index registers *)
  let (waves, sim), slot, op =
    ( Waveform.create sim
    , (fun i -> slot := Bits.of_int_trunc ~width:log_vec_size i)
    , fun (f : (module Comb.S with type t = 'a) -> 'a) -> op := f (module Bits) )
  in
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  (* move indexes and tags around by inserting and deleting *)
  slot 0;
  op Vec.insert;
  Cyclesim.cycle sim;
  slot 0;
  op Vec.insert;
  Cyclesim.cycle sim;
  slot 2;
  op Vec.insert;
  Cyclesim.cycle sim;
  slot 1;
  op Vec.remove;
  Cyclesim.cycle sim;
  op Vec.noop;
  Cyclesim.cycle sim;
  (* set the tags back to a known pattern *)
  set_tags := Bits.vdd;
  Cyclesim.cycle sim;
  set_tags := Bits.gnd;
  Cyclesim.cycle sim;
  (* set the index back to a known pattern *)
  set_indexes := Bits.vdd;
  Cyclesim.cycle sim;
  set_indexes := Bits.gnd;
  Cyclesim.cycle sim;
  waves
;;

let%expect_test "test tagging - show that the tags move (and change) with insertions and \
                 deletions. Show both indexes and tags can be arbitrarily set by the \
                 [tag_next] and [index_next] functions"
  =
  let log_vec_size = 2 in
  let waves = tagging_sim ~log_vec_size |> test_tagging ~log_vec_size in
  Waveform.print ~display_width:86 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                         │
    │                  ││      └─────────────────────────────────────────────────────    │
    │                  ││──────┬─────────────────┬─────┬─────────────────────────────    │
    │op                ││ 0    │2                │3    │0                                │
    │                  ││──────┴─────────────────┴─────┴─────────────────────────────    │
    │set_indexes       ││                                                ┌─────┐         │
    │                  ││────────────────────────────────────────────────┘     └─────    │
    │set_tags          ││                                    ┌─────┐                     │
    │                  ││────────────────────────────────────┘     └─────────────────    │
    │                  ││──────────────────┬─────┬───────────────────────────────────    │
    │slot              ││ 0                │2    │1                                      │
    │                  ││──────────────────┴─────┴───────────────────────────────────    │
    │                  ││────────────┬─────┬─────┬─────┬───────────────────────┬─────    │
    │deletion_index    ││ 0          │3    │0    │3    │1                      │2        │
    │                  ││────────────┴─────┴─────┴─────┴───────────────────────┴─────    │
    │empty             ││────────────┐                                                   │
    │                  ││            └───────────────────────────────────────────────    │
    │full              ││                                                                │
    │                  ││────────────────────────────────────────────────────────────    │
    │                  ││────────────┬─────┬───────────────────────────────────┬─────    │
    │idx0              ││ 0          │3    │2                                  │3        │
    │                  ││────────────┴─────┴───────────────────────────────────┴─────    │
    │                  ││──────┬─────┬─────┬───────────┬───────────────────────┬─────    │
    │idx1              ││ 0    │1    │0    │3          │1                      │2        │
    │                  ││──────┴─────┴─────┴───────────┴───────────────────────┴─────    │
    │                  ││──────┬─────┬─────┬─────┬─────┬───────────────────────┬─────    │
    │idx2              ││ 0    │2    │1    │0    │1    │0                      │1        │
    │                  ││──────┴─────┴─────┴─────┴─────┴───────────────────────┴─────    │
    │                  ││──────┬─────┬─────┬─────┬─────┬───────────────────────┬─────    │
    │idx3              ││ 0    │3    │2    │1    │0    │3                      │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴───────────────────────┴─────    │
    │                  ││──────┬─────┬─────┬─────┬─────┬───────────────────────┬─────    │
    │insertion_index   ││ 0    │3    │2    │1    │0    │3                      │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴───────────────────────┴─────    │
    │                  ││────────────┬─────┬─────┬─────┬─────────────────────────────    │
    │length            ││ 0          │1    │2    │3    │2                                │
    │                  ││────────────┴─────┴─────┴─────┴─────────────────────────────    │
    │                  ││────────────┬─────────────────────────────┬─────────────────    │
    │tag0              ││ 0          │A                            │0                    │
    │                  ││────────────┴─────────────────────────────┴─────────────────    │
    │                  ││──────────────────┬───────────────────────┬─────────────────    │
    │tag1              ││ 0                │A                      │1                    │
    │                  ││──────────────────┴───────────────────────┴─────────────────    │
    │                  ││────────────────────────┬─────┬───────────┬─────────────────    │
    │tag2              ││ 0                      │A    │0          │2                    │
    │                  ││────────────────────────┴─────┴───────────┴─────────────────    │
    │                  ││──────────────────────────────┬───────────┬─────────────────    │
    │tag3              ││ 0                            │D          │3                    │
    │                  ││──────────────────────────────┴───────────┴─────────────────    │
    │gnd               ││                                                                │
    │                  ││────────────────────────────────────────────────────────────    │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;
