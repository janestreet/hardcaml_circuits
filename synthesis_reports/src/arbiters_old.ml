open! Core
module Synth = Hardcaml_xilinx_reports

module Make (Bits : sig
    val data_width : int
    val num_sources : int
  end) =
struct
  let log2_num_sources = Int.ceil_log2 Bits.num_sources

  module With_valid = struct
    type 'a t =
      { valid : 'a
      ; value : 'a [@bits Bits.data_width]
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { index : 'a [@bits log2_num_sources]
      ; data : 'a With_valid.t list [@length Bits.num_sources]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = With_valid
  module With_regs = Hardcaml_xilinx_reports.Wrap_with_registers.Make (I) (O)
  module Arb = Hardcaml_circuits.Arbiters

  let wrap_comb f =
    let f _scope (i : _ I.t) =
      let data =
        List.map i.data ~f:(fun { valid; value } -> { Hardcaml.With_valid.valid; value })
      in
      let { Hardcaml.With_valid.valid; value } =
        f ~index:(Arb.Index.Offset i.index) ~data
      in
      { O.valid; value }
    in
    `Combinational f
  ;;

  let wrap_seq f =
    let f _scope (i : _ With_regs.I_with_clock.t) =
      let data =
        List.map i.i.data ~f:(fun { valid; value } ->
          { Hardcaml.With_valid.valid; value })
      in
      let { Hardcaml.With_valid.valid; value } =
        f ~clock:i.clock ~clear:i.clear ~index:(Arb.Index.Offset i.i.index) ~data
      in
      { O.valid; value }
    in
    `Sequential f
  ;;

  let create =
    With_regs.create_list
      [ ( "log_comb"
        , wrap_comb
            (Arb.Round_robin_with_priority.Log_shift.combinational
               (module Hardcaml.Signal)) )
      ; "log_seq", wrap_seq Arb.Round_robin_with_priority.Log_shift.sequential
      ; ( "clz_comb"
        , wrap_comb
            (Arb.Round_robin_with_priority.Count_zeros.combinational
               (module Hardcaml.Signal)) )
      ; "clz_seq", wrap_seq Arb.Round_robin_with_priority.Count_zeros.sequential
      ; ( "hot1_comb"
        , wrap_comb
            (Arb.Round_robin_with_priority.Onehot_cleaner.combinational
               (module Hardcaml.Signal)) )
      ; "hot1_seq", wrap_seq Arb.Round_robin_with_priority.Onehot_cleaner.sequential
      ]
  ;;
end

let command =
  Async.Command.async
    ~summary:"count leading zeros logic"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and flags = Hardcaml_xilinx_reports.Command.Command_flags.flags ()
     and data_width = flag "width" (required int) ~doc:"DATA_WIDTH Width of data"
     and num_sources =
       flag "num-sources" (required int) ~doc:"NUM sources to arbitrate"
     in
     fun () ->
       let module Arb =
         Make (struct
           let data_width = data_width
           let num_sources = num_sources
         end)
       in
       let module Synth =
         Synth.Command.With_interface (Arb.With_regs.I_with_clock) (Arb.O)
       in
       Synth.run ~name:"arbiters" ~flags Arb.create)
;;
