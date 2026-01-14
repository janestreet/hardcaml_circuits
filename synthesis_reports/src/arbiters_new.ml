open Core
open Hardcaml

module Params = struct
  module Architecture = struct
    type t =
      | Log_comb
      | Log_seq
      | Clz_comb
      | Clz_seq
      | Hot1_comb
      | Hot1_seq
    [@@deriving variants, enumerate, sexp]

    let of_string s = t_of_sexp (Sexp.Atom s)
    let arg_type = Command.Arg_type.create of_string
  end

  type t =
    { data_width : int
    ; num_sources : int
    ; architecture : Architecture.t
    }

  let name { data_width; num_sources; architecture } =
    sprintf
      "data_width_%d__num_sources_%d__%s"
      data_width
      num_sources
      (Architecture.Variants.to_name architecture)
  ;;

  let flags =
    [%map_open.Command
      let data_width = flag "data-width" (required int) ~doc:"<int> arbiter input width"
      and num_sources =
        flag "num-sources" (required int) ~doc:"<int> arbiter number of input sources"
      and architecture =
        flag
          "architecture"
          (required Architecture.arg_type)
          ~doc:"<arch> arbiter architecture"
      in
      { data_width; num_sources; architecture }]
  ;;
end

module With_valid = struct
  module Pre = struct
    include With_valid

    let port_names_and_widths = { valid = "valid", 1; value = "value", 32 }
  end

  include Pre
  include Hardcaml.Interface.Make (Pre)
end

module Input = struct
  type 'a t =
    { clear : 'a
    ; data : 'a With_valid.t list [@length 1]
    ; index : 'a
    }
  [@@deriving hardcaml ~rtlmangle:false]
end

module Output = With_valid

let input_port_names_and_width (params : Params.t) =
  let open Input in
  let index_width = Int.ceil_log2 params.num_sources in
  { clear = port_names_and_widths.clear
  ; data =
      List.init params.num_sources ~f:(fun i ->
        { With_valid.valid = sprintf "data_in_%d_valid" i, 1
        ; value = sprintf "data_in_%d_value" i, params.data_width
        })
  ; index = port_names.index, index_width
  }
;;

let output_port_names_and_width (params : Params.t) =
  let open Output in
  { valid = "data_out_valid", 1; value = "data_out_value", params.data_width }
;;

module Arb = Hardcaml_circuits.Arbiters

(*=
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
*)

let name = "arbiter"

let params =
  let open List.Let_syntax in
  let%bind data_width = [ 32 ] in
  let%bind num_sources = [ 32; 16 ] in
  let%bind architecture = Params.Architecture.all in
  [ { Params.data_width; num_sources; architecture } ]
;;

let create ~params ~scope:_ ~clock (input : _ Input.t) =
  let fn =
    match params.Params.architecture with
    | Log_comb ->
      fun ~clock:_ ~clear:_ -> Arb.Round_robin_with_priority.combinational (module Signal)
    | Clz_comb ->
      fun ~clock:_ ~clear:_ ->
        Arb.Round_robin_with_priority.Count_zeros.combinational (module Signal)
    | Hot1_comb ->
      fun ~clock:_ ~clear:_ ->
        Arb.Round_robin_with_priority.Onehot_cleaner.combinational (module Signal)
    | Log_seq -> Arb.Round_robin_with_priority.Log_shift.sequential
    | Clz_seq -> Arb.Round_robin_with_priority.Count_zeros.sequential
    | Hot1_seq -> Arb.Round_robin_with_priority.Onehot_cleaner.sequential
  in
  fn ~clock ~clear:input.clear ~data:input.data ~index:(Arb.Index.Offset input.index)
;;
