open Core
module P = Hardcaml_circuits.Pipelined_adder

module Params = struct
  type architecture =
    | Classic
    | Cac

  type t =
    { term_width : int
    ; part_width : int
    ; architecture : architecture
    }

  let architecture_to_string = function
    | Classic -> "Classic"
    | Cac -> "Cac"
  ;;

  let name t =
    sprintf
      "%d_bits__arch_%s__part_width_%d_bits"
      t.term_width
      (architecture_to_string t.architecture)
      t.part_width
  ;;

  let architecture_arg_type =
    Command.Arg_type.create (function
      | "cac" | "Cac" -> Cac
      | "classic" | "Classic" -> Classic
      | architecture ->
        raise_s [%message "unrecognized architecture string" (architecture : string)])
  ;;

  let flags =
    [%map_open.Command
      let term_width =
        flag "term-width" (required int) ~doc:"<int> Term width in addition"
      and part_width =
        flag "part-width" (required int) ~doc:"<int> Part width in addition"
      and architecture =
        flag
          "architecture"
          (required architecture_arg_type)
          ~doc:"<classic|cac> pipelined adder architecture"
      in
      { term_width; part_width; architecture }]
  ;;
end

module Input = struct
  type 'a t =
    { lhs : 'a
    ; rhs : 'a
    }
  [@@deriving hardcaml]
end

module Output = struct
  type 'a t = { result : 'a } [@@deriving hardcaml]
end

let input_port_names_and_width (params : Params.t) =
  { Input.lhs = "lhs", params.term_width; rhs = "rhs", params.term_width }
;;

let output_port_names_and_width (params : Params.t) =
  { Output.result = "result", params.term_width }
;;

let name = "pipelined_adder"

let params =
  let open List.Let_syntax in
  let%bind term_width = [ 32; 64 ] in
  let%bind part_width = [ term_width; term_width / 2 ] in
  let%bind architecture = Params.[ Classic; Cac ] in
  [ Params.{ term_width; part_width; architecture } ]
;;

let create ~(params : Params.t) ~scope:_ ~clock (input : _ Input.t) =
  let part_width = params.part_width in
  { Output.result = P.Short_latency.create ~part_width ~clock input.lhs input.rhs }
;;
