open Base
open Hardcaml
open Signal

module Params = struct
  type t = int

  let name t = Printf.sprintf "%d_bits" t

  let flags =
    let open Core in
    [%map_open.Command
      let bit_width =
        flag
          "bit-width"
          (required int)
          ~doc:"<int> bit width of adder argument and result"
      in
      bit_width]
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

let input_port_names_and_width width =
  Input.{ lhs = port_names.lhs, width; rhs = port_names.rhs, width }
;;

let name = "adder"
let output_port_names_and_width width = Output.{ result = port_names.result, width }
let params = [ 16; 32; 64 ]

let create ~params:_ ~scope:_ ~clock:_ (input : _ Input.t) =
  { Output.result = input.lhs +: input.rhs }
;;
