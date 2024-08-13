open Core
open Hardcaml_circuits.Divider

let top_width = 64
let top_signed = true

module Params = struct
  type t = unit

  let name _ = Printf.sprintf "%d_bits__signed_%b" top_width top_signed
  let flags = Command.Param.return ()
end

let name = "divider"

module Div = Hardcaml_circuits.Divider.Make (struct
    let width = 32
    let signedness = Hardcaml.Signedness.Signed
    let architecture = Architecture.Pipelined
  end)

include Div
module Input = I
module Output = O

let input_port_names_and_width _ = I.port_names_and_widths
let output_port_names_and_width _ = O.port_names_and_widths
let params = []
let create ~params:_ ~scope ~clock (i : _ I.t) = create scope { i with clock }
