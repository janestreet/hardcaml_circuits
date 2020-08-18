open Core
open Hardcaml

module type Params = sig
  type t

  val flags : t Command.Param.t
  val name : t -> string
end

module type S = sig
  module Params : Params
  module Input : Hardcaml.Interface.Pre_partial
  module Output : Hardcaml.Interface.Pre_partial

  val name : string
  val input_port_names_and_width : Params.t -> (string * int) Input.t
  val output_port_names_and_width : Params.t -> (string * int) Output.t
  val params : Params.t list

  val create
    :  params:Params.t
    -> scope:Scope.t
    -> clock:Signal.t
    -> Signal.t Input.t
    -> Signal.t Output.t
end
