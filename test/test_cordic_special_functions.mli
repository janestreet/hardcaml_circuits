open! Core
open! Hardcaml
open Hardcaml_circuits

module Test (Function : Cordic_special_functions.Function) : sig
  val create_sims : unit -> Function.Sim.t * Function.Sim.t

  val run
    :  Function.Sim.t
    -> Function.Sim.t
    -> float Function.Args.t
    -> float Function.Results.t
end
