open! Core
open Hardcaml

module Sim (Config : Hardcaml_circuits.Counter_div_mod.Config) : sig
  module Counter_div_mod : module type of Hardcaml_circuits.Counter_div_mod.Make (Config)

  module Sim :
      module type of Cyclesim.With_interface (Counter_div_mod.I) (Counter_div_mod.O)

  val create_sim : unit -> Sim.t

  val run_test
    :  set_input_list:(int * int) option list
    -> input_list:bool list
    -> Sim.t
    -> Hardcaml_waveterm.Waveform.t * int Counter_div_mod.O.t list

  val random_test : Sim.t -> unit
end
