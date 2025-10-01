open! Core
open! Hardcaml

type event =
  | Push of int
  | Pop
  | Push_pop of int
[@@deriving sexp_of]

module Make_test (C : sig
    val capacity : int
  end) : sig
  module Stack_config : Hardcaml_circuits.Stack.Config
  module Stack : module type of Hardcaml_circuits.Stack.Make (Stack_config)
  module Sim : module type of Cyclesim.With_interface (Stack.I) (Stack.O)

  val create : read_latency:int -> unit -> Hardcaml_waveterm.Waveform.t * Sim.t
  val run : ?verbose:bool -> read_latency:int -> Sim.t -> event list -> unit
end

val basic_events : event list
val fill_push_and_pop_all_events : event list
val read_latency_2_events : event list
val read_latency_3_events : event list
val capacity_5_events : event list
