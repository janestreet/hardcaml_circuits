open! Core
open Hardcaml

val init
  :  ?trace_reductions:bool
  -> cycles:int
  -> int
  -> Hardcaml_waveterm.Waveform.t * Cyclesim.t_port_list

val run : Cyclesim.t_port_list -> cycles:int -> int -> unit
val random : Cyclesim.t_port_list -> cycles:int -> int -> unit
